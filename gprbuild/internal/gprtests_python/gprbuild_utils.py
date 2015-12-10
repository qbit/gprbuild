#!/usr/bin/env gnatpython
"""run_test.py

Run a gprbuild test
"""
from gnatpython import testsuite_logging
from gnatpython.env import Env
from gnatpython.ex import Run
from gnatpython.fileutils import ln
from gnatpython import logging
from gnatpython.logging_util import add_handlers, RAW
from gnatpython.internal.prj import Project

try:
    from gnatpython.internal.excross import run_cross
except ImportError:
    # Will be an error when running cross tests, but simplifies the setup
    # on user's machines since they don't need to install gnatpython-internal
    def run_cross(*args, **kwargs):
        raise ImportError("Could not import gnatpython.internal.excross")

from gnatpython import fileutils

# Export some functions to be used in testcases.
# Ignore pyflakes error on these ones.
from gnatpython.fileutils import (cd, cp, diff, mkdir, rm, touch, mv, which,
                                  find)

from optparse import OptionParser

import glob
import time
import os
import re
import sys
import fileinput

os = os                         # to export os in all test.py
env = Env()

worker_id = os.environ.get("WORKER_ID", "1")
env.restore(os.environ["TEST_CONFIG"])
os.environ["WORKER_ID"] = worker_id

GPRBUILD = env.config['gprbuild']
GPRINSTALL = env.config['gprinstall']
GPRCLEAN = env.config['gprclean']
GPRNAME = env.config['gprname']
GPRLS = env.config['gprls']
CONFIG_OUT = env.config['config_out']

optparser = OptionParser()
args = optparser.parse_args()[1]

target = env.target.triplet

logger = logging.getLogger('gprbuild_utils')

if env.main_options.verbose:
    logging.getLogger('').setLevel(RAW)
    default_format = '%(levelname)-8s %(message)s'
    add_handlers(RAW, default_format, "gprbuild.log")


def print_and_log(out):
    """Print and log the output of a command"""
    logger.debug("Output: %s" % out)
    print "<<<\n%s\n>>>" % out

# The globals described below follow the following convention:
# TEST_DIR = $WORK_DIR/tmp-test-$TEST_NAME-$PID
ROOT_DIR = os.getcwd()
TEST_DIR = os.path.dirname(sys.modules['__main__'].__file__)
TEST_NAME = os.path.basename(os.path.dirname(TEST_DIR))
TEST_NAME = TEST_NAME.lstrip("tmp-test-")
TEST_NAME = TEST_NAME[:TEST_NAME.rfind('-')]
WORK_DIR = os.path.dirname(TEST_DIR)

# Move to test directory
cd(TEST_DIR)

if os.path.exists(os.path.join(ROOT_DIR, 'examples')):
    EXAMPLES_DIR = os.path.join(ROOT_DIR, 'examples')
else:
    EXAMPLES_DIR = os.path.join(
        os.path.dirname(GPRBUILD), os.pardir, 'share',
        'examples', 'gprbuild')

# ??? For the following, use gnatpython.env.target_exeext whenever
# convenient (issue with coordinating with public version of gnatpython?).
if env.RTS is not None and "rtp" in env.RTS:
    exeext = ".vxe"
else:
    exeext = env.target.os.exeext

if env.target.platform == 'x86-pikeos':
    # ??? On PikeOS, zfp_support is the "internal" version, which is
    # only used by the ACATS for now and is meant to be replaced by
    # testsuite_support. See LB22-012.
    os.environ["VARIANT"] = target
    for gprfilename in glob.glob('*.gpr') + glob.glob('*/*.gpr'):
        gprfile = open(gprfilename, 'r')
        old_content = gprfile.read()
        gprfile.close()
        if 'aggregate project' not in old_content:
            # Do not add with "zfp_support"; to aggregate project
            gprfile = open(gprfilename, 'w')
            gprfile.write('with "zfp_support";\n')
            gprfile.write(old_content)
            gprfile.close()


def gpr_RTS_switches(rts):
    """Return the command-line switches needed to select the given RTS.

    Most of the time, this is just "--RTS=...". But in some situations,
    we also need to add other switches, for instance to mimick other
    tools calling grpbuild.

    PARAMETERS
        rts: The name of the runtime we want to use.

    RETURNS
        A list of command-line switches (strings).
    """
    switches = ['--RTS=' + rts]
    # On VxWorks, GNATbench passes RTS to all languages;
    # Test gprbuild with the same configuration.
    if 'vxworks' in target:

        # On x86-vx6cert, the tests are done against a vx6 kernel,
        # as vxsim is not supported on vx6cert; so we have a vx6 C
        # compiler in our path, for which vx6cert runtimes mean
        # nothing. See M911-042.

        if env.target.platform != 'x86-vx6cert':
                switches.append('--RTS:C=' + rts)
                switches.append('--RTS:C++=' + rts)

    return switches


def gpr_trailing_switches():
    """Return the switches to place at the end of the gprbuild command.
    """
    switches = []
    if ((env.target.platform in ('x86-vx7', 'x86_64-vx7')
         and 'vxsim' in env.target.machine)):
        # The compilers assume that the program is being built against
        # a VSB targetting real hardware, which can cause an occasional
        # problem when the VSB targets vxsim. For Ada, it's already
        # too late, but for C & C++, adjust the compilation to target
        # vxsim.
        cpu_names = {'linux': '_VX_SIMLINUX',
                     'windows': '_VX_SIMNT'}
        cpu_switches = ['-UCPU', '-DCPU=%s' % cpu_names[env.host.os.name]]
        switches.append('-cargs:C')
        switches.extend(cpu_switches)
        switches.append('-cargs:C++')
        switches.extend(cpu_switches)

        # WRS RTP base is not the same for vxsim and for a real board;
        # so set it explicitely. See O924-018.
        switches.append('-largs')
        if env.target.cpu.name == 'x86_64':
            switches.append('-Wl,--defsym,__wrs_rtp_base=0x200000000')
        else:
            switches.append('-Wl,--defsym,__wrs_rtp_base=0x68000000')
    return switches


def gprbuild(options=None, output=None, verbose=False, notarget=False):
    """Run gprbuild with the given options"""
    def filter_compiler(comp_out):
        for alt_comp in ('ccpentium', 'ccppc', 'ccsparc', 'ccarm'):
            comp_out = comp_out.replace(alt_comp, 'gcc')
        return comp_out

    if options is None:
        options = []
    elif isinstance(options, str):
        options = options.split(" ")

    install_target_specific_hooks_from_options(options)

    project = Project("prj", {"": {"main": "test.adb"}})
    config_dict = {'target': target,
                   'platform': env.target.platform}
    yaml_path = os.path.dirname(os.environ["TEST_CONFIG"])
    yaml_path = yaml_path + '/gprbuild.yaml'
    project.load_options_from_yaml(yaml_path, config_dict)

    if verbose:
        quiet_flag = ""
    else:
        quiet_flag = "-q"

    if notarget:
        target_flag = ""
    else:
        target_flag = '--target=' + target

    if CONFIG_OUT:
        gprbuild_cmd = [GPRBUILD, quiet_flag, '-p', target_flag,
                        '--config=' + CONFIG_OUT]
    else:
        gprbuild_cmd = [GPRBUILD, quiet_flag, '-p', target_flag]

    if env.RTS:
        gprbuild_cmd.extend(gpr_RTS_switches(env.RTS))

    gprbuild_cmd = (gprbuild_cmd + options + project.to_gnatmake_options()
                    + gpr_trailing_switches())

    if output is None:
        p = Run(gprbuild_cmd)
        if p.out:
            print filter_compiler(p.out.rstrip())
    else:
        Run(gprbuild_cmd, output=output + ".tmp")
        tmp_output = open(output + ".tmp")
        final_output = open(output, "w")
        final_output.write(filter_compiler(tmp_output.read()))
        final_output.close()
        tmp_output.close()


def gprinstall(options=None, output=None, verbose=False, notarget=False):
    """Run gprinstall with the given options"""
    if options is None:
        options = []
    elif isinstance(options, str):
        options = options.split(" ")

    install_target_specific_hooks_from_options(options)

    if verbose:
        quiet_flag = ""
    else:
        quiet_flag = "-q"

    if notarget:
        target_flag = ""
    else:
        target_flag = '--target=' + target

    gprinstall_cmd = [GPRINSTALL, quiet_flag, '-p', target_flag] + options

    if env.RTS:
        gprinstall_cmd.extend(['--RTS=' + env.RTS])

    if output is None:
        Run(gprinstall_cmd)
    else:
        Run(gprinstall_cmd, output=output + ".tmp")
        tmp_output = open(output + ".tmp")
        final_output = open(output, "w")
        final_output.write(tmp_output.read())
        final_output.close()
        tmp_output.close()


def count(sub, filename):
    """Prints the number of non-overlapping occurrences of sub in filename"""
    content_f = open(filename)
    content = content_f.read()
    n = content.count(sub)
    content_f.close()
    print "%d" % n


def add_dll_dir(directory):
    """Add a shared libraries directory"""
    env.add_dll_path(directory)


def run(binary, output=None):
    """Run the executable binary and redirect stdout to output if not None

    If target_machine is SKIP, do not run the tests
    """

    def saved_bin_name(filename, suffix):
        basename, ext = os.path.splitext(filename)
        if suffix == 0:
            return filename
        else:
            if ext:
                return "%s-%d.%s" % (basename, suffix, ext)
            else:
                return "%s-%d" % (basename, suffix)

    if os.environ.get("GPRBUILD_TEST_SKIP", ''):
        return

    # If the executable is not found, output an error message; this
    # message should be target-independent as a few testcase use this
    # output in their baseline. This explains why the executable
    # extension is not printed.

    if not (os.path.exists(binary) or os.path.exists(binary + exeext)):
        print_and_log("run: executable %s not found" % binary)
        return

    # copy all binaries in ../bin to allow debuging a problem
    if os.path.exists('../src'):
        for bin_name in set((binary, binary + exeext)):
            if os.path.exists(bin_name):
                mkdir('../bin/' + os.path.dirname(bin_name))
                suffix = 0
                # run call be called more than once on the same binary
                while os.path.exists(os.path.join(
                        os.pardir, 'bin', saved_bin_name(bin_name, suffix))):
                    suffix += 1
                ln(bin_name, os.path.join(os.pardir, 'bin',
                                          saved_bin_name(bin_name, suffix)))

    if env.target.is_host:
        cmd = ['./%s' % binary]
    else:
        p = run_cross([binary + exeext], timeout=600)
        if p.out.strip():
            print_and_log(p.out)
        return

    if output is not None:
        Run(cmd, output=output)
    else:
        try:
            p = Run(cmd)
            p_out = p.out.rstrip()
        except OSError, msg:
            p_out = msg

        if p_out:
            print_and_log(p_out)


def ls(pattern, output=None):
    """Show all files matching the given pattern"""
    files = fileutils.ls(pattern)

    # Remove references to "auto.cgpr", which is only created when the tests
    # are run in special modes, not always
    files = (f for f in files if not re.search("auto.cgpr", f))

    # Remove references to "so_locations" not deleted on alpha-tru64 and
    # mips-irix (See GB22-006) ???
    files = (f for f in files if not f.endswith("so_locations"))

    # Remove files generated by gnatpython.ex_cross.run
    if (('qemu' in env.target.machine
         or 'simics' in env.target.machine
         or 'vxsim' in env.target.machine
         or 'tsim' in env.target.machine)):
        files = ((f for f in files
                  if not f.endswith('out.script')
                  and not f.endswith('vxe.script')
                  and not f.endswith('.vx6_script')
                  and not f.endswith('dummy%s.vxe' % worker_id)
                  and not f.endswith('dummy.vxe')
                  and not f.endswith('.qcow2')
                  and not f.endswith('mils-kernel-%s' % worker_id)
                  and not f.endswith('local_build_%s' % worker_id)
                  and not f.endswith('local_kernel_%s' % worker_id)
                  and not f.endswith('persistent_dir-%s' % worker_id)
                  ))

    # Ignore p20101.flash files
    if env.target.machine == 'qemu-p2010':
        files = (f for f in files if not ('p2010'
                 in f and f.endswith('flash')))

    if env.target.machine.startswith(
            'vxsim') or env.target.machine == 'qemu-p2010':
        files = (f for f in files if 'rlimit-vxworks' not in f)

    if output:
        with open(output, 'w') as f:
            for k in files:
                f.write("%s\n" % k)
    else:
        for k in files:
            print k


def gprclean(options=None, verbose=False, output=None, notarget=False):
    """Run gprclean with given options"""
    if options is None:
        options = []
    elif isinstance(options, str):
        options = options.split(" ")

    if verbose:
        quiet_flag = ""
    else:
        quiet_flag = "-q"

    if notarget:
        target_flag = ""
    else:
        target_flag = '--target=' + target

    if CONFIG_OUT:
        cmd = [GPRCLEAN, quiet_flag, target_flag,
               '--config=' + CONFIG_OUT] + options
    else:
        cmd = [GPRCLEAN, quiet_flag, target_flag] + options

    if env.RTS:
        cmd.extend(gpr_RTS_switches(env.RTS))

    if output is None:
        p = Run(cmd)
        if p.out:
            print p.out.rstrip()
    else:
        p = Run(cmd, output=output)


def gprname(options=None, verbose=False, output=None, notarget=False):
    """Run gprname with given options"""
    if options is None:
        options = []
    elif isinstance(options, str):
        options = options.split(" ")

    quiet_flag = ""

    if notarget:
        target_flag = ""
    else:
        target_flag = '--target=' + target

    cmd = [GPRNAME, quiet_flag, target_flag] + options

    if output is None:
        p = Run(cmd)
        if p.out:
            print p.out.rstrip()
    else:
        p = Run(cmd, output=output)


def gprls(options=None, verbose=False, output=None):
    """Run gprls with given options"""
    if options is None:
        options = []
    elif isinstance(options, str):
        options = options.split(" ")

    quiet_flag = ""

    target_flag = ""

    cmd = [GPRLS, quiet_flag, target_flag] + options

    if output is None:
        p = Run(cmd)
        if p.out:
            print p.out.rstrip()
    else:
        p = Run(cmd, output=output)


def sleep(duration):
    """Sleep for duration (in seconds)"""
    time.sleep(duration)


def insert_shutdown_ada(filename, shutdown_funcname, arg_values, arg_types):
    """Insert Ada shutdown sequence

    PARAMETERS
      filename:          main unit to patch
      shutdown_funcname: symbol name of the shutdown function
      arg_values:        parameters in shutdown call
      arg_types:         Ada types for these parameters
    """
    def print_shutdown_sequence():
        print '   declare'
        if arg_types is not None:
            arg_decls = []
            for arg_t in arg_types:
                arg_decls.append("A%d : %s" % (len(arg_decls), arg_t))
            print '      procedure OS_Exit (%s);' % ", ".join(arg_decls)
        else:
            print '      procedure OS_Exit;' % ", ".join(arg_decls)
        print ('      pragma Import (C, OS_Exit, "%s");'
               % shutdown_funcname)
        print '   begin'
        if arg_values is not None:
            print '      OS_Exit (%s);' % ", ".join(arg_values)
        else:
            print '      OS_exit;'
        print '   end;'

    for line in fileinput.input(filename, inplace=1):
        if ((re.match("^end.*", line, re.I | re.S)
             or re.match("^exception.*", line, re.I | re.S))):
            print_shutdown_sequence()
            print line,
        else:
            print line,


def insert_shutdown_c(filename, shutdown_funcname, arg_values):
    """Insert C shutdown sequence

    PARAMETERS
      filename:          main unit to patch
      shutdown_funcname: symbol name of the shutdown function
      arg_values:        parameters in shutdown call
    """
    for line in fileinput.input(filename, inplace=1):
        if re.match("^}", line, re.S):
            print "%s (%s);" % (shutdown_funcname, ", ".join(arg_values))
            print line,
        elif re.match("^\s+return.*", line, re.S):
            print "{"
            print "%s (%s);" % (shutdown_funcname, ", ".join(arg_values))
            print line,
            print "}"
        else:
            print line,


def insert_shutdown(mains, shutdown_funcname, arg_values, arg_types):
    """Insert shutdown sequence

    PARAMETERS
      mains:             list of main units to patch
      shutdown_funcname: symbol name of the shutdown function
      arg_values:        parameters in shutdown call
      arg_types:         Ada types for these parameters
    """
    ada_body = re.compile("^\S+\.ad[ab]")
    for f in mains:
        if re.match(ada_body, f) is not None:
            insert_shutdown_ada(f, shutdown_funcname, arg_values, arg_types)
        else:
            insert_shutdown_c(f, shutdown_funcname, arg_values)


def insert_shutdown_pikeos(mains):
    """Insert PikeOS' shutdown sequence

    PARAMETERS
      mains: list of main units to patch
    """
    insert_shutdown(mains, "vm_target_reset", ["0", ], ["Integer", ])


def get_mains(input):
    """Apply iteratively find_mains on input and join the results
    """
    result = []
    for e in input:
        result += find_mains(e)
    return result


def find_mains(filename):
    """Find main units

    This takes the file in parameter as an indication of where to find a
    main unit; if it is a source file, check if it is a main unit; if it
    is a project file, check for a Main attribute. otherwise returns an
    empty list.

    PARAMETERS
      filename: candidate source file/project file
    """
    cc_file = re.compile("^((\S+.c$)|(\S+.cc$))")
    ada_file = re.compile("^\S+\.ad[ab]$")
    gpr_file = re.compile("^\S+\.gpr$")

    def find_all(filename):
        """Same as fileutils.find, except that filename can be a path to file
        """
        result = find(".", filename)
        if os.path.exists(filename):
            result.append(filename)
        return result

    def is_lib_unit(filename):
        """True if filename contains an Ada library unit
        """
        lib_unit = re.compile("^package\s+.*")
        with open(filename) as fd:
            lines = fd.readlines()
            for line in lines:
                if re.match(lib_unit, line):
                    return True
        return False

    if re.match(cc_file, filename) is not None:
        return find_all(filename)
    elif re.match(ada_file, filename) is not None:
        paths = find_all(filename)
        return [f for f in paths if not is_lib_unit(f)]
    elif (re.match(gpr_file, filename) is not None
          and os.path.exists(filename)):
        main_attr = re.compile('.*for\s+Main\s+use\s+\((.*)\).*', re.I | re.S)
        with open(filename) as fd:
            lines = fd.readlines()
            for line in lines:
                m = re.match(main_attr, line)
                if m is not None:
                    quoted_files = m.group(1).split(',')
                    result = []
                    for file in quoted_files:
                        result += find_all(file.strip()[1:-1])
                    return result
        return []
    else:
        return []


def install_target_specific_hooks():
    """Patch main files to insert target-specific calls

    These calls are typically a system shutdown at the end of the
    application on platforms where this is the only way to detect
    the end of the execution (e.g. PikeOS or 653/RTP).

    The heuristics looks for Main attribute of all gpr files; then this
    patches them with the needed target specific hooks.
    """
    if 'pikeos' in target:
        full_paths = find(".", "*.gpr")
        insert_shutdown_pikeos(get_mains(full_paths))


def install_target_specific_hooks_from_options(options):
    """Same as install_target_specific_hooks but for main units on command line

    PARAMETERS
      options: list of options sent to gprbuild
    """
    if 'pikeos' in target:
        insert_shutdown_pikeos(get_mains(options))

install_target_specific_hooks()
# The following line shoud be removed when a Main object is used to gather
# command lines options.
logging.getLogger('').setLevel(RAW)
testsuite_logging.setup_logging(TEST_NAME, WORK_DIR)
