#!/usr/bin/env gnatpython

"""./testsuite.py [options] [test name]

Run the GPRbuild testsuite
"""

from gnatpython import testsuite_logging
from gnatpython.env import Env
from gnatpython.ex import Run
from gnatpython.fileutils import which
from gnatpython.main import Main
from gnatpython.mainloop import (MainLoop, add_mainloop_options,
                                 generate_collect_result,
                                 generate_run_testcase, setup_result_dir)
from gnatpython.testdriver import add_run_test_options
from gnatpython.reports import ReportDiff

from glob import glob

import logging
import os
import re
import sys


def main(tests_dir, options, config_dict, discs=None):
    """Run the testsuite

    PARAMETERS
      tests_dir: top level directory name
      options: optparser options
      config_dict: configuration options that will be passed to the test driver
      discs: list of discriminants (autocomputed if empty)
    """
    env = Env()
    write_test_config(config_dict)

    test_list = [os.path.dirname(os.path.abspath(t))
                 for t in filter_list(
                     tests_dir + '/*/test.py', options.run_test)]
    test_list.sort()

    # Various files needed or created by the testsuite
    setup_result_dir(options)
    result_dir = options.output_dir
    results_file = result_dir + '/results'

    if discs is None:
        # Compute discs
        discs = [lang for lang in ('Ada', 'C', 'C++', 'ASM', 'Fortran') if lang
                 in options.run_config]
        if 'auto' in options.run_config.lower():
            # testsuite run with the auto configuration mode
            discs.append('AUTOCGPR')
        discs += get_gnat_discs(RTS=options.RTS)

        # Check if xmlada is supported
        xmlada_test = [config_dict['gprbuild'], '-c', '-q', '-p',
                       '--target=' + env.target.triplet,
                       '-Ptest_xmlada.gpr']
        if options.RTS:
            xmlada_test.append('--RTS=' + options.RTS)
        if Run(xmlada_test).status == 0:
            discs.append('xmlada')

    # Always add ALL and target info
    discs.append('ALL')
    discs += env.discriminants

    if options.RTS:
        discs.append(options.RTS)
        if 'zfp' in options.RTS and options.RTS != 'zfp':
            discs.append('zfp')
    else:
        discs.append('default_rts')

    if options.discs:
        discs += options.discs

    if env.target.machine is not None:
        if env.target.machine.startswith('vxsim'):
            discs.append('vxsim')
        elif env.target.machine == 'qemu':
            discs.append('qemu')

    with open(result_dir + "/discs", "w") as discs_f:
        discs_f.write(" ".join(discs))

    with open(result_dir + '/comment', 'w') as comment_f:
        comment_f.write("Options: %s\n" % " ".join(sys.argv))
        comment_f.write("PATH: %s\n" % os.environ["PATH"])

    metrics = {'total': len(test_list)}
    collect_result = generate_collect_result(
        result_dir, results_file, options.view_diffs,
        metrics=metrics, options=options)
    run_testcase = generate_run_testcase('run-test', discs, options)

    MainLoop(test_list, run_testcase, collect_result, options.mainloop_jobs)

    # Write report
    report = os.path.join(result_dir, "report")

    if os.path.isfile(os.path.join(result_dir, "results")):
        # The testsuite might have generated no results if no test was run.
        # This happens if a pattern was specified on the command line, and
        # this pattern matches no file.
        ReportDiff(result_dir, options.old_output_dir).txt_image(report)

    testsuite_logging.write_comment(result_dir)


def write_test_config(config_dict):
    """Create test_config.py configuration file"""
    # Write test_config.py
    env = Env()
    env.config = config_dict

    # Add current directory in PYTHONPATH (to find gprconfig_utils,
    # gprbuild_utils and generated test_config modules)
    env.add_search_path('PYTHONPATH', os.getcwd())
    os.environ["TEST_CONFIG"] = os.path.join(os.getcwd(), 'env.dump')
    os.environ["GPRCONFIG_SUPPORT"] = os.path.join(os.getcwd(), 'support')
    env.store(os.environ["TEST_CONFIG"])


def filter_list(pattern, run_test=""):
    """Compute the list of test matching pattern

    If run_test is not null, run only tests containing run_test
    """
    test_list = glob(pattern)
    if not run_test:
        return test_list
    else:
        for test in test_list:
            if run_test in test:
                return [test]
        # not found
        return []


def parse_options():
    """Parse command lines options"""
    m = Main(add_targets_options=True)
    add_mainloop_options(m, extended_options=True)
    add_run_test_options(m)
    m.add_option("--view-diffs", dest="view_diffs", action="store_true",
                 default=False, help="show diffs on stdout")
    m.add_option("--diffs", dest="view_diffs", action="store_true",
                 default=False, help="Alias for --view-diffs")
    m.add_option("--config", dest="run_config", type="string",
                 metavar="CONFIG", default="AdaC_Auto",
                 help="Run only the config CONFIG")
    m.add_option("--config-list", dest="show_config_list",
                 action="store_true", default=False,
                 help="show config list for this machine")
    m.add_option("--skip-run", dest="skip_run",
                 action="store_true", default=False,
                 help="Do not run the generated binaries. All successfully"
                 " compiled tests will be marked as PASSED")
    m.add_option('--RTS', help="RTS", default=None)
    m.parse_args()

    if m.options.skip_run:
        # All calls to run() will be ignored
        os.environ['GPRBUILD_TEST_SKIP'] = 'skip'

    if m.args and not m.options.show_config_list:
        m.options.run_test = os.path.sep + m.args[0] + os.path.sep
        # User want to run only one test
        logging.info("Running only test '%s'" % m.options.run_test)
    else:
        m.options.run_test = ""
    m.options.gprconfig_db = ""  # No configurable for now

    if m.options.discs:
        m.options.discs = m.options.discs.split(',')

    return m.options


def get_config_dict(options):
    env = Env()
    env.RTS = options.RTS or None
    config_dict = {'gprbuild': 'gprbuild' + env.host.os.exeext,
                   'gprinstall': 'gprinstall' + env.host.os.exeext,
                   'gprclean': 'gprclean' + env.host.os.exeext,
                   'gprconfig': 'gprconfig' + env.host.os.exeext,
                   'gprname': 'gprname' + env.host.os.exeext,
                   'gprls': 'gprls' + env.host.os.exeext,
                   'gnatmake': '',
                   'config_out': ''}

    for key in 'gprbuild', 'gprclean', 'gprconfig', 'gprinstall', 'gprname':
        # Search gprbuild utils in $PATH
        config_dict[key] = which(config_dict[key])
        assert config_dict[key], "%s not found" % key
    config_dict['gprls'] = which(config_dict['gprls'])
    config_dict['gnatmake'] = which('gnatmake' + env.host.os.exeext)
    return config_dict


def get_gnat_discs(path="", RTS=None):
    """Compute gnat discriminants"""
    from gnatpython.internal.gnat import GNAT  # Needed to compute discs
    discs = []
    env = Env()

    if path:
        # Put the tested compiler in front of the PATH to collect
        # its features (and revert this just after).
        env.store()
        env.add_path(path)

    gnat = GNAT(runtime=RTS)
    try:
        gnat.collect_features()
    except OSError:
        sys.exit("Cannot collect features of the compiler installed in " +
                 path)
    if path:
        env.restore()

    if env.is_cross:
        gnatls_bin = os.path.join(path, env.target.triplet + '-gnatls')
    else:
        gnatls_bin = os.path.join(path, 'gnatls')
    gnatls_cmd = [gnatls_bin, '-v']

    if RTS is not None:
        gnatls_cmd.append('--RTS=' + RTS)

    try:
        gnatls_out = Run(gnatls_cmd).out
    except (OSError), e:
        sys.exit("Error when running %s: %s" % (" ".join(gnatls_cmd), e))

    gnatls_v = re.match('.*GNATLS .* ([0-9\.a]+) .*',
                        gnatls_out, re.M + re.S)
    if gnatls_v is not None:
        discs.append('gnat-' + gnatls_v.groups()[0])

    for r in gnat.supported_runtimes:
        if r in ('zfp', 'sjlj') and (
                not RTS or RTS == r):
            discs.append('rts-' + r)

    if gnat.features.get('shared_runtime', False):
        discs.append('SHARED')
    if gnat.features.get('addr2line_lib', False):
        discs.append('LIBADDR2LINE')
    if gnat.features.get('shm', False):
        discs.append('SHM')
    if not gnat.features['float_io']:
        discs.append('No_Float_IO')
    if not gnat.features['exception_propagation']:
        discs.append("No_Exception_Propagation")

    return discs
