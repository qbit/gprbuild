#!/usr/bin/env gnatpython
"""Run a gprconfig test"""
from gnatpython.arch import Arch
from gnatpython.env import Env
from gnatpython.ex import Run
from gnatpython.fileutils import mkdir, cd, mv, rm, diff

import os
import re
import sys

os = os                         # to export os in all test.py
env = Env()

# Restore env
env.restore(os.environ["TEST_CONFIG"])

GPRCONFIG = env.config['gprconfig']
GNATMAKE = env.config['gnatmake']

# Move to test directory
TEST_DIR = os.path.dirname(sys.modules['__main__'].__file__)

# Get the canonical path (it will be returned by gprconfig
# and we want to strip it)
if not env.host.platform.endswith('windows'):
    TEST_DIR = os.path.realpath(TEST_DIR)

cd(TEST_DIR)


def create_fake_c_compiler(comp_dir, comp_target,
                           gcc_version, comp_is_cross=False):
    if comp_is_cross:
        comp_prefix = comp_target + '-'
    else:
        comp_prefix = ""

    arch = Arch()
    comp_dict = {'comp_dir': comp_dir,
                 'comp_target': comp_target,
                 'gcc_version': gcc_version,
                 'comp_prefix': comp_prefix,
                 'exeext': arch.os.exeext}

    mkdir(os.path.join(comp_dir, 'bin'))
    gcc_adb = open(os.path.join(comp_dir, 'bin', 'gcc.adb'), 'w')
    gcc_adb.write(
"""
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure gcc is
begin
   if Argument_Count >= 1 and then Argument (1) = "-v" then
        Put_Line ("gcc version %(gcc_version)s 20130908 for GNAT Pro");
   elsif Argument_Count >= 1 and then Argument (1) = "--version" then
        Put_Line ("gcc (GCC) %(gcc_version)s");
   elsif Argument_Count >= 1 and then Argument (1) = "-dumpmachine" then
        Put_Line ("%(comp_target)s");
   else
         Put ("Running gcc");
         for J in 1 .. Argument_Count loop
             Put (" " & Argument (J));
         end loop;
   end if;
end gcc;
""" % comp_dict)
    gcc_adb.close()
    env.store()

    cd(os.path.join(comp_dir, 'bin'))

    # Delete previously built gcc (case in invalid_1 test)
    rm('%(comp_prefix)sgcc%(exeext)s' % comp_dict)

    Run([GNATMAKE, 'gcc.adb', '-o',
         'dummy-%(comp_prefix)sgcc%(exeext)s' % comp_dict])
    mv('dummy-%(comp_prefix)sgcc%(exeext)s' % comp_dict,
       '%(comp_prefix)sgcc%(exeext)s' % comp_dict)
    env.restore()


def create_fake_ada_compiler(comp_dir, comp_target, gnat_version,
                             gcc_version, comp_is_cross=False,
                             runtimes=["native", "sjlj"],
                             create_symlink=False,
                             create_ada_object_path=False):
    """If create_symlink is true, the default runtime will be made
       available through an 'adalib' symbolic link, as is the case
       on Unix systems for standard GNAT installations.
       If create_ada_object_path is true, that file will be created
       to simulate a Windows install.
       The first runtime in the list is also the default one.
    """

    if comp_is_cross:
        comp_prefix = comp_target + '-'
    else:
        comp_prefix = ""

    arch = Arch()
    comp_dict = {'comp_dir': comp_dir,
                 'comp_target': comp_target,
                 'gnat_version': gnat_version,
                 'gcc_version': gcc_version,
                 'comp_prefix': comp_prefix,
                 'exeext': arch.os.exeext}

    mkdir(os.path.join(comp_dir, 'bin'))
    gnatls_adb = open(os.path.join(comp_dir, 'bin', 'gnatls.adb'), 'w')
    gnatls_adb.write(
"""
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure gnatls is
begin
   if Argument_Count >= 1 and Argument (1) = "-v" then
        Put_Line ("GNATLS Pro %(gnat_version)s (20070123-34)");
   else
         Put ("Running gnatls");
         for J in 1 .. Argument_Count loop
             Put (" " & Argument (J));
         end loop;
   end if;
end gnatls;
""" % comp_dict)
    gnatls_adb.close()

    gcc_adb = open(os.path.join(comp_dir, 'bin', 'gcc.adb'), 'w')
    gcc_adb.write(
"""
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure gcc is
begin
   if Argument_Count >= 1 and then Argument (1) = "-v" then
        Put_Line ("gcc version %(gcc_version)s 20131008 for GNAT Pro");
   elsif Argument_Count >= 1 and then Argument (1) = "--version" then
        Put_Line ("gcc (GCC) %(gcc_version)s");
   elsif Argument_Count >= 1 and then Argument (1) = "-dumpmachine" then
        Put_Line ("%(comp_target)s");
   else
         Put ("Running gcc");
         for J in 1 .. Argument_Count loop
             Put (" " & Argument (J));
         end loop;
   end if;
end gcc;
"""  % comp_dict)
    gcc_adb.close()

    gnatmake_adb = open(os.path.join(comp_dir, 'bin', 'gnatmake.adb'), 'w')
    gnatmake_adb.write(
"""
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure gnatmake is
begin
         Put ("Running gcc");
         for J in 1 .. Argument_Count loop
             Put (" " & Argument (J));
         end loop;
end gnatmake;
""")
    gnatmake_adb.close()

    env.store()

    cd(os.path.join(comp_dir, 'bin'))

    for tool in ['gnatmake', 'gcc', 'gnatls']:
        comp_dict['bin'] = tool
        Run([GNATMAKE, tool + '.adb', '-o',
         'dummy-%(comp_prefix)s%(bin)s%(exeext)s' % comp_dict])

    for tool in ['gnatmake', 'gcc', 'gnatls']:
        comp_dict['bin'] = tool
        mv('dummy-%(comp_prefix)s%(bin)s%(exeext)s' % comp_dict,
           '%(comp_prefix)s%(bin)s%(exeext)s' % comp_dict)

    env.restore()

    if comp_target == "dotnet":
        for dir in ("adalib", "adainclude"):
            mkdir(os.path.join(comp_dir, 'lib', 'dotgnat', dir))
    else:
        for runtime in runtimes:
            for dir in ("adalib", "adainclude"):
                mkdir(os.path.join(comp_dir, 'lib', 'gcc', comp_target,
                                   gcc_version, 'rts-%s' % runtime, dir))

    libdir = os.path.join(comp_dir, 'lib', 'gcc', comp_target, gcc_version)

    # On Unix systems, we have a symbolic link for the default
    # runtime. gprconfig should automatically detect these are
    # the same two runtimes and only list "native".

    if create_symlink:
        os.symlink(
            os.path.join('rts-native', 'adalib'),
            os.path.join(libdir, 'adalib'))

    # Simulate windows system, with an ada_object_path file

    if create_ada_object_path:
        with open(os.path.join(libdir, 'ada_object_path'), 'w') as ada_obj:
            ada_obj.write("rts-%s/adalib" % runtimes[0])


def gprconfig(options):
    p = Run([GPRCONFIG] + options.split(' '))
    p_out = p.out.rstrip()
    if p_out:
        print p_out


def __format_output(output):
    output = output.replace('\\', '/')
    quoted_test_dir = TEST_DIR.replace('\\', '/')
    for l in ('C:', 'c:'):
        quoted_test_dir = re.sub(l, '', quoted_test_dir)
        output = re.sub(l, '', output)
    output = re.sub(quoted_test_dir, '<pwd>', output)
    output = re.sub(quoted_test_dir.lower(), '<pwd>', output)
    return output


def test_gprconfig_batch(cmd):
    p = Run([GPRCONFIG, '-o', 'output.gpr', '--db-', '--db',
             os.environ['GPRCONFIG_SUPPORT'], '--batch'] + cmd)
    output = p.out.rstrip()
    output = re.sub('Creating configuration file.*', '', output)
    if output:
        print __format_output(output)

    if os.path.exists('output.gpr'):
        output_file = open('output.gpr')
        output = output_file.read()
        output = __format_output(output)
        output_file.close()
        for line in output.splitlines():
            if not line.startswith('-- '):
                print line
    else:
        print "Report from test driver: No output.gpr file created"


def test_gprconfig_error(error_msg, cmd):
    p = Run([GPRCONFIG, '-o', 'output.gpr', '--db-', '--db',
             os.environ['GPRCONFIG_SUPPORT'], '--batch'] + cmd)
    if p.out != error_msg:
        print "Invalid error message %s" % p.out
        print "Expecting " + error_msg


def test_gprconfig_interactive(inputs, cmd):
    p = Run([GPRCONFIG, '-o', 'output.gpr', '--db-', '--db',
             os.environ['GPRCONFIG_SUPPORT']] + cmd,
            input='|%s' % inputs)
    output = p.out
    output = __format_output(output)
    for line in output.splitlines():
        if not line.startswith('-- '):
            print line


def diff_gpr(left_f, right_f):
    f = open(left_f)
    left = f.read()
    f.close()

    f = open(right_f)
    right = f.read()
    f.close()

    left = re.sub('--', '', left)
    left = " ".join(left.splitlines())

    right = right.replace(os.environ['GPRCONFIG_SUPPORT'], '<support>')
    right = re.sub(r'RUNTIME_DIR=.*', 'RUNTIME_DIR=...,', right)
    right = right.replace(GPRCONFIG, 'gprconfig')
    right = re.sub(r'[^ ]*gprconfig.exe', ' gprconfig', right)
    right = re.sub('--', '', right)
    right = " ".join(right.splitlines())

    diff_content = diff(left, right).rstrip()
    if diff_content:
        print diff_content
