# After editing this file, please check that it is still valid by running
# python machines.py

# The keys that can be used in the dictionaries below

gprconfig = 'gprconfig'  # gprconfig command line
descr = 'descr'      # description
discs = 'discs'      # discriminants
ada = 'Ada'
c = 'C'
cpp = 'C++'
asm = 'ASM'
fortran = 'Fortran'
usr_bin = '/usr/bin'
opt_bin = '/opt/bin'
opt_gpp_bin = '/opt/gnu/g++/bin'

# A dictionary of all possible config names. Any name you use later in the
# machine description must appear in this list. The idea is to have as
# homogeneous a set as possible. The key is the config name, the value is
# the command line arguments for gprconfig
# If set to none, gprconfig will not be spawned explicitly, but gprbuild will
# create auto.cgpr. Otherwise, the command line can be specified as either a
# string, or a list of tuples (lang, path) which are automatically transformed
# into --config parameters.
# Note that if you set the gprconfig command line to None (thus requesting
# automatic generation of the config file), you also need to specify
# discrimants for the languages that you know are on the PATH, or most tests
# will not be run. In this mode, the additional discriminant AUTOCGPR is set

configs = {
    'AdaC_auto': None,  # A config that does not run gprconfig directly, but
                        # creates auto.cgpr in gprbuild
    'AdaCC++_auto': None}


def add_configs(version, path=''):
    """Add standard configs for a given gnatpro version.

    If GNAT_PATH is unspecified, the first compiler on the path will be
    used.
    If path is empty then use the first compiler found in $PATH
    """
    global configs
    configs.update({
        'AdaC_' + version: [(ada, path), (c, path)],
        'AdaCC++_' + version:         [(ada, path), (c, path), (cpp, path)],
        'AdaC++_' + version:          [(ada, path), (cpp, path)],
        'AdaCASM_' + version:         [(ada, path), (c, path), (asm, path)],
        'AdaCCASM_' + version:
        [(ada, path), (c, path), (cpp, path), (asm, path)],
        'Ada_' + version + '-C_gcc32':  [(ada, path), (c, usr_bin)],
        'Ada_' + version + '-C_gcc33':  [(ada, path), (c, usr_bin)],
        'Ada_' + version + '-C_gcc34':  [(ada, path), (c, usr_bin)],
        'Ada_' + version + '-C_gcc41':  [(ada, path), (c, usr_bin)],
        'Ada_' + version + '-C_gcc42':  [(ada, path), (c, usr_bin)],
        'Ada_' + version + '-C_gcc43':  [(ada, path), (c, opt_bin)],
        'Ada_' + version + '-Fortran':  [(ada, path), (fortran, usr_bin)],
        'AdaC_' + version + '-Fortran': [(ada, path),
                                         (c, path), (fortran, usr_bin)],
        'Ada_' + version + '-CC++_gcc33':
        [(ada, path), (c, opt_gpp_bin), (cpp, opt_gpp_bin)],
        'Ada_' + version + '-CC++_gcc34':
        [(ada, path), (c, opt_gpp_bin), (cpp, opt_gpp_bin)],
        'Ada_' + version + '-CC++_gcc41':
        [(ada, path), (c, opt_gpp_bin), (cpp, opt_gpp_bin)],
        'Ada_' + version + '-CC++_gcc43': [(ada, path), (c, opt_bin),
                                           (cpp, opt_bin)]
    })

# Description of the machines
# Only the machines from the standard servers should be described here,
# Developers' machines should be in a new file local_machines.py, not checked
# in. Its format is the same.
# For each machine, list the platforms it supports. For each platform, list
# the configs.
# For each platform or config, you can specify "discs", which is the lists of
# discrimants to apply. A config inherits the discrimants of the platform
# unless it overrides them. A config can also override, via its "gprconfig"
# key, the default gprconfig parameters inherited from the global configs
# dict.

machines = {}
