#!/usr/bin/env python
"""./run-gprbuild-test [OPTIONS] [TESTNAME]

Run gprbuild testsuite
"""

from testsuite_support import (
    main, parse_options, get_config_dict, get_gnat_discs)
from gnatpython.env import Env
from gnatpython.ex import Run

from machines import configs, machines, add_configs

import logging
import os
import re


def get_config(name):
    # If there is a local_machines file, it can be used to add the developers
    # machines. The format is the same as in machines.py, and the machines are
    # added the ones declared in machines.py
    try:
        import local_machines
        machines.update(local_machines.machines)
    except ImportError:
        pass

    env = Env()
    # Process the lists of configs, to expand the gprconfig command lines
    for c in configs:
        if configs[c] is not None and not isinstance(configs[c], str):
            configs[c] = " ".join(["--config " + lang + ",,," + path
                                   for lang, path in configs[c]])

    # Check that all machines only use known configs. This helps ensures
    # consistency, and doing this check for all machines makes it easier to
    # check changes to machines.py
    # This also ensures that "gprconfig" and "discs" are defined for each
    # config (thus inheriting values as appropriate)

    for m, platforms in machines.iteritems():
        for _, local_configs in platforms.iteritems():
            discs = local_configs.get('discs', '')
            local_configs.pop('discs', None)

            for c, attrs in local_configs.iteritems():
                if c not in configs:
                    print "Unknown config %s used for machine %s" \
                        % (c, m)
                    return

                # Inherit the discriminants from the platform
                if 'discs' not in attrs:
                    attrs['discs'] = discs

                # Inherit the gprconfig command line from the config
                if 'gprconfig' not in attrs:
                    attrs['gprconfig'] = configs[c]

    # Find machine in machine list for native target
    try:
        local_m = machines[env.host.machine]
        machine_config = local_m[env.target.platform]
        if name is None:
            # return the full list
            return machine_config

        discs, gprconfig_cmd = (machine_config[name]['discs'],
                                machine_config[name]['gprconfig'])
        if discs != "":
            discs = discs.split()
        return (discs, gprconfig_cmd)
    except (KeyError):
        if name is None:
            return {}
        else:
            return ([], configs[name])


def run_gprconfig(config_out, gprconfig_path, gprconfig_cmdline, discs):
    """Generate gprconfig configuration file"""
    env = Env()

    # Add the compiler to be tested in $PATH
    r = re.search('--config.*Ada,[^ ]*,[^ ]*,([^ ]*)',
                  gprconfig_cmdline)
    if r:
        ada_compiler_path = r.groups()[0]

        # Add the compiler in the PATH, just after gprbuild
        path = os.environ['PATH'].split(os.pathsep)
        path.insert(1, ada_compiler_path)
        os.environ['PATH'] = os.pathsep.join(path)

        # Also update the dll_path
        env.add_dll_path(os.path.join(ada_compiler_path, os.pardir, 'lib'))
        env.add_dll_path(
            os.path.join(ada_compiler_path, os.pardir, 'lib', 'sparcv9'))

        if not discs:
            # No discs in configuration, auto detect
            discs = get_gnat_discs(ada_compiler_path)

    r = re.search('--config.*C\+\+,[^ ]*,[^ ]*,([^ ]*)',
                  gprconfig_cmdline)
    if r:
        # Add cpp dynamic libraries
        cpp_compiler_path = r.groups()[0]
        env.add_dll_path(os.path.join(
            cpp_compiler_path, os.pardir, 'lib'))

    assert env.target.triplet, "target triplet cannot be empty !"
    gprconfig = [gprconfig_path, '-o', config_out]
    gprconfig.append('--target=' + env.target.triplet)
    gprconfig.append('--batch')
    gprconfig += gprconfig_cmdline.split(' ')
    gprconfig_line = " ".join(gprconfig)
    p = Run(gprconfig)
    logging.debug(p.out)

    # Generate language discs
    language_discs = re.findall("--config[= ]([\w\+]*)",
                                gprconfig_line)
    gprconfig_out_f = open(config_out)
    gprconfig_out = gprconfig_out_f.read()
    if "GNAT 6" in gprconfig_out:
        language_discs.append("Ada2005")
    gprconfig_out_f.close()

    discs += language_discs

    return discs


def run_testsuite():
    opts = parse_options()

    if opts.show_config_list:
        config_list = get_config(None)
        if not config_list:
            print 'no specific config registered for this machine'
        else:
            for name in get_config(None):
                print name
        return

    _, version = opts.run_config.rsplit('_', 1)

    conf = get_config_dict(opts)
    if version.lower() != 'auto':
        add_configs(version)
        conf['config_out'] = os.path.join(os.getcwd(), 'config-' +
                                          opts.run_config)
        discs, gprconfig_cmd = get_config(opts.run_config)
        discs = run_gprconfig(conf['config_out'], conf['gprconfig'],
                              gprconfig_cmd, discs)
    else:
        discs = None

    main('tests', opts, conf, discs)


if __name__ == '__main__':
    run_testsuite()
