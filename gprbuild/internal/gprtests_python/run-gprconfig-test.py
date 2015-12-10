#!/usr/bin/env python
"""./run-gprconfig-test [OPTIONS] [TESTNAME]

Run gprconfig testsuite
"""

from testsuite_support import main, parse_options, get_config_dict
from gnatpython.fileutils import cp

import os


def setup_gprconfig_db(gprconfig_db, conf):
    """Copy all need support files from `gprconfig`/share/gprconfig/"""
    if not gprconfig_db:
        gprconfig_db = os.path.join(
            os.path.dirname(os.path.dirname(conf['gprconfig'])),
            'share', 'gprconfig')

    cp(os.path.join(gprconfig_db, 'compilers.xml'),
       os.path.join('support', 'compilers.xml'))

    cp(os.path.join(gprconfig_db, 'targetset.xml'),
       os.path.join('support', 'targetset.xml'))

    cp(os.path.join(gprconfig_db, 'nocompiler.xml'),
       os.path.join('support', 'nocompiler.xml'))


def run_testsuite():
    """Run the gprconfig testsuite"""
    opts = parse_options()
    conf = get_config_dict(opts)
    setup_gprconfig_db(opts.gprconfig_db, conf)
    main('gprconfig-tests', opts, conf)


if __name__ == '__main__':
    run_testsuite()
