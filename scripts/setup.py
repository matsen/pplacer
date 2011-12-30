try:
    # Prefer setuptools if it exists - cleaner uninstall
    from setuptools import setup
except ImportError:
    from distutils.core import setup

import glob
import re
import warnings

VERSION_FILE = '../common_src/version.ml'
VERSION_PATTERN = r'^let version = "(.*)"$'

def get_version():
    """
    parse version
    """
    r = re.compile(VERSION_PATTERN)
    with open(VERSION_FILE) as fp:
        for line in fp:
            m = r.match(line)
            if m:
                return m.group(1)
    raise ValueError("Couldn't find version in {0}".format(VERSION_FILE))

def scripts():
    # Scripts depending on biopython
    scripts = [i for i in glob.glob('*.py') if not i.startswith('test_') and i != 'setup.py']
    bio_depends = ['refpkg_align.py']

    try:
        # Check for biopython
        import Bio
    except ImportError:
        warnings.warn("BioPython is not installed. Proceeding without {0}".format(
            ' '.join(bio_depends)))
        scripts = frozenset(scripts) - frozenset(bio_depends)

    return scripts

setup(name='pplacer-scripts',
      version=get_version(),
      description='Complementary scripts for use with pplacer',
      author='Erick Matsen, Aaron Gallagher, Connor McCoy, Brian Hodges',
      url='http://matsen.fhcrc.org/pplacer',
      scripts=scripts(),
)
