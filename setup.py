#!/usr/bin/env python3

import merc
from setuptools import setup, find_packages


with open("requirements.txt") as f:
  requires = f.read().splitlines()


setup(name="merc",
      version=merc.__version__,
      description="merc",
      packages=find_packages(),
      include_package_data=True,
      zip_safe=False,
      test_suite="merc",
      install_requires=requires,
      entry_points="""\
      [console_scripts]
      merc = merc:main
      """
      )
