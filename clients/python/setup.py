#
# Copyright 2009 Paul J. Davis <paul.joseph.davis@gmail.com>
#
# This file is part of the gloom package released under the MIT license.
"""Gloom.py is a client to the Gloom job distribution framework."""

import ez_setup
ez_setup.use_setuptools()
from setuptools import setup

setup(
    name = "gloom",
    version = "0.0.1",
    license = "MIT",
    author = "Paul J. Davis",
    author_email = "paul.joseph.davis@gmail.com",
    description = "Gloom client",
    long_description = __doc__,
    url = "http://github.com/davisp/gloom",
    download_url = "http://pypi.python.org/pypi/gloom/",
    zip_safe = True,
    
    classifiers = [
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Natural Language :: English',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Topic :: Software Development :: Libraries :: Python Modules',
    ],
    
    setup_requires = [
        'setuptools>=0.6c8'
    ],

    test_suite = 'nose.collector',
)
