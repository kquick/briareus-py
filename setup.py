# N.B.  consult https://github.com/pypa/sampleproject

from setuptools import setup, find_packages
from os import path

setup(
    name = "briareus",
    version = "0.2",
    description = 'Build configuration generator',
    # long_description = ...,
    # long_description_content_type = 'text/markdown',
    # url = ...,
    author = "Kevin Quick",
    author_email = "kq1quick@gmail.com",
    classifiers = [
        'Development status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'Topic :: Software Development :: Build Tools',
        # 'License :: OSI Approved :: ? License',
        'Programming Language :: Python :: 3',
    ],
    keywords='ci build development',
    packages = find_packages(), # ["src/Briareus"],
    package_data={ '': ['*.pl'] },   # Include Prolog scripts in packages
    data_files=[('hydra', ['hydra/sysconfig.nix', 'hydra/copy_hh.nix'])],
    python_requires = '>=3.3',
    # install_requires=[],  # for pip
    entry_points={ 'console_scripts': [ 'hh=Briareus.hh:main' ], },
    # project_urls={ 'Bug Reports': ?, 'Source': ? },
)
