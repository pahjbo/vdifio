import os, sys

try:
    from ctypesgen import main as ct
except:
    print ('Error: required Python ctypesgen module not installed!')
    sys.exit(-1)
     

import setuptools

with open("README", "r") as fh:
    ld = fh.read()
    
    
def build_ctypes():
    args = ['-l','vdifio','-o','vdifio/vdifio.py','../src/vdifio.h']
    ct.main(args)
 
if 'build' in sys.argv:
    build_ctypes()
    
setuptools.setup(
    name = 'vdifio',
    packages=setuptools.find_packages(),
    version = '1.1',
    description = ('A ctypes-based Python wrapper to the vdifio C/C++ library'),
    long_description=ld,
    license = 'LICENSE',
    # built in? install_requires = ['ctypes'],
)
