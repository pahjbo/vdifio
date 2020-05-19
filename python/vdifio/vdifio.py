r"""Wrapper for vdifio.h

Generated with:
/home/packages/p3env/bin/ctypesgen -l vdifio ../src/vdifio.h

Do not modify this file.
"""

__docformat__ = "restructuredtext"

# Begin preamble for Python v(3, 2)

import ctypes, os, sys
from ctypes import *

_int_types = (c_int16, c_int32)
if hasattr(ctypes, "c_int64"):
    # Some builds of ctypes apparently do not have c_int64
    # defined; it's a pretty good bet that these builds do not
    # have 64-bit pointers.
    _int_types += (c_int64,)
for t in _int_types:
    if sizeof(t) == sizeof(c_size_t):
        c_ptrdiff_t = t
del t
del _int_types


class UserString:
    def __init__(self, seq):
        if isinstance(seq, bytes):
            self.data = seq
        elif isinstance(seq, UserString):
            self.data = seq.data[:]
        else:
            self.data = str(seq).encode()

    def __bytes__(self):
        return self.data

    def __str__(self):
        return self.data.decode()

    def __repr__(self):
        return repr(self.data)

    def __int__(self):
        return int(self.data.decode())

    def __long__(self):
        return int(self.data.decode())

    def __float__(self):
        return float(self.data.decode())

    def __complex__(self):
        return complex(self.data.decode())

    def __hash__(self):
        return hash(self.data)

    def __cmp__(self, string):
        if isinstance(string, UserString):
            return cmp(self.data, string.data)
        else:
            return cmp(self.data, string)

    def __le__(self, string):
        if isinstance(string, UserString):
            return self.data <= string.data
        else:
            return self.data <= string

    def __lt__(self, string):
        if isinstance(string, UserString):
            return self.data < string.data
        else:
            return self.data < string

    def __ge__(self, string):
        if isinstance(string, UserString):
            return self.data >= string.data
        else:
            return self.data >= string

    def __gt__(self, string):
        if isinstance(string, UserString):
            return self.data > string.data
        else:
            return self.data > string

    def __eq__(self, string):
        if isinstance(string, UserString):
            return self.data == string.data
        else:
            return self.data == string

    def __ne__(self, string):
        if isinstance(string, UserString):
            return self.data != string.data
        else:
            return self.data != string

    def __contains__(self, char):
        return char in self.data

    def __len__(self):
        return len(self.data)

    def __getitem__(self, index):
        return self.__class__(self.data[index])

    def __getslice__(self, start, end):
        start = max(start, 0)
        end = max(end, 0)
        return self.__class__(self.data[start:end])

    def __add__(self, other):
        if isinstance(other, UserString):
            return self.__class__(self.data + other.data)
        elif isinstance(other, bytes):
            return self.__class__(self.data + other)
        else:
            return self.__class__(self.data + str(other).encode())

    def __radd__(self, other):
        if isinstance(other, bytes):
            return self.__class__(other + self.data)
        else:
            return self.__class__(str(other).encode() + self.data)

    def __mul__(self, n):
        return self.__class__(self.data * n)

    __rmul__ = __mul__

    def __mod__(self, args):
        return self.__class__(self.data % args)

    # the following methods are defined in alphabetical order:
    def capitalize(self):
        return self.__class__(self.data.capitalize())

    def center(self, width, *args):
        return self.__class__(self.data.center(width, *args))

    def count(self, sub, start=0, end=sys.maxsize):
        return self.data.count(sub, start, end)

    def decode(self, encoding=None, errors=None):  # XXX improve this?
        if encoding:
            if errors:
                return self.__class__(self.data.decode(encoding, errors))
            else:
                return self.__class__(self.data.decode(encoding))
        else:
            return self.__class__(self.data.decode())

    def encode(self, encoding=None, errors=None):  # XXX improve this?
        if encoding:
            if errors:
                return self.__class__(self.data.encode(encoding, errors))
            else:
                return self.__class__(self.data.encode(encoding))
        else:
            return self.__class__(self.data.encode())

    def endswith(self, suffix, start=0, end=sys.maxsize):
        return self.data.endswith(suffix, start, end)

    def expandtabs(self, tabsize=8):
        return self.__class__(self.data.expandtabs(tabsize))

    def find(self, sub, start=0, end=sys.maxsize):
        return self.data.find(sub, start, end)

    def index(self, sub, start=0, end=sys.maxsize):
        return self.data.index(sub, start, end)

    def isalpha(self):
        return self.data.isalpha()

    def isalnum(self):
        return self.data.isalnum()

    def isdecimal(self):
        return self.data.isdecimal()

    def isdigit(self):
        return self.data.isdigit()

    def islower(self):
        return self.data.islower()

    def isnumeric(self):
        return self.data.isnumeric()

    def isspace(self):
        return self.data.isspace()

    def istitle(self):
        return self.data.istitle()

    def isupper(self):
        return self.data.isupper()

    def join(self, seq):
        return self.data.join(seq)

    def ljust(self, width, *args):
        return self.__class__(self.data.ljust(width, *args))

    def lower(self):
        return self.__class__(self.data.lower())

    def lstrip(self, chars=None):
        return self.__class__(self.data.lstrip(chars))

    def partition(self, sep):
        return self.data.partition(sep)

    def replace(self, old, new, maxsplit=-1):
        return self.__class__(self.data.replace(old, new, maxsplit))

    def rfind(self, sub, start=0, end=sys.maxsize):
        return self.data.rfind(sub, start, end)

    def rindex(self, sub, start=0, end=sys.maxsize):
        return self.data.rindex(sub, start, end)

    def rjust(self, width, *args):
        return self.__class__(self.data.rjust(width, *args))

    def rpartition(self, sep):
        return self.data.rpartition(sep)

    def rstrip(self, chars=None):
        return self.__class__(self.data.rstrip(chars))

    def split(self, sep=None, maxsplit=-1):
        return self.data.split(sep, maxsplit)

    def rsplit(self, sep=None, maxsplit=-1):
        return self.data.rsplit(sep, maxsplit)

    def splitlines(self, keepends=0):
        return self.data.splitlines(keepends)

    def startswith(self, prefix, start=0, end=sys.maxsize):
        return self.data.startswith(prefix, start, end)

    def strip(self, chars=None):
        return self.__class__(self.data.strip(chars))

    def swapcase(self):
        return self.__class__(self.data.swapcase())

    def title(self):
        return self.__class__(self.data.title())

    def translate(self, *args):
        return self.__class__(self.data.translate(*args))

    def upper(self):
        return self.__class__(self.data.upper())

    def zfill(self, width):
        return self.__class__(self.data.zfill(width))


class MutableString(UserString):
    """mutable string objects

    Python strings are immutable objects.  This has the advantage, that
    strings may be used as dictionary keys.  If this property isn't needed
    and you insist on changing string values in place instead, you may cheat
    and use MutableString.

    But the purpose of this class is an educational one: to prevent
    people from inventing their own mutable string class derived
    from UserString and than forget thereby to remove (override) the
    __hash__ method inherited from UserString.  This would lead to
    errors that would be very hard to track down.

    A faster and better solution is to rewrite your program using lists."""

    def __init__(self, string=""):
        self.data = string

    def __hash__(self):
        raise TypeError("unhashable type (it is mutable)")

    def __setitem__(self, index, sub):
        if index < 0:
            index += len(self.data)
        if index < 0 or index >= len(self.data):
            raise IndexError
        self.data = self.data[:index] + sub + self.data[index + 1 :]

    def __delitem__(self, index):
        if index < 0:
            index += len(self.data)
        if index < 0 or index >= len(self.data):
            raise IndexError
        self.data = self.data[:index] + self.data[index + 1 :]

    def __setslice__(self, start, end, sub):
        start = max(start, 0)
        end = max(end, 0)
        if isinstance(sub, UserString):
            self.data = self.data[:start] + sub.data + self.data[end:]
        elif isinstance(sub, bytes):
            self.data = self.data[:start] + sub + self.data[end:]
        else:
            self.data = self.data[:start] + str(sub).encode() + self.data[end:]

    def __delslice__(self, start, end):
        start = max(start, 0)
        end = max(end, 0)
        self.data = self.data[:start] + self.data[end:]

    def immutable(self):
        return UserString(self.data)

    def __iadd__(self, other):
        if isinstance(other, UserString):
            self.data += other.data
        elif isinstance(other, bytes):
            self.data += other
        else:
            self.data += str(other).encode()
        return self

    def __imul__(self, n):
        self.data *= n
        return self


class String(MutableString, Union):

    _fields_ = [("raw", POINTER(c_char)), ("data", c_char_p)]

    def __init__(self, obj=""):
        if isinstance(obj, (bytes, UserString)):
            self.data = bytes(obj)
        else:
            self.raw = obj

    def __len__(self):
        return self.data and len(self.data) or 0

    def from_param(cls, obj):
        # Convert None or 0
        if obj is None or obj == 0:
            return cls(POINTER(c_char)())

        # Convert from String
        elif isinstance(obj, String):
            return obj

        # Convert from bytes
        elif isinstance(obj, bytes):
            return cls(obj)

        # Convert from str
        elif isinstance(obj, str):
            return cls(obj.encode())

        # Convert from c_char_p
        elif isinstance(obj, c_char_p):
            return obj

        # Convert from POINTER(c_char)
        elif isinstance(obj, POINTER(c_char)):
            return obj

        # Convert from raw pointer
        elif isinstance(obj, int):
            return cls(cast(obj, POINTER(c_char)))

        # Convert from c_char array
        elif isinstance(obj, c_char * len(obj)):
            return obj

        # Convert from object
        else:
            return String.from_param(obj._as_parameter_)

    from_param = classmethod(from_param)


def ReturnString(obj, func=None, arguments=None):
    return String.from_param(obj)


# As of ctypes 1.0, ctypes does not support custom error-checking
# functions on callbacks, nor does it support custom datatypes on
# callbacks, so we must ensure that all callbacks return
# primitive datatypes.
#
# Non-primitive return values wrapped with UNCHECKED won't be
# typechecked, and will be converted to c_void_p.
def UNCHECKED(type):
    if hasattr(type, "_type_") and isinstance(type._type_, str) and type._type_ != "P":
        return type
    else:
        return c_void_p


# ctypes doesn't have direct support for variadic functions, so we have to write
# our own wrapper class
class _variadic_function(object):
    def __init__(self, func, restype, argtypes, errcheck):
        self.func = func
        self.func.restype = restype
        self.argtypes = argtypes
        if errcheck:
            self.func.errcheck = errcheck

    def _as_parameter_(self):
        # So we can pass this variadic function as a function pointer
        return self.func

    def __call__(self, *args):
        fixed_args = []
        i = 0
        for argtype in self.argtypes:
            # Typecheck what we can
            fixed_args.append(argtype.from_param(args[i]))
            i += 1
        return self.func(*fixed_args + list(args[i:]))


def ord_if_char(value):
    """
    Simple helper used for casts to simple builtin types:  if the argument is a
    string type, it will be converted to it's ordinal value.

    This function will raise an exception if the argument is string with more
    than one characters.
    """
    return ord(value) if (isinstance(value, bytes) or isinstance(value, str)) else value

# End preamble

_libs = {}
_libdirs = []

# Begin loader

# ----------------------------------------------------------------------------
# Copyright (c) 2008 David James
# Copyright (c) 2006-2008 Alex Holkner
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#  * Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in
#    the documentation and/or other materials provided with the
#    distribution.
#  * Neither the name of pyglet nor the names of its
#    contributors may be used to endorse or promote products
#    derived from this software without specific prior written
#    permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ----------------------------------------------------------------------------

import os.path, re, sys, glob
import platform
import ctypes
import ctypes.util


def _environ_path(name):
    if name in os.environ:
        return os.environ[name].split(":")
    else:
        return []


class LibraryLoader(object):
    # library names formatted specifically for platforms
    name_formats = ["%s"]

    class Lookup(object):
        mode = ctypes.DEFAULT_MODE

        def __init__(self, path):
            super(LibraryLoader.Lookup, self).__init__()
            self.access = dict(cdecl=ctypes.CDLL(path, self.mode))

        def get(self, name, calling_convention="cdecl"):
            if calling_convention not in self.access:
                raise LookupError(
                    "Unknown calling convention '{}' for function '{}'".format(
                        calling_convention, name
                    )
                )
            return getattr(self.access[calling_convention], name)

        def has(self, name, calling_convention="cdecl"):
            if calling_convention not in self.access:
                return False
            return hasattr(self.access[calling_convention], name)

        def __getattr__(self, name):
            return getattr(self.access["cdecl"], name)

    def __init__(self):
        self.other_dirs = []

    def __call__(self, libname):
        """Given the name of a library, load it."""
        paths = self.getpaths(libname)

        for path in paths:
            try:
                return self.Lookup(path)
            except:
                pass

        raise ImportError("Could not load %s." % libname)

    def getpaths(self, libname):
        """Return a list of paths where the library might be found."""
        if os.path.isabs(libname):
            yield libname
        else:
            # search through a prioritized series of locations for the library

            # we first search any specific directories identified by user
            for dir_i in self.other_dirs:
                for fmt in self.name_formats:
                    # dir_i should be absolute already
                    yield os.path.join(dir_i, fmt % libname)

            # then we search the directory where the generated python interface is stored
            for fmt in self.name_formats:
                yield os.path.abspath(os.path.join(os.path.dirname(__file__), fmt % libname))

            # now, use the ctypes tools to try to find the library
            for fmt in self.name_formats:
                path = ctypes.util.find_library(fmt % libname)
                if path:
                    yield path

            # then we search all paths identified as platform-specific lib paths
            for path in self.getplatformpaths(libname):
                yield path

            # Finally, we'll try the users current working directory
            for fmt in self.name_formats:
                yield os.path.abspath(os.path.join(os.path.curdir, fmt % libname))

    def getplatformpaths(self, libname):
        return []


# Darwin (Mac OS X)


class DarwinLibraryLoader(LibraryLoader):
    name_formats = [
        "lib%s.dylib",
        "lib%s.so",
        "lib%s.bundle",
        "%s.dylib",
        "%s.so",
        "%s.bundle",
        "%s",
    ]

    class Lookup(LibraryLoader.Lookup):
        # Darwin requires dlopen to be called with mode RTLD_GLOBAL instead
        # of the default RTLD_LOCAL.  Without this, you end up with
        # libraries not being loadable, resulting in "Symbol not found"
        # errors
        mode = ctypes.RTLD_GLOBAL

    def getplatformpaths(self, libname):
        if os.path.pathsep in libname:
            names = [libname]
        else:
            names = [format % libname for format in self.name_formats]

        for dir in self.getdirs(libname):
            for name in names:
                yield os.path.join(dir, name)

    def getdirs(self, libname):
        """Implements the dylib search as specified in Apple documentation:

        http://developer.apple.com/documentation/DeveloperTools/Conceptual/
            DynamicLibraries/Articles/DynamicLibraryUsageGuidelines.html

        Before commencing the standard search, the method first checks
        the bundle's ``Frameworks`` directory if the application is running
        within a bundle (OS X .app).
        """

        dyld_fallback_library_path = _environ_path("DYLD_FALLBACK_LIBRARY_PATH")
        if not dyld_fallback_library_path:
            dyld_fallback_library_path = [os.path.expanduser("~/lib"), "/usr/local/lib", "/usr/lib"]

        dirs = []

        if "/" in libname:
            dirs.extend(_environ_path("DYLD_LIBRARY_PATH"))
        else:
            dirs.extend(_environ_path("LD_LIBRARY_PATH"))
            dirs.extend(_environ_path("DYLD_LIBRARY_PATH"))

        if hasattr(sys, "frozen") and sys.frozen == "macosx_app":
            dirs.append(os.path.join(os.environ["RESOURCEPATH"], "..", "Frameworks"))

        dirs.extend(dyld_fallback_library_path)

        return dirs


# Posix


class PosixLibraryLoader(LibraryLoader):
    _ld_so_cache = None

    _include = re.compile(r"^\s*include\s+(?P<pattern>.*)")

    class _Directories(dict):
        def __init__(self):
            self.order = 0

        def add(self, directory):
            if len(directory) > 1:
                directory = directory.rstrip(os.path.sep)
            # only adds and updates order if exists and not already in set
            if not os.path.exists(directory):
                return
            o = self.setdefault(directory, self.order)
            if o == self.order:
                self.order += 1

        def extend(self, directories):
            for d in directories:
                self.add(d)

        def ordered(self):
            return (i[0] for i in sorted(self.items(), key=lambda D: D[1]))

    def _get_ld_so_conf_dirs(self, conf, dirs):
        """
        Recursive funtion to help parse all ld.so.conf files, including proper
        handling of the `include` directive.
        """

        try:
            with open(conf) as f:
                for D in f:
                    D = D.strip()
                    if not D:
                        continue

                    m = self._include.match(D)
                    if not m:
                        dirs.add(D)
                    else:
                        for D2 in glob.glob(m.group("pattern")):
                            self._get_ld_so_conf_dirs(D2, dirs)
        except IOError:
            pass

    def _create_ld_so_cache(self):
        # Recreate search path followed by ld.so.  This is going to be
        # slow to build, and incorrect (ld.so uses ld.so.cache, which may
        # not be up-to-date).  Used only as fallback for distros without
        # /sbin/ldconfig.
        #
        # We assume the DT_RPATH and DT_RUNPATH binary sections are omitted.

        directories = self._Directories()
        for name in (
            "LD_LIBRARY_PATH",
            "SHLIB_PATH",  # HPUX
            "LIBPATH",  # OS/2, AIX
            "LIBRARY_PATH",  # BE/OS
        ):
            if name in os.environ:
                directories.extend(os.environ[name].split(os.pathsep))

        self._get_ld_so_conf_dirs("/etc/ld.so.conf", directories)

        bitage = platform.architecture()[0]

        unix_lib_dirs_list = []
        if bitage.startswith("64"):
            # prefer 64 bit if that is our arch
            unix_lib_dirs_list += ["/lib64", "/usr/lib64"]

        # must include standard libs, since those paths are also used by 64 bit
        # installs
        unix_lib_dirs_list += ["/lib", "/usr/lib"]
        if sys.platform.startswith("linux"):
            # Try and support multiarch work in Ubuntu
            # https://wiki.ubuntu.com/MultiarchSpec
            if bitage.startswith("32"):
                # Assume Intel/AMD x86 compat
                unix_lib_dirs_list += ["/lib/i386-linux-gnu", "/usr/lib/i386-linux-gnu"]
            elif bitage.startswith("64"):
                # Assume Intel/AMD x86 compat
                unix_lib_dirs_list += ["/lib/x86_64-linux-gnu", "/usr/lib/x86_64-linux-gnu"]
            else:
                # guess...
                unix_lib_dirs_list += glob.glob("/lib/*linux-gnu")
        directories.extend(unix_lib_dirs_list)

        cache = {}
        lib_re = re.compile(r"lib(.*)\.s[ol]")
        ext_re = re.compile(r"\.s[ol]$")
        for dir in directories.ordered():
            try:
                for path in glob.glob("%s/*.s[ol]*" % dir):
                    file = os.path.basename(path)

                    # Index by filename
                    cache_i = cache.setdefault(file, set())
                    cache_i.add(path)

                    # Index by library name
                    match = lib_re.match(file)
                    if match:
                        library = match.group(1)
                        cache_i = cache.setdefault(library, set())
                        cache_i.add(path)
            except OSError:
                pass

        self._ld_so_cache = cache

    def getplatformpaths(self, libname):
        if self._ld_so_cache is None:
            self._create_ld_so_cache()

        result = self._ld_so_cache.get(libname, set())
        for i in result:
            # we iterate through all found paths for library, since we may have
            # actually found multiple architectures or other library types that
            # may not load
            yield i


# Windows


class WindowsLibraryLoader(LibraryLoader):
    name_formats = ["%s.dll", "lib%s.dll", "%slib.dll", "%s"]

    class Lookup(LibraryLoader.Lookup):
        def __init__(self, path):
            super(WindowsLibraryLoader.Lookup, self).__init__(path)
            self.access["stdcall"] = ctypes.windll.LoadLibrary(path)


# Platform switching

# If your value of sys.platform does not appear in this dict, please contact
# the Ctypesgen maintainers.

loaderclass = {
    "darwin": DarwinLibraryLoader,
    "cygwin": WindowsLibraryLoader,
    "win32": WindowsLibraryLoader,
    "msys": WindowsLibraryLoader,
}

load_library = loaderclass.get(sys.platform, PosixLibraryLoader)()


def add_library_search_dirs(other_dirs):
    """
    Add libraries to search paths.
    If library paths are relative, convert them to absolute with respect to this
    file's directory
    """
    for F in other_dirs:
        if not os.path.isabs(F):
            F = os.path.abspath(F)
        load_library.other_dirs.append(F)


del loaderclass

# End loader

add_library_search_dirs([])

# Begin libraries
_libs["vdifio"] = load_library("vdifio")

# 1 libraries
# End libraries

# No modules

uint_fast32_t = c_ulong# /usr/include/stdint.h: 106

__off_t = c_long# /usr/include/bits/types.h: 140

__off64_t = c_long# /usr/include/bits/types.h: 141

__time_t = c_long# /usr/include/bits/types.h: 148

# /usr/include/libio.h: 246
class struct__IO_FILE(Structure):
    pass

FILE = struct__IO_FILE# /usr/include/stdio.h: 48

_IO_lock_t = None# /usr/include/libio.h: 155

# /usr/include/libio.h: 161
class struct__IO_marker(Structure):
    pass

struct__IO_marker.__slots__ = [
    '_next',
    '_sbuf',
    '_pos',
]
struct__IO_marker._fields_ = [
    ('_next', POINTER(struct__IO_marker)),
    ('_sbuf', POINTER(struct__IO_FILE)),
    ('_pos', c_int),
]

struct__IO_FILE.__slots__ = [
    '_flags',
    '_IO_read_ptr',
    '_IO_read_end',
    '_IO_read_base',
    '_IO_write_base',
    '_IO_write_ptr',
    '_IO_write_end',
    '_IO_buf_base',
    '_IO_buf_end',
    '_IO_save_base',
    '_IO_backup_base',
    '_IO_save_end',
    '_markers',
    '_chain',
    '_fileno',
    '_flags2',
    '_old_offset',
    '_cur_column',
    '_vtable_offset',
    '_shortbuf',
    '_lock',
    '_offset',
    '__pad1',
    '__pad2',
    '__pad3',
    '__pad4',
    '__pad5',
    '_mode',
    '_unused2',
]
struct__IO_FILE._fields_ = [
    ('_flags', c_int),
    ('_IO_read_ptr', String),
    ('_IO_read_end', String),
    ('_IO_read_base', String),
    ('_IO_write_base', String),
    ('_IO_write_ptr', String),
    ('_IO_write_end', String),
    ('_IO_buf_base', String),
    ('_IO_buf_end', String),
    ('_IO_save_base', String),
    ('_IO_backup_base', String),
    ('_IO_save_end', String),
    ('_markers', POINTER(struct__IO_marker)),
    ('_chain', POINTER(struct__IO_FILE)),
    ('_fileno', c_int),
    ('_flags2', c_int),
    ('_old_offset', __off_t),
    ('_cur_column', c_ushort),
    ('_vtable_offset', c_char),
    ('_shortbuf', c_char * int(1)),
    ('_lock', POINTER(_IO_lock_t)),
    ('_offset', __off64_t),
    ('__pad1', POINTER(None)),
    ('__pad2', POINTER(None)),
    ('__pad3', POINTER(None)),
    ('__pad4', POINTER(None)),
    ('__pad5', c_size_t),
    ('_mode', c_int),
    ('_unused2', c_char * int((((15 * sizeof(c_int)) - (4 * sizeof(POINTER(None)))) - sizeof(c_size_t)))),
]

off_t = __off_t# /usr/include/stdio.h: 90

time_t = __time_t# /usr/include/time.h: 75

# difx/libraries/vdifio/src/vdifio.h: 82
class struct_vdif_header(Structure):
    pass

struct_vdif_header.__slots__ = [
    'seconds',
    'legacymode',
    'invalid',
    'frame',
    'epoch',
    'unassigned',
    'framelength8',
    'nchan',
    'version',
    'stationid',
    'threadid',
    'nbits',
    'iscomplex',
    'extended1',
    'eversion',
    'extended2',
    'extended3',
    'extended4',
]
struct_vdif_header._fields_ = [
    ('seconds', c_uint32, 30),
    ('legacymode', c_uint32, 1),
    ('invalid', c_uint32, 1),
    ('frame', c_uint32, 24),
    ('epoch', c_uint32, 6),
    ('unassigned', c_uint32, 2),
    ('framelength8', c_uint32, 24),
    ('nchan', c_uint32, 5),
    ('version', c_uint32, 3),
    ('stationid', c_uint32, 16),
    ('threadid', c_uint32, 10),
    ('nbits', c_uint32, 5),
    ('iscomplex', c_uint32, 1),
    ('extended1', c_uint32, 24),
    ('eversion', c_uint32, 8),
    ('extended2', c_uint32),
    ('extended3', c_uint32),
    ('extended4', c_uint32),
]

vdif_header = struct_vdif_header# difx/libraries/vdifio/src/vdifio.h: 82

# difx/libraries/vdifio/src/vdifio.h: 109
class struct_vdif_edv1_header(Structure):
    pass

struct_vdif_edv1_header.__slots__ = [
    'seconds',
    'legacymode',
    'invalid',
    'frame',
    'epoch',
    'unassigned',
    'framelength8',
    'nchan',
    'version',
    'stationid',
    'threadid',
    'nbits',
    'iscomplex',
    'samprate',
    'samprateunits',
    'eversion',
    'syncword',
    'name',
]
struct_vdif_edv1_header._fields_ = [
    ('seconds', c_uint32, 30),
    ('legacymode', c_uint32, 1),
    ('invalid', c_uint32, 1),
    ('frame', c_uint32, 24),
    ('epoch', c_uint32, 6),
    ('unassigned', c_uint32, 2),
    ('framelength8', c_uint32, 24),
    ('nchan', c_uint32, 5),
    ('version', c_uint32, 3),
    ('stationid', c_uint32, 16),
    ('threadid', c_uint32, 10),
    ('nbits', c_uint32, 5),
    ('iscomplex', c_uint32, 1),
    ('samprate', c_uint32, 23),
    ('samprateunits', c_uint32, 1),
    ('eversion', c_uint32, 8),
    ('syncword', c_uint32),
    ('name', c_char * int(8)),
]

vdif_edv1_header = struct_vdif_edv1_header# difx/libraries/vdifio/src/vdifio.h: 109

# difx/libraries/vdifio/src/vdifio.h: 136
class struct_vdif_edv2_header_generic(Structure):
    pass

struct_vdif_edv2_header_generic.__slots__ = [
    'seconds',
    'legacymode',
    'invalid',
    'frame',
    'epoch',
    'unassigned',
    'framelength8',
    'nchan',
    'version',
    'stationid',
    'threadid',
    'nbits',
    'iscomplex',
    'notrelevant',
    'subsubversion',
    'eversion',
    'word5',
    'word6',
    'word7',
]
struct_vdif_edv2_header_generic._fields_ = [
    ('seconds', c_uint32, 30),
    ('legacymode', c_uint32, 1),
    ('invalid', c_uint32, 1),
    ('frame', c_uint32, 24),
    ('epoch', c_uint32, 6),
    ('unassigned', c_uint32, 2),
    ('framelength8', c_uint32, 24),
    ('nchan', c_uint32, 5),
    ('version', c_uint32, 3),
    ('stationid', c_uint32, 16),
    ('threadid', c_uint32, 10),
    ('nbits', c_uint32, 5),
    ('iscomplex', c_uint32, 1),
    ('notrelevant', c_uint32, 4),
    ('subsubversion', c_uint32, 20),
    ('eversion', c_uint32, 8),
    ('word5', c_uint32, 32),
    ('word6', c_uint32, 32),
    ('word7', c_uint32, 32),
]

vdif_edv2_header_generic = struct_vdif_edv2_header_generic# difx/libraries/vdifio/src/vdifio.h: 136

# difx/libraries/vdifio/src/vdifio.h: 165
class struct_vdif_edv2_header_alma(Structure):
    pass

struct_vdif_edv2_header_alma.__slots__ = [
    'seconds',
    'legacymode',
    'invalid',
    'frame',
    'epoch',
    'unassigned',
    'framelength8',
    'nchan',
    'version',
    'stationid',
    'threadid',
    'nbits',
    'iscomplex',
    'polblock',
    'quadrantminus1',
    'correlator',
    'subsubversion',
    'eversion',
    'picstatus',
    'psn',
]
struct_vdif_edv2_header_alma._fields_ = [
    ('seconds', c_uint32, 30),
    ('legacymode', c_uint32, 1),
    ('invalid', c_uint32, 1),
    ('frame', c_uint32, 24),
    ('epoch', c_uint32, 6),
    ('unassigned', c_uint32, 2),
    ('framelength8', c_uint32, 24),
    ('nchan', c_uint32, 5),
    ('version', c_uint32, 3),
    ('stationid', c_uint32, 16),
    ('threadid', c_uint32, 10),
    ('nbits', c_uint32, 5),
    ('iscomplex', c_uint32, 1),
    ('polblock', c_uint32, 1),
    ('quadrantminus1', c_uint32, 2),
    ('correlator', c_uint32, 1),
    ('subsubversion', c_uint32, 20),
    ('eversion', c_uint32, 8),
    ('picstatus', c_uint32),
    ('psn', c_uint64),
]

vdif_edv2_header_alma = struct_vdif_edv2_header_alma# difx/libraries/vdifio/src/vdifio.h: 165

# difx/libraries/vdifio/src/vdifio.h: 195
class struct_vdif_edv2_header_r2dbe(Structure):
    pass

struct_vdif_edv2_header_r2dbe.__slots__ = [
    'seconds',
    'legacymode',
    'invalid',
    'frame',
    'epoch',
    'unassigned',
    'framelength8',
    'nchan',
    'version',
    'stationid',
    'threadid',
    'nbits',
    'iscomplex',
    'polblock',
    'bdcsideband',
    'rxsideband',
    'undefined',
    'subsubversion',
    'eversion',
    'ppsdiff',
    'psn',
]
struct_vdif_edv2_header_r2dbe._fields_ = [
    ('seconds', c_uint32, 30),
    ('legacymode', c_uint32, 1),
    ('invalid', c_uint32, 1),
    ('frame', c_uint32, 24),
    ('epoch', c_uint32, 6),
    ('unassigned', c_uint32, 2),
    ('framelength8', c_uint32, 24),
    ('nchan', c_uint32, 5),
    ('version', c_uint32, 3),
    ('stationid', c_uint32, 16),
    ('threadid', c_uint32, 10),
    ('nbits', c_uint32, 5),
    ('iscomplex', c_uint32, 1),
    ('polblock', c_uint32, 1),
    ('bdcsideband', c_uint32, 1),
    ('rxsideband', c_uint32, 1),
    ('undefined', c_uint32, 1),
    ('subsubversion', c_uint32, 20),
    ('eversion', c_uint32, 8),
    ('ppsdiff', c_int32, 32),
    ('psn', c_uint64),
]

vdif_edv2_header_r2dbe = struct_vdif_edv2_header_r2dbe# difx/libraries/vdifio/src/vdifio.h: 195

# difx/libraries/vdifio/src/vdifio.h: 224
class struct_vdif_edv2_header(Structure):
    pass

struct_vdif_edv2_header.__slots__ = [
    'seconds',
    'legacymode',
    'invalid',
    'frame',
    'epoch',
    'unassigned',
    'framelength8',
    'nchan',
    'version',
    'stationid',
    'threadid',
    'nbits',
    'iscomplex',
    'polblock',
    'quadrantminus1',
    'correlator',
    'sync',
    'eversion',
    'status',
    'psn',
]
struct_vdif_edv2_header._fields_ = [
    ('seconds', c_uint32, 30),
    ('legacymode', c_uint32, 1),
    ('invalid', c_uint32, 1),
    ('frame', c_uint32, 24),
    ('epoch', c_uint32, 6),
    ('unassigned', c_uint32, 2),
    ('framelength8', c_uint32, 24),
    ('nchan', c_uint32, 5),
    ('version', c_uint32, 3),
    ('stationid', c_uint32, 16),
    ('threadid', c_uint32, 10),
    ('nbits', c_uint32, 5),
    ('iscomplex', c_uint32, 1),
    ('polblock', c_uint32, 1),
    ('quadrantminus1', c_uint32, 2),
    ('correlator', c_uint32, 1),
    ('sync', c_uint32, 20),
    ('eversion', c_uint32, 8),
    ('status', c_uint32),
    ('psn', c_uint64),
]

vdif_edv2_header = struct_vdif_edv2_header# difx/libraries/vdifio/src/vdifio.h: 224

# difx/libraries/vdifio/src/vdifio.h: 260
class struct_vdif_edv3_header(Structure):
    pass

struct_vdif_edv3_header.__slots__ = [
    'seconds',
    'legacymode',
    'invalid',
    'frame',
    'epoch',
    'unassigned',
    'framelength8',
    'nchan',
    'version',
    'stationid',
    'threadid',
    'nbits',
    'iscomplex',
    'samprate',
    'samprateunits',
    'eversion',
    'syncword',
    'tuning',
    'personalitytype',
    'minorrev',
    'majorrev',
    'sideband',
    'subband',
    'ifnumber',
    'dbeunit',
    'unassigned2',
]
struct_vdif_edv3_header._fields_ = [
    ('seconds', c_uint32, 30),
    ('legacymode', c_uint32, 1),
    ('invalid', c_uint32, 1),
    ('frame', c_uint32, 24),
    ('epoch', c_uint32, 6),
    ('unassigned', c_uint32, 2),
    ('framelength8', c_uint32, 24),
    ('nchan', c_uint32, 5),
    ('version', c_uint32, 3),
    ('stationid', c_uint32, 16),
    ('threadid', c_uint32, 10),
    ('nbits', c_uint32, 5),
    ('iscomplex', c_uint32, 1),
    ('samprate', c_uint32, 23),
    ('samprateunits', c_uint32, 1),
    ('eversion', c_uint32, 8),
    ('syncword', c_uint32),
    ('tuning', c_uint32),
    ('personalitytype', c_uint32, 8),
    ('minorrev', c_uint32, 4),
    ('majorrev', c_uint32, 4),
    ('sideband', c_uint32, 1),
    ('subband', c_uint32, 3),
    ('ifnumber', c_uint32, 4),
    ('dbeunit', c_uint32, 4),
    ('unassigned2', c_uint32, 4),
]

vdif_edv3_header = struct_vdif_edv3_header# difx/libraries/vdifio/src/vdifio.h: 260

# difx/libraries/vdifio/src/vdifio.h: 287
class struct_vdif_edv4_header(Structure):
    pass

struct_vdif_edv4_header.__slots__ = [
    'seconds',
    'legacymode',
    'invalid',
    'frame',
    'epoch',
    'unassigned',
    'framelength8',
    'nchan',
    'version',
    'stationid',
    'threadid',
    'nbits',
    'iscomplex',
    'dummy',
    'masklength',
    'eversion',
    'syncword',
    'validitymask',
]
struct_vdif_edv4_header._fields_ = [
    ('seconds', c_uint32, 30),
    ('legacymode', c_uint32, 1),
    ('invalid', c_uint32, 1),
    ('frame', c_uint32, 24),
    ('epoch', c_uint32, 6),
    ('unassigned', c_uint32, 2),
    ('framelength8', c_uint32, 24),
    ('nchan', c_uint32, 5),
    ('version', c_uint32, 3),
    ('stationid', c_uint32, 16),
    ('threadid', c_uint32, 10),
    ('nbits', c_uint32, 5),
    ('iscomplex', c_uint32, 1),
    ('dummy', c_uint32, 16),
    ('masklength', c_uint32, 8),
    ('eversion', c_uint32, 8),
    ('syncword', c_uint32),
    ('validitymask', c_uint64),
]

vdif_edv4_header = struct_vdif_edv4_header# difx/libraries/vdifio/src/vdifio.h: 287

enum_VDIFHeaderPrintLevel = c_int# difx/libraries/vdifio/src/vdifio.h: 289

VDIFHeaderPrintLevelHex = 0# difx/libraries/vdifio/src/vdifio.h: 289

VDIFHeaderPrintLevelColumns = (VDIFHeaderPrintLevelHex + 1)# difx/libraries/vdifio/src/vdifio.h: 289

VDIFHeaderPrintLevelShort = (VDIFHeaderPrintLevelColumns + 1)# difx/libraries/vdifio/src/vdifio.h: 289

VDIFHeaderPrintLevelLong = (VDIFHeaderPrintLevelShort + 1)# difx/libraries/vdifio/src/vdifio.h: 289

# difx/libraries/vdifio/src/vdifio.h: 298
if _libs["vdifio"].has("ymd2doy", "cdecl"):
    ymd2doy = _libs["vdifio"].get("ymd2doy", "cdecl")
    ymd2doy.argtypes = [c_int, c_int, c_int]
    ymd2doy.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 299
if _libs["vdifio"].has("ymd2mjd", "cdecl"):
    ymd2mjd = _libs["vdifio"].get("ymd2mjd", "cdecl")
    ymd2mjd.argtypes = [c_int, c_int, c_int]
    ymd2mjd.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 302
if _libs["vdifio"].has("createVDIFHeader", "cdecl"):
    createVDIFHeader = _libs["vdifio"].get("createVDIFHeader", "cdecl")
    createVDIFHeader.argtypes = [POINTER(vdif_header), c_int, c_int, c_int, c_int, c_int, c_char * int(3)]
    createVDIFHeader.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 311
if _libs["vdifio"].has("getVDIFFrameMJDSec", "cdecl"):
    getVDIFFrameMJDSec = _libs["vdifio"].get("getVDIFFrameMJDSec", "cdecl")
    getVDIFFrameMJDSec.argtypes = [POINTER(vdif_header)]
    getVDIFFrameMJDSec.restype = c_uint64

# difx/libraries/vdifio/src/vdifio.h: 312
if _libs["vdifio"].has("getVDIFFrameMJD", "cdecl"):
    getVDIFFrameMJD = _libs["vdifio"].get("getVDIFFrameMJD", "cdecl")
    getVDIFFrameMJD.argtypes = [POINTER(vdif_header)]
    getVDIFFrameMJD.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 313
if _libs["vdifio"].has("getVDIFFrameDMJD", "cdecl"):
    getVDIFFrameDMJD = _libs["vdifio"].get("getVDIFFrameDMJD", "cdecl")
    getVDIFFrameDMJD.argtypes = [POINTER(vdif_header), c_int]
    getVDIFFrameDMJD.restype = c_double

# difx/libraries/vdifio/src/vdifio.h: 318
if _libs["vdifio"].has("getVDIFNumChannels", "cdecl"):
    getVDIFNumChannels = _libs["vdifio"].get("getVDIFNumChannels", "cdecl")
    getVDIFNumChannels.argtypes = [POINTER(vdif_header)]
    getVDIFNumChannels.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 322
if _libs["vdifio"].has("getVDIFEpochMJD", "cdecl"):
    getVDIFEpochMJD = _libs["vdifio"].get("getVDIFEpochMJD", "cdecl")
    getVDIFEpochMJD.argtypes = [POINTER(vdif_header)]
    getVDIFEpochMJD.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 325
if _libs["vdifio"].has("setVDIFFrameMJD", "cdecl"):
    setVDIFFrameMJD = _libs["vdifio"].get("setVDIFFrameMJD", "cdecl")
    setVDIFFrameMJD.argtypes = [POINTER(vdif_header), c_int]
    setVDIFFrameMJD.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 326
if _libs["vdifio"].has("setVDIFFrameMJDSec", "cdecl"):
    setVDIFFrameMJDSec = _libs["vdifio"].get("setVDIFFrameMJDSec", "cdecl")
    setVDIFFrameMJDSec.argtypes = [POINTER(vdif_header), c_uint64]
    setVDIFFrameMJDSec.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 332
if _libs["vdifio"].has("setVDIFFrameBytes", "cdecl"):
    setVDIFFrameBytes = _libs["vdifio"].get("setVDIFFrameBytes", "cdecl")
    setVDIFFrameBytes.argtypes = [POINTER(vdif_header), c_int]
    setVDIFFrameBytes.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 333
if _libs["vdifio"].has("setVDIFFrameSecond", "cdecl"):
    setVDIFFrameSecond = _libs["vdifio"].get("setVDIFFrameSecond", "cdecl")
    setVDIFFrameSecond.argtypes = [POINTER(vdif_header), c_int]
    setVDIFFrameSecond.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 334
if _libs["vdifio"].has("setVDIFNumChannels", "cdecl"):
    setVDIFNumChannels = _libs["vdifio"].get("setVDIFNumChannels", "cdecl")
    setVDIFNumChannels.argtypes = [POINTER(vdif_header), c_int]
    setVDIFNumChannels.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 335
if _libs["vdifio"].has("setVDIFThreadID", "cdecl"):
    setVDIFThreadID = _libs["vdifio"].get("setVDIFThreadID", "cdecl")
    setVDIFThreadID.argtypes = [POINTER(vdif_header), c_int]
    setVDIFThreadID.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 336
if _libs["vdifio"].has("setVDIFFrameTime", "cdecl"):
    setVDIFFrameTime = _libs["vdifio"].get("setVDIFFrameTime", "cdecl")
    setVDIFFrameTime.argtypes = [POINTER(vdif_header), time_t]
    setVDIFFrameTime.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 337
if _libs["vdifio"].has("setVDIFEpochTime", "cdecl"):
    setVDIFEpochTime = _libs["vdifio"].get("setVDIFEpochTime", "cdecl")
    setVDIFEpochTime.argtypes = [POINTER(vdif_header), time_t]
    setVDIFEpochTime.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 339
if _libs["vdifio"].has("setVDIFEpochMJD", "cdecl"):
    setVDIFEpochMJD = _libs["vdifio"].get("setVDIFEpochMJD", "cdecl")
    setVDIFEpochMJD.argtypes = [POINTER(vdif_header), c_int]
    setVDIFEpochMJD.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 340
if _libs["vdifio"].has("nextVDIFHeader", "cdecl"):
    nextVDIFHeader = _libs["vdifio"].get("nextVDIFHeader", "cdecl")
    nextVDIFHeader.argtypes = [POINTER(vdif_header), c_int]
    nextVDIFHeader.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 341
if _libs["vdifio"].has("incrementVDIFHeader", "cdecl"):
    incrementVDIFHeader = _libs["vdifio"].get("incrementVDIFHeader", "cdecl")
    incrementVDIFHeader.argtypes = [POINTER(vdif_header), c_int, c_int64]
    incrementVDIFHeader.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 343
if _libs["vdifio"].has("fprintVDIFHeader", "cdecl"):
    fprintVDIFHeader = _libs["vdifio"].get("fprintVDIFHeader", "cdecl")
    fprintVDIFHeader.argtypes = [POINTER(FILE), POINTER(vdif_header), enum_VDIFHeaderPrintLevel]
    fprintVDIFHeader.restype = None

# difx/libraries/vdifio/src/vdifio.h: 344
if _libs["vdifio"].has("printVDIFHeader", "cdecl"):
    printVDIFHeader = _libs["vdifio"].get("printVDIFHeader", "cdecl")
    printVDIFHeader.argtypes = [POINTER(vdif_header), enum_VDIFHeaderPrintLevel]
    printVDIFHeader.restype = None

# difx/libraries/vdifio/src/vdifio.h: 349
if _libs["vdifio"].has("determinevdifframesize", "cdecl"):
    determinevdifframesize = _libs["vdifio"].get("determinevdifframesize", "cdecl")
    determinevdifframesize.argtypes = [POINTER(c_ubyte), c_int]
    determinevdifframesize.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 351
if _libs["vdifio"].has("determinevdifframeoffset", "cdecl"):
    determinevdifframeoffset = _libs["vdifio"].get("determinevdifframeoffset", "cdecl")
    determinevdifframeoffset.argtypes = [POINTER(c_ubyte), c_int, c_int]
    determinevdifframeoffset.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 356
if _libs["vdifio"].has("getCornerTurner", "cdecl"):
    getCornerTurner = _libs["vdifio"].get("getCornerTurner", "cdecl")
    getCornerTurner.argtypes = [c_int, c_int]
    getCornerTurner.restype = POINTER(CFUNCTYPE(UNCHECKED(None), POINTER(c_ubyte), POINTER(POINTER(c_ubyte)), c_int))

# difx/libraries/vdifio/src/vdifio.h: 370
class struct_vdif_mux(Structure):
    pass

struct_vdif_mux.__slots__ = [
    'inputFrameSize',
    'inputDataSize',
    'outputFrameSize',
    'outputDataSize',
    'inputFramesPerSecond',
    'inputChannelsPerThread',
    'bitsPerSample',
    'nThread',
    'nSort',
    'nGap',
    'frameGranularity',
    'nOutputChan',
    'complexFactor',
    'fanoutFactor',
    'flags',
    'chanIndex',
    'goodMask',
    'cornerTurner',
]
struct_vdif_mux._fields_ = [
    ('inputFrameSize', c_int),
    ('inputDataSize', c_int),
    ('outputFrameSize', c_int),
    ('outputDataSize', c_int),
    ('inputFramesPerSecond', c_int),
    ('inputChannelsPerThread', c_int),
    ('bitsPerSample', c_int),
    ('nThread', c_int),
    ('nSort', c_int),
    ('nGap', c_int),
    ('frameGranularity', c_int),
    ('nOutputChan', c_int),
    ('complexFactor', c_int),
    ('fanoutFactor', c_int),
    ('flags', c_uint),
    ('chanIndex', c_uint16 * int((1023 + 1))),
    ('goodMask', c_uint64),
    ('cornerTurner', CFUNCTYPE(UNCHECKED(None), POINTER(c_ubyte), POINTER(POINTER(c_ubyte)), c_int)),
]

# difx/libraries/vdifio/src/vdifio.h: 391
class struct_vdif_mux_statistics(Structure):
    pass

struct_vdif_mux_statistics.__slots__ = [
    'nValidFrame',
    'nInvalidFrame',
    'nDiscardedFrame',
    'nWrongThread',
    'nSkippedByte',
    'nFillByte',
    'nDuplicateFrame',
    'bytesProcessed',
    'nGoodFrame',
    'nPartialFrame',
    'nFillerFrame',
    'nCall',
    'nOutOfDataConditions',
    'srcSize',
    'srcUsed',
    'destSize',
    'destUsed',
    'inputFrameSize',
    'outputFrameSize',
    'outputFrameGranularity',
    'outputFramesPerSecond',
    'nOutputFrame',
    'epoch',
    'startFrameNumber',
]
struct_vdif_mux_statistics._fields_ = [
    ('nValidFrame', c_longlong),
    ('nInvalidFrame', c_longlong),
    ('nDiscardedFrame', c_longlong),
    ('nWrongThread', c_longlong),
    ('nSkippedByte', c_longlong),
    ('nFillByte', c_longlong),
    ('nDuplicateFrame', c_longlong),
    ('bytesProcessed', c_longlong),
    ('nGoodFrame', c_longlong),
    ('nPartialFrame', c_longlong),
    ('nFillerFrame', c_longlong),
    ('nCall', c_int),
    ('nOutOfDataConditions', c_int),
    ('srcSize', c_int),
    ('srcUsed', c_int),
    ('destSize', c_int),
    ('destUsed', c_int),
    ('inputFrameSize', c_int),
    ('outputFrameSize', c_int),
    ('outputFrameGranularity', c_int),
    ('outputFramesPerSecond', c_int),
    ('nOutputFrame', c_int),
    ('epoch', c_int),
    ('startFrameNumber', c_int64),
]

# difx/libraries/vdifio/src/vdifio.h: 425
if _libs["vdifio"].has("configurevdifmux", "cdecl"):
    configurevdifmux = _libs["vdifio"].get("configurevdifmux", "cdecl")
    configurevdifmux.argtypes = [POINTER(struct_vdif_mux), c_int, c_int, c_int, c_int, POINTER(c_int), c_int, c_int, c_int]
    configurevdifmux.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 427
if _libs["vdifio"].has("setvdifmuxinputchannels", "cdecl"):
    setvdifmuxinputchannels = _libs["vdifio"].get("setvdifmuxinputchannels", "cdecl")
    setvdifmuxinputchannels.argtypes = [POINTER(struct_vdif_mux), c_int]
    setvdifmuxinputchannels.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 428
if _libs["vdifio"].has("setvdifmuxfanoutfactor", "cdecl"):
    setvdifmuxfanoutfactor = _libs["vdifio"].get("setvdifmuxfanoutfactor", "cdecl")
    setvdifmuxfanoutfactor.argtypes = [POINTER(struct_vdif_mux), c_int]
    setvdifmuxfanoutfactor.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 430
if _libs["vdifio"].has("printvdifmux", "cdecl"):
    printvdifmux = _libs["vdifio"].get("printvdifmux", "cdecl")
    printvdifmux.argtypes = [POINTER(struct_vdif_mux)]
    printvdifmux.restype = None

# difx/libraries/vdifio/src/vdifio.h: 432
if _libs["vdifio"].has("vdifmux", "cdecl"):
    vdifmux = _libs["vdifio"].get("vdifmux", "cdecl")
    vdifmux.argtypes = [POINTER(c_ubyte), c_int, POINTER(c_ubyte), c_int, POINTER(struct_vdif_mux), c_int64, POINTER(struct_vdif_mux_statistics)]
    vdifmux.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 434
if _libs["vdifio"].has("printvdifmuxstatistics", "cdecl"):
    printvdifmuxstatistics = _libs["vdifio"].get("printvdifmuxstatistics", "cdecl")
    printvdifmuxstatistics.argtypes = [POINTER(struct_vdif_mux_statistics)]
    printvdifmuxstatistics.restype = None

# difx/libraries/vdifio/src/vdifio.h: 436
if _libs["vdifio"].has("resetvdifmuxstatistics", "cdecl"):
    resetvdifmuxstatistics = _libs["vdifio"].get("resetvdifmuxstatistics", "cdecl")
    resetvdifmuxstatistics.argtypes = [POINTER(struct_vdif_mux_statistics)]
    resetvdifmuxstatistics.restype = None

# difx/libraries/vdifio/src/vdifio.h: 438
if _libs["vdifio"].has("testvdifcornerturners", "cdecl"):
    testvdifcornerturners = _libs["vdifio"].get("testvdifcornerturners", "cdecl")
    testvdifcornerturners.argtypes = [c_int, c_int]
    testvdifcornerturners.restype = None

# difx/libraries/vdifio/src/vdifio.h: 443
class struct_vdif_file_summary(Structure):
    pass

struct_vdif_file_summary.__slots__ = [
    'fileName',
    'fileSize',
    'nBit',
    'nThread',
    'threadIds',
    'frameSize',
    'framesPerSecond',
    'epoch',
    'startSecond',
    'startFrame',
    'endSecond',
    'endFrame',
    'firstFrameOffset',
]
struct_vdif_file_summary._fields_ = [
    ('fileName', c_char * int(256)),
    ('fileSize', c_longlong),
    ('nBit', c_int),
    ('nThread', c_int),
    ('threadIds', c_int * int(64)),
    ('frameSize', c_int),
    ('framesPerSecond', c_int),
    ('epoch', c_int),
    ('startSecond', c_int),
    ('startFrame', c_int),
    ('endSecond', c_int),
    ('endFrame', c_int),
    ('firstFrameOffset', c_int),
]

# difx/libraries/vdifio/src/vdifio.h: 459
if _libs["vdifio"].has("resetvdiffilesummary", "cdecl"):
    resetvdiffilesummary = _libs["vdifio"].get("resetvdiffilesummary", "cdecl")
    resetvdiffilesummary.argtypes = [POINTER(struct_vdif_file_summary)]
    resetvdiffilesummary.restype = None

# difx/libraries/vdifio/src/vdifio.h: 461
if _libs["vdifio"].has("printvdiffilesummary", "cdecl"):
    printvdiffilesummary = _libs["vdifio"].get("printvdiffilesummary", "cdecl")
    printvdiffilesummary.argtypes = [POINTER(struct_vdif_file_summary)]
    printvdiffilesummary.restype = None

# difx/libraries/vdifio/src/vdifio.h: 463
if _libs["vdifio"].has("snprintvdiffilesummary", "cdecl"):
    snprintvdiffilesummary = _libs["vdifio"].get("snprintvdiffilesummary", "cdecl")
    snprintvdiffilesummary.argtypes = [String, c_int, POINTER(struct_vdif_file_summary)]
    snprintvdiffilesummary.restype = None

# difx/libraries/vdifio/src/vdifio.h: 480
if _libs["vdifio"].has("vdiffilesummarygetstartmjd", "cdecl"):
    vdiffilesummarygetstartmjd = _libs["vdifio"].get("vdiffilesummarygetstartmjd", "cdecl")
    vdiffilesummarygetstartmjd.argtypes = [POINTER(struct_vdif_file_summary)]
    vdiffilesummarygetstartmjd.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 484
if _libs["vdifio"].has("summarizevdiffile", "cdecl"):
    summarizevdiffile = _libs["vdifio"].get("summarizevdiffile", "cdecl")
    summarizevdiffile.argtypes = [POINTER(struct_vdif_file_summary), String, c_int]
    summarizevdiffile.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 501
class struct_vdif_file_reader(Structure):
    pass

struct_vdif_file_reader.__slots__ = [
    'details',
    'fd',
    'frame',
    'sec',
    'head',
    'desynched',
    'feof',
    'eof',
    'fps',
    'firstframeoffset',
    'offset',
    'tail',
]
struct_vdif_file_reader._fields_ = [
    ('details', struct_vdif_file_summary),
    ('fd', POINTER(FILE) * int(64)),
    ('frame', uint_fast32_t * int(64)),
    ('sec', uint_fast32_t * int(64)),
    ('head', off_t * int(64)),
    ('desynched', c_int * int(64)),
    ('feof', c_int * int(64)),
    ('eof', c_int),
    ('fps', uint_fast32_t),
    ('firstframeoffset', off_t),
    ('offset', off_t),
    ('tail', off_t),
]

# difx/libraries/vdifio/src/vdifio.h: 516
class struct_vdif_file_reader_stats(Structure):
    pass

struct_vdif_file_reader_stats.__slots__ = [
    'nThread',
    'threadOffsets',
    'maxOffset',
]
struct_vdif_file_reader_stats._fields_ = [
    ('nThread', c_int),
    ('threadOffsets', off_t * int(64)),
    ('maxOffset', off_t),
]

# difx/libraries/vdifio/src/vdifio.h: 523
if _libs["vdifio"].has("vdifreaderOpen", "cdecl"):
    vdifreaderOpen = _libs["vdifio"].get("vdifreaderOpen", "cdecl")
    vdifreaderOpen.argtypes = [POINTER(struct_vdif_file_summary), POINTER(struct_vdif_file_reader)]
    vdifreaderOpen.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 526
if _libs["vdifio"].has("vdifreaderRead", "cdecl"):
    vdifreaderRead = _libs["vdifio"].get("vdifreaderRead", "cdecl")
    vdifreaderRead.argtypes = [POINTER(struct_vdif_file_reader), POINTER(None), c_size_t]
    vdifreaderRead.restype = c_size_t

# difx/libraries/vdifio/src/vdifio.h: 529
if _libs["vdifio"].has("vdifreaderSeek", "cdecl"):
    vdifreaderSeek = _libs["vdifio"].get("vdifreaderSeek", "cdecl")
    vdifreaderSeek.argtypes = [POINTER(struct_vdif_file_reader), c_size_t]
    vdifreaderSeek.restype = c_size_t

# difx/libraries/vdifio/src/vdifio.h: 532
if _libs["vdifio"].has("vdifreaderStats", "cdecl"):
    vdifreaderStats = _libs["vdifio"].get("vdifreaderStats", "cdecl")
    vdifreaderStats.argtypes = [POINTER(struct_vdif_file_reader), POINTER(struct_vdif_file_reader_stats)]
    vdifreaderStats.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 535
if _libs["vdifio"].has("vdifreaderClose", "cdecl"):
    vdifreaderClose = _libs["vdifio"].get("vdifreaderClose", "cdecl")
    vdifreaderClose.argtypes = [POINTER(struct_vdif_file_reader)]
    vdifreaderClose.restype = c_int

# difx/libraries/vdifio/src/vdifio.h: 42
try:
    VDIF_HEADER_BYTES = 32
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 43
try:
    VDIF_LEGACY_HEADER_BYTES = 16
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 44
try:
    MAX_VDIF_FRAME_BYTES = 9032
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 45
try:
    VDIF_MAX_THREAD_ID = 1023
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 47
try:
    VDIF_SUMMARY_MAX_THREADS = 64
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 48
try:
    VDIF_SUMMARY_FILE_LENGTH = 256
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 50
try:
    VDIF_NOERROR = 0
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 51
try:
    VDIF_ERROR = 1
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 53
try:
    VDIF_EDV2_SUBVER_ALMA = 678629
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 54
try:
    VDIF_EDV2_SUBVER_R2DBE = 0
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 361
try:
    VDIF_MUX_FLAG_GOTOEND = 1
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 362
try:
    VDIF_MUX_FLAG_RESPECTGRANULARITY = 2
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 363
try:
    VDIF_MUX_FLAG_ENABLEVALIDITY = 4
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 364
try:
    VDIF_MUX_FLAG_INPUTLEGACY = 8
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 365
try:
    VDIF_MUX_FLAG_OUTPUTLEGACY = 16
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 366
try:
    VDIF_MUX_FLAG_COMPLEX = 32
except:
    pass

# difx/libraries/vdifio/src/vdifio.h: 367
try:
    VDIF_MUX_FLAG_PROPAGATEVALIDITY = 64
except:
    pass

vdif_header = struct_vdif_header# difx/libraries/vdifio/src/vdifio.h: 82

vdif_edv1_header = struct_vdif_edv1_header# difx/libraries/vdifio/src/vdifio.h: 109

vdif_edv2_header_generic = struct_vdif_edv2_header_generic# difx/libraries/vdifio/src/vdifio.h: 136

vdif_edv2_header_alma = struct_vdif_edv2_header_alma# difx/libraries/vdifio/src/vdifio.h: 165

vdif_edv2_header_r2dbe = struct_vdif_edv2_header_r2dbe# difx/libraries/vdifio/src/vdifio.h: 195

vdif_edv2_header = struct_vdif_edv2_header# difx/libraries/vdifio/src/vdifio.h: 224

vdif_edv3_header = struct_vdif_edv3_header# difx/libraries/vdifio/src/vdifio.h: 260

vdif_edv4_header = struct_vdif_edv4_header# difx/libraries/vdifio/src/vdifio.h: 287

vdif_mux = struct_vdif_mux# difx/libraries/vdifio/src/vdifio.h: 370

vdif_mux_statistics = struct_vdif_mux_statistics# difx/libraries/vdifio/src/vdifio.h: 391

vdif_file_summary = struct_vdif_file_summary# difx/libraries/vdifio/src/vdifio.h: 443

vdif_file_reader = struct_vdif_file_reader# difx/libraries/vdifio/src/vdifio.h: 501

vdif_file_reader_stats = struct_vdif_file_reader_stats# difx/libraries/vdifio/src/vdifio.h: 516

# No inserted files

# No prefix-stripping

