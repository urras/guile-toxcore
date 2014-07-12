dnl -*- Autoconf -*-

AC_INIT(guile-toxcore, 0.1.0)
AC_CONFIG_SRCDIR(tox)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

GUILE_PROGS

if $GUILE_TOOLS | grep -q compile; then
   # everything cool
   true
else
   AC_MSG_ERROR([Guile 2.0 required.])
fi

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([env], [chmod +x env])

PKG_CHECK_MODULES([GUILE], [guile-2.0 >= 2.0.5])
PKG_CHECK_MODULES([TOXCORE], [libtoxcore])

AC_OUTPUT
