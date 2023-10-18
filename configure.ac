dnl Process this file with autoconf to produce a configure script.

AC_INIT([chess-shell], [0.1], [gaiusmod2@gmail.com])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_CONFIG_SRCDIR([version.c])

AC_PROG_AWK
AC_PROG_CPP
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_PATH_PROG(GM2, gm2, /usr/bin/gm2, $PATH:/usr/bin)

AC_ARG_ENABLE([multiprocessor],
[  --enable-multiprocessor   Turn on building chess shell to run on multiple processors.],
[case "${enableval}" in
    yes) multi=true ;;
    no)  multi=false ;;
    *)   AC_MSG_ERROR([bad value ${enableval} for --enable-multiprocessor]) ;;
 esac],[multi=false])

AM_CONDITIONAL([MULTIPROCESSOR], [test x$multi = xtrue])

if test x$multi = xtrue; then
    AC_MSG_RESULT([multiprocessor ... yes])
    AC_SUBST(MULTIPROCESSOR,[TRUE])
    AC_DEFINE([MULTIPROCESSOR], [], [is multiprocessor runtime enabled?])
else
    AC_MSG_RESULT([multiprocessor ... no])
    AC_SUBST(MULTIPROCESSOR,[FALSE],[])
fi

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([
 Makefile
 m2config.def
 debug
])

AC_OUTPUT
