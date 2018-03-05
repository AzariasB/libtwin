QT += testlib
QT -= gui

CONFIG += qt console warn_on depend_includepath testcase c++17
CONFIG -= app_bundle

TEMPLATE = app

HEADERS += ../libtwing/twin.hpp

SOURCES +=  tst_twintest.cpp
