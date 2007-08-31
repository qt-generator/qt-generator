isEmpty(GENERATORPATH):GENERATORPATH = $$PWD
INCLUDEPATH += $$GENERATORPATH

TEMPLATE = app
TARGET +=
DEPENDPATH += $$GENERATORPATH tests
mac:CONFIG -= app_bundle
INCLUDEPATH += $$GENERATORPATH/.
INCLUDEPATH += $$GENERATORPATH/../common

unix:CONFIG += debug_and_release

CONFIG += console
RESOURCES += generator.qrc

include($$GENERATORPATH/parser/rxx.pri)

include($$GENERATORPATH/parser/rpp/rpp.pri)

win32-msvc2005:{
        QMAKE_CXXFLAGS += -wd4996
        QMAKE_CFLAGS += -wd4996
}

# Input
HEADERS += \
        $$GENERATORPATH/generator.h \
        $$GENERATORPATH/main.h \
        $$GENERATORPATH/reporthandler.h \
        $$GENERATORPATH/typeparser.h \
        $$GENERATORPATH/typesystem.h \
        $$GENERATORPATH/asttoxml.h \
        $$GENERATORPATH/fileout.h \
        $$GENERATORPATH/generatorset.h \
        $$GENERATORPATH/metajava.h \
        $$GENERATORPATH/customtypes.h \

      
SOURCES += \
        $$GENERATORPATH/generator.cpp \
        $$GENERATORPATH/main.cpp \
        $$GENERATORPATH/reporthandler.cpp \
        $$GENERATORPATH/typeparser.cpp \
        $$GENERATORPATH/typesystem.cpp \
        $$GENERATORPATH/asttoxml.cpp \
        $$GENERATORPATH/fileout.cpp \
        $$GENERATORPATH/generatorset.cpp \
        $$GENERATORPATH/metajava.cpp \
        $$GENERATORPATH/customtypes.cpp \

   
QT = core xml

win32{
        PRECOMPILED_HEADER = generator_pch.h
        CONFIG += precompile_header
}

win32-msvc.net {
        QMAKE_CXXFLAGS += /Zm500
        QMAKE_CXXFLAGS -= -Zm200
        QMAKE_CFLAGS -= -Zm200
}

mac {
    contains(QT_CONFIG, x86):contains(QT_CONFIG, ppc):CONFIG += x86 ppc
    CONFIG -= precompile_header
}
