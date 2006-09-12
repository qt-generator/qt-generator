######################################################################
# Automatically generated by qmake (2.00a) ti 25. okt 19:06:46 2005
######################################################################

TEMPLATE = app
TARGET +=
DEPENDPATH += . tests
mac:CONFIG -= app_bundle
INCLUDEPATH += .
INCLUDEPATH += ../common

CONFIG += console
RESOURCES += generator.qrc

include(parser/rxx.pri)

include(parser/rpp/rpp.pri)

win32-msvc2005:{
        QMAKE_CXXFLAGS += -wd4996
        QMAKE_CFLAGS += -wd4996
}

# Input
HEADERS += \
	cppgenerator.h \
	cppimplgenerator.h \
	customtypes.h \
	javagenerator.h \
	juicdatagenerator.h \
	reporthandler.h \
	typeparser.h \
        classlistgenerator.h \
        cppheadergenerator.h \
        generator.h \
        main.h \
        metainfogenerator.h \
        metajava.h \
        metajavabuilder.h \
        typesystem.h \
        qdocgenerator.h \

SOURCES += \
	cppgenerator.cpp \
	cppimplgenerator.cpp \
	customtypes.cpp \
	juicdatagenerator.cpp \
	main.cpp \
	reporthandler.cpp \
	typeparser.cpp \
	typesystem.cpp \
        classlistgenerator.cpp \
        cppheadergenerator.cpp \
        generator.cpp \
        javagenerator.cpp \
        metainfogenerator.cpp \
        metajava.cpp \
        metajavabuilder.cpp \
        qdocgenerator.cpp \

QT = core xml

win32{
	PRECOMPILED_HEADER = generator_pch.h
	CONFIG += precompile_header
}

mac {
    CONFIG += x86 ppc
    CONFIG -= precompile_header
}
