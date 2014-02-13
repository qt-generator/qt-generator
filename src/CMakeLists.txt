PROJECT(qt_generator CXX C)
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
FIND_PACKAGE(Qt4 REQUIRED)

SET(qt_generator_SOURCES
    abstractmetabuilder.cpp
    abstractmetalang.cpp
    asttoxml.cpp
    classlistgenerator.cpp
    cppgenerator.cpp
    cppheadergenerator.cpp
    cppimplgenerator.cpp
    customtypes.cpp
    docparser.cpp
    fileout.cpp
    generator.cpp
    generatorset.cpp
    generatorsetjava.cpp
    javagenerator.cpp
    jumptable.cpp
    main.cpp
    metainfogenerator.cpp
    metajava.cpp
    metajavabuilder.cpp
    preprocesshandler.cpp
    prigenerator.cpp
    qdocgenerator.cpp
    reporthandler.cpp
    typeparser.cpp
    uiconverter.cpp
    util.cpp
    wrapper.cpp

    parser/ast.cpp
    parser/binder.cpp
    parser/class_compiler.cpp
    parser/codemodel.cpp
    parser/codemodel_finder.cpp
    parser/compiler_utils.cpp
    parser/control.cpp
    parser/declarator_compiler.cpp
    parser/default_visitor.cpp
    parser/dumptree.cpp
    parser/lexer.cpp
    parser/list.cpp
    parser/name_compiler.cpp
    parser/parser.cpp
    parser/smallobject.cpp
    parser/tokens.cpp
    parser/type_compiler.cpp
    parser/visitor.cpp

    parser/rpp/builtin-macros.cpp
    parser/rpp/pp-engine-bits.cpp
    parser/rpp/pp-environment.cpp
    parser/rpp/pp-macro-expander.cpp
    parser/rpp/preprocessor.cpp

    typesystem/codesnip.cpp
    typesystem/handler.cpp
    typesystem/modification.cpp
    typesystem/typedatabase.cpp
    typesystem/typeentry.cpp
    typesystem/typesystem.cpp
)

SET(qt_generator_HEADERS
    cppgenerator.h
    cppheadergenerator.h
    cppimplgenerator.h
    fileout.h
    generator.h
    generatorset.h
    generatorsetjava.h
    javagenerator.h
    jumptable.h
    prigenerator.h
)

SET(qt_generator_RESOURCES
    generator.qrc
)

ADD_DEFINITIONS(-DRXX_ALLOCATOR_INIT_0)

SET(QT_USE_QTMAIN true)
SET(QT_USE_QTGUI false)
SET(QT_USE_QTXML true)

INCLUDE(${QT_USE_FILE})
ADD_DEFINITIONS(${QT_DEFINITIONS})

QT4_WRAP_CPP(qt_generator_HEADERS_MOC ${qt_generator_HEADERS})
QT4_ADD_RESOURCES(qt_generator_RESOURCES_RCC ${qt_generator_RESOURCES})

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}
                    ${CMAKE_CURRENT_SOURCE_DIR}/parser
                    ${CMAKE_CURRENT_SOURCE_DIR}/parser/rpp)

ADD_EXECUTABLE(qt-generator
               ${qt_generator_SOURCES}
               ${qt_generator_HEADERS_MOC}
               ${qt_generator_RESOURCES_RCC})
TARGET_LINK_LIBRARIES(qt-generator ${QT_LIBRARIES})
