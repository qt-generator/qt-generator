PROJECT(qt_generator CXX C)
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
FIND_PACKAGE(Qt4 REQUIRED)

OPTION(GENERATOR_LANG "Which generator to use." OFF)
IF(GENERATOR_LANG STREQUAL "DYLAN")
    ADD_DEFINITIONS(-DLANG_DYLAN)
    SET(LANG dylan)
ELSE()
    ADD_DEFINITIONS(-DLANG_JAVA)
    SET(LANG java)
ENDIF()

SET(qt_generator_SOURCES
    abstractmetabuilder.cpp
    abstractmetalang.cpp
    asttoxml.cpp
    customtypes.cpp
    docparser.cpp
    fileout.cpp
    generator.cpp
    generatorset.cpp
    main.cpp
    preprocesshandler.cpp
    prigenerator.cpp
    reporthandler.cpp
    typeparser.cpp
    uiconverter.cpp
    util.cpp
    wrapper.cpp

    lang-dylan/dylangenerator.cpp
    lang-dylan/generatorsetdylan.cpp
    lang-dylan/librarygenerator.cpp
    lang-dylan/lidgenerator.cpp
    lang-dylan/metadylan.cpp
    lang-dylan/metadylanbuilder.cpp
    lang-dylan/plaincppgenerator.cpp
    lang-dylan/plaincppheadergenerator.cpp
    lang-dylan/plaincppimplgenerator.cpp

    lang-java/classlistgenerator.cpp
    lang-java/cppgenerator.cpp
    lang-java/cppheadergenerator.cpp
    lang-java/cppimplgenerator.cpp
    lang-java/generatorsetjava.cpp
    lang-java/javagenerator.cpp
    lang-java/jumptable.cpp
    lang-java/metainfogenerator.cpp
    lang-java/metajava.cpp
    lang-java/metajavabuilder.cpp
    lang-java/qdocgenerator.cpp

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
    fileout.h
    generator.h
    generatorset.h
    prigenerator.h

    lang-dylan/dylangenerator.h
    lang-dylan/generatorsetdylan.h
    lang-dylan/librarygenerator.h
    lang-dylan/lidgenerator.h
    lang-dylan/plaincppgenerator.h
    lang-dylan/plaincppheadergenerator.h
    lang-dylan/plaincppimplgenerator.h

    lang-java/cppgenerator.h
    lang-java/cppheadergenerator.h
    lang-java/cppimplgenerator.h
    lang-java/generatorsetjava.h
    lang-java/javagenerator.h
    lang-java/jumptable.h
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

SET(qt_generator_TYPESYSTEMS
    typesystem_core
    typesystem_dbus
    typesystem_declarative
    typesystem_designer
    typesystem_gui
    typesystem_help
    typesystem_multimedia
    typesystem_network
    typesystem_opengl
    typesystem_phonon
    typesystem_script
    typesystem_scripttools
    typesystem_sql
    typesystem_svg
    typesystem_test
    typesystem_webkit
    typesystem_xml
    typesystem_xmlpatterns
)
SET(generated_xml_files)
FOREACH(typesystem ${qt_generator_TYPESYSTEMS})
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_BINARY_DIR}/${typesystem}.xml
    COMMAND xsltproc
              -o ${CMAKE_BINARY_DIR}/${typesystem}.xml
              --stringparam source ${CMAKE_CURRENT_SOURCE_DIR}/typesystem/xml/${typesystem}-${LANG}.xml
              ${CMAKE_CURRENT_SOURCE_DIR}/typesystem/xml/merge.xsl
              ${CMAKE_CURRENT_SOURCE_DIR}/typesystem/xml/${typesystem}-common.xml
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/typesystem/xml/${typesystem}-${LANG}.xml
            ${CMAKE_CURRENT_SOURCE_DIR}/typesystem/xml/${typesystem}-common.xml
            ${CMAKE_CURRENT_SOURCE_DIR}/typesystem/xml/merge.xsl
  )
  SET(generated_xml_files ${generated_xml_files} ${CMAKE_BINARY_DIR}/${typesystem}.xml)
ENDFOREACH()

ADD_CUSTOM_TARGET(generate-merged-xml
                  DEPENDS ${generated_xml_files})
ADD_DEPENDENCIES(qt-generator generate-merged-xml)
