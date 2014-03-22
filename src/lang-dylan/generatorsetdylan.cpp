/****************************************************************************
**
** Copyright (C) 1992-2009 Nokia. All rights reserved.
**
** This file is part of Qt Jambi.
**
** ** $BEGIN_LICENSE$
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Nokia gives you certain
** additional rights. These rights are described in the Nokia Qt LGPL
** Exception version 1.0, included in the file LGPL_EXCEPTION.txt in this
** package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
**
** $END_LICENSE$
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/

#include "generatorsetdylan.h"
#include "reporthandler.h"

#include "dylangenerator.h"
#include "cppheadergenerator.h"
#include "cppimplgenerator.h"

#include <QFileInfo>

GeneratorSet *GeneratorSetDylan::getInstance() {
    return new GeneratorSetDylan();
}

void dumpMetaDylanTree(const AbstractMetaClassList &classes);

GeneratorSetDylan::GeneratorSetDylan() {
}

QString GeneratorSetDylan::usage() {
    QString usage;
    return usage;
}

bool GeneratorSetDylan::readParameters(const QMap<QString, QString> args) {
    return GeneratorSet::readParameters(args);
}

void GeneratorSetDylan::buildModel(const QString pp_file) {
    builder.setFileName(pp_file);
    if (!outDir.isNull())
        builder.setOutputDirectory(outDir);
    builder.build();
}

void GeneratorSetDylan::dumpObjectTree() {
    dumpMetaDylanTree(builder.classes());
}

QString GeneratorSetDylan::generate() {

    // Code generation
    QList<Generator *> generators;
    PriGenerator *priGenerator = new PriGenerator;
    DylanGenerator *dylan_generator = 0;
    CppHeaderGenerator *cpp_header_generator = 0;
    CppImplGenerator *cpp_impl_generator = 0;

    QStringList contexts;

    dylan_generator = new DylanGenerator;
    if (!javaOutDir.isNull())
        dylan_generator->setDylanOutputDirectory(javaOutDir);
    if (!outDir.isNull())
        dylan_generator->setLogOutputDirectory(outDir);
    generators << dylan_generator;

    contexts << "DylanGenerator";

    cpp_header_generator = new CppHeaderGenerator(priGenerator);
    if (!cppOutDir.isNull())
        cpp_header_generator->setCppOutputDirectory(cppOutDir);
    generators << cpp_header_generator;
    contexts << "CppHeaderGenerator";

    cpp_impl_generator = new CppImplGenerator(priGenerator);
    if (!cppOutDir.isNull())
        cpp_impl_generator->setCppOutputDirectory(cppOutDir);
    generators << cpp_impl_generator;
    contexts << "CppImplGenerator";

    for (int i = 0; i < generators.size(); ++i) {
        Generator *generator = generators.at(i);
        ReportHandler::setContext(contexts.at(i));

        if (generator->outputDirectory().isNull())
            generator->setOutputDirectory(outDir);
        generator->setClasses(builder.classes());
        if (printStdout)
            generator->printClasses();
        else
            generator->generate();
    }

    QString res;
    res = QString("Classes in typesystem: %1\n"
                  "Generated:\n"
                  "  - dylan.....: %2 (%3)\n"
                  "  - cpp-impl..: %4 (%5)\n"
                  "  - cpp-h.....: %6 (%7)\n"
                 )
          .arg(builder.classes().size())
          .arg(dylan_generator ? dylan_generator->numGenerated() : 0)
          .arg(dylan_generator ? dylan_generator->numGeneratedAndWritten() : 0)
          .arg(cpp_impl_generator ? cpp_impl_generator->numGenerated() : 0)
          .arg(cpp_impl_generator ? cpp_impl_generator->numGeneratedAndWritten() : 0)
          .arg(cpp_header_generator ? cpp_header_generator->numGenerated() : 0)
          .arg(cpp_header_generator ? cpp_header_generator->numGeneratedAndWritten() : 0);

    return res;
}

void dumpMetaDylanAttributes(const AbstractMetaAttributes *attr) {
    if (attr->isNative()) printf(" native");
    if (attr->isAbstract()) printf(" abstract");
    if (attr->isFinalInTargetLang()) printf(" final(java)");
    if (attr->isFinalInCpp()) printf(" final(cpp)");
    if (attr->isStatic()) printf(" static");
    if (attr->isPrivate()) printf(" private");
    if (attr->isProtected()) printf(" protected");
    if (attr->isPublic()) printf(" public");
    if (attr->isFriendly()) printf(" friendly");
}

void dumpMetaDylanType(const AbstractMetaType *type) {
    if (!type) {
        printf("[void]");
    } else {
        printf("[type: %s", qPrintable(type->typeEntry()->qualifiedCppName()));
        if (type->isReference()) printf(" &");
        int indirections = type->indirections();
        if (indirections) printf(" %s", qPrintable(QString(indirections, '*')));

        printf(", %s", qPrintable(type->typeEntry()->qualifiedTargetLangName()));

        if (type->isPrimitive()) printf(" primitive");
        if (type->isEnum()) printf(" enum");
        if (type->isQObject()) printf(" q_obj");
        if (type->isNativePointer()) printf(" n_ptr");
        if (type->isTargetLangString()) printf(" java_string");
        if (type->isTargetLangStringRef()) printf(" java_string");
        if (type->isConstant()) printf(" const");
        printf("]");
    }
}

void dumpMetaDylanArgument(const AbstractMetaArgument *arg) {
    printf("        ");
    dumpMetaDylanType(arg->type());
    printf(" %s", qPrintable(arg->argumentName()));
    if (!arg->defaultValueExpression().isEmpty())
        printf(" = %s", qPrintable(arg->defaultValueExpression()));
    printf("\n");
}

void dumpMetaDylanFunction(const AbstractMetaFunction *func) {
    printf("    %s() - ", qPrintable(func->name()));
    dumpMetaDylanType(func->type());
    dumpMetaDylanAttributes(func);

    // Extra attributes...
    if (func->isSignal()) printf(" signal");
    if (func->isSlot()) printf(" slot");
    if (func->isConstant()) printf(" const");

    printf("\n      arguments:\n");
    foreach(AbstractMetaArgument *arg, func->arguments())
        dumpMetaDylanArgument(arg);
}

void dumpMetaDylanClass(const AbstractMetaClass *cls) {
    printf("\nclass: %s, package: %s\n", qPrintable(cls->name()), qPrintable(cls->package()));
    if (cls->hasVirtualFunctions())
        printf("    shell based\n");
    printf("  baseclass: %s %s\n", qPrintable(cls->baseClassName()), cls->isQObject() ? "'QObject-type'" : "'not a QObject-type'");
    printf("  interfaces:");
    foreach(AbstractMetaClass *iface, cls->interfaces())
        printf(" %s", qPrintable(iface->name()));
    printf("\n");
    printf("  attributes:");
    dumpMetaDylanAttributes(cls);

    printf("\n  functions:\n");
    foreach(const AbstractMetaFunction *func, cls->functions())
        dumpMetaDylanFunction(func);
}

void dumpMetaDylanTree(const AbstractMetaClassList &classes) {
    foreach(AbstractMetaClass *cls, classes) {
        dumpMetaDylanClass(cls);
    }
}
