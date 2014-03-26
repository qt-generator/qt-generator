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

#ifndef DYLANGENERATOR_H
#define DYLANGENERATOR_H

#include "abstractmetalang.h"
#include "generator.h"
#include "lidgenerator.h"

#include <QTextStream>

class DylanGenerator : public Generator {
        Q_OBJECT

    public:
        DylanGenerator(LidGenerator *lid);

        static QString translateType(const AbstractMetaType *dylan_type, const AbstractMetaClass *context, Option option = NoOption);

        void writeInjectedCode(QTextStream &s,
                               const AbstractMetaFunction *dylan_function,
                               CodeSnip::Position position);
        static void writeArgument(QTextStream &s,
                                  const AbstractMetaFunction *dylan_function,
                                  const AbstractMetaArgument *dylan_argument,
                                  uint options = 0);
        static QString argumentString(const AbstractMetaFunction *dylan_function,
                                      const AbstractMetaArgument *dylan_argument,
                                      uint options = 0);
        void writeEnum(QTextStream &s, const AbstractMetaEnum *dylan_enum);
        void writeIntegerEnum(QTextStream &s, const AbstractMetaEnum *dylan_enum);
        void writeSignal(QTextStream &s, const AbstractMetaFunction *dylan_function);
        void writeFunction(QTextStream &s, const AbstractMetaFunction *dylan_function,
                           uint included_attributes = 0, uint excluded_attributes = 0);
        void writeFieldAccessors(QTextStream &s, const AbstractMetaField *field);
        void write(QTextStream &s, const AbstractMetaClass *dylan_class);

        QString arm_platform_kludge_defaultValue(const QString &defaultExpr) const;

        void writeFunctionOverloads(QTextStream &s, const AbstractMetaFunction *dylan_function,
                                    uint included_attributes, uint excluded_attributes);
        void writeEnumOverload(QTextStream &s, const AbstractMetaFunction *dylan_function,
                               uint include_attributes, uint exclude_attributes);
        void writeExtraFunctions(QTextStream &s, const AbstractMetaClass *dylan_class);
        void writeToStringFunction(QTextStream &s, const AbstractMetaClass *dylan_class);
        void writeCloneFunction(QTextStream &s, const AbstractMetaClass *dylan_class);
        void writeFunctionAttributes(QTextStream &s, const AbstractMetaFunction *dylan_function,
                                     uint included_attributes = 0, uint excluded_attributes = 0,
                                     uint options = 0);
        void writeConstructorContents(QTextStream &s, const AbstractMetaFunction *dylan_function);
        void writeFunctionArguments(QTextStream &s, const AbstractMetaFunction *dylan_function,
                                    int count = -1, uint options = 0);
        void writeDylanCallThroughContents(QTextStream &s, const AbstractMetaFunction *dylan_function, uint attributes = 0);
        void writeOwnershipForContainer(QTextStream &s, TypeSystem::Ownership ownership, AbstractMetaArgument *arg);
        void writeOwnershipForContainer(QTextStream &s, TypeSystem::Ownership ownership, AbstractMetaType *type,
                                        const QString &arg_name);
        void writePrivateNativeFunction(QTextStream &s, const AbstractMetaFunction *dylan_function);
        void writeDylanLangObjectOverrideFunctions(QTextStream &s, const AbstractMetaClass *cls);
        void writeReferenceCount(QTextStream &s, const ReferenceCount &refCount, const QString &argumentName);
        void retrieveModifications(const AbstractMetaFunction *f, const AbstractMetaClass *dylan_class,
                                   uint *exclude_attributes, uint *include_attributes) const;
        QString functionSignature(const AbstractMetaFunction *dylan_function,
                                  uint included_attributes,
                                  uint excluded_attributes,
                                  Option option = NoOption,
                                  int arg_count = -1);
        void setupForFunction(const AbstractMetaFunction *dylan_function,
                              uint *included_attributes, uint *excluded_attributes) const;

        virtual QString subDirectoryForClass(const AbstractMetaClass *dylan_class) const
        { return subDirectoryForPackage(dylan_class->package()); }

        virtual QString fileNameForClass(const AbstractMetaClass *dylan_class) const;

        bool isComparable(const AbstractMetaClass *cls) const;

#if 0
        void write1_dot_5_enum(QTextStream &s, const AbstractMetaEnum *dylan_enum);
#endif

        bool shouldGenerate(const AbstractMetaClass *dylan_class) const {
            return !dylan_class->typeEntry()->isContainer() && !dylan_class->typeEntry()->isVariant()
                   && (dylan_class->typeEntry()->codeGeneration() & TypeEntry::GenerateTargetLang);
        }

        void generate();

        /*virtual*/ QString resolveOutputDirectory() const { return dylanOutputDirectory(); }

        QString dylanOutputDirectory() const {
            if (!m_dylan_out_dir.isNull())
                return m_dylan_out_dir;
            return outputDirectory() + QLatin1String("/dylan");
        }
        void setDylanOutputDirectory(const QString &dylanOutDir) { m_dylan_out_dir = dylanOutDir; }

        QString logOutputDirectory() const {
            if (!m_log_out_dir.isNull())
                return m_log_out_dir;
            return outputDirectory();
        }
        void setLogOutputDirectory(const QString &logOutDir) { m_log_out_dir = logOutDir; }

    private:
        QString subDirectoryForPackage(const QString &package) const { return QString(package).replace(".", "/"); }
        void writeInstantiatedType(QTextStream &s, const AbstractMetaType *abstractMetaType) const;

    protected:
        LidGenerator *lidGenerator;
        QString m_package_name;
        QString m_dylan_out_dir;
        QString m_log_out_dir;		// needed for *.log generation
        QList<const AbstractMetaFunction *> m_nativepointer_functions;
        QList<const AbstractMetaFunction *> m_resettable_object_functions;
        QList<const AbstractMetaFunction *> m_reference_count_candidate_functions;
};

#endif // DYLANGENERATOR_H
