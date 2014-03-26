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

#include "dylangenerator.h"
#include "reporthandler.h"
#include "metadylan.h"

#include <QtCore/QDir>
#include <QtCore/QTextStream>
#include <QtCore/QVariant>
#include <QtCore/QRegExp>
#include <QDebug>
#include "typesystem/typedatabase.h"
#include "wrapper.h"			/* for isTargetPlatformArmCpu */

static Indentor INDENT;

DylanGenerator::DylanGenerator(LidGenerator * lid) {
  lidGenerator = lid;
}

QString DylanGenerator::fileNameForClass(const AbstractMetaClass *dylan_class) const {
    return QString("%1.dylan").arg(dylan_class->name());
}

void DylanGenerator::writeFieldAccessors(QTextStream &s, const AbstractMetaField *field) {
    Q_ASSERT(field->isPublic() || field->isProtected());

    const AbstractMetaClass *declaringClass = field->enclosingClass();

    FieldModification mod = declaringClass->typeEntry()->fieldModification(field->name());

    // Set function
    if (mod.isWritable() && !field->type()->isConstant()) {
        const AbstractMetaFunction *setter = field->setter();
        if (declaringClass->hasFunction(setter)) {
            QString warning =
                QString("class '%1' already has setter '%2' for public field '%3'")
                .arg(declaringClass->name()).arg(setter->name()).arg(field->name());
            ReportHandler::warning(warning);
        } else {
            writeFunction(s, setter);
        }
    }

    // Get function
    const AbstractMetaFunction *getter = field->getter();
    if (mod.isReadable()) {
        if (declaringClass->hasFunction(getter)) {
            QString warning =
                QString("class '%1' already has getter '%2' for public field '%3'")
                .arg(declaringClass->name()).arg(getter->name()).arg(field->name());
            ReportHandler::warning(warning);
        } else {
            writeFunction(s, getter);
        }
    }
}

QString DylanGenerator::translateType(const AbstractMetaType *dylan_type, const AbstractMetaClass *context, Option option) {
    QString s;

    if (context != 0 && dylan_type != 0 && context->typeEntry()->isGenericClass() && dylan_type->originalTemplateType() != 0)
        dylan_type = dylan_type->originalTemplateType();

    if (!dylan_type) {
        s = "void";
    } else if (dylan_type->isArray()) {
        s = translateType(dylan_type->arrayElementType(), context) + "[]";
    } else if (dylan_type->isEnum() || dylan_type->isFlags()) {
        if ((dylan_type->isEnum() &&
                ((EnumTypeEntry *) dylan_type->typeEntry())->forceInteger()) ||
                (dylan_type->isFlags() &&
                ((FlagsTypeEntry *) dylan_type->typeEntry())->forceInteger())) {
            s = "<integer>";
        } else {
            if (option & EnumAsInts)
                s = "<integer>";
            else
                s = dylan_type->name();
        }
    } else {
        if (dylan_type->isPrimitive() && (option & BoxedPrimitive)) {
            s = static_cast<const PrimitiveTypeEntry *>(dylan_type->typeEntry())->javaObjectFullName();

        } else if (dylan_type->isNativePointer()) {
            // Pointer
            s = dylan_type->name();
        } else if (dylan_type->isContainer()) {
            s = dylan_type->typeEntry()->qualifiedTargetLangName();
            if ((option & SkipTemplateParameters) == 0) {
                s += '<';
                QList<AbstractMetaType *> args = dylan_type->instantiations();
                for (int i = 0; i < args.size(); ++i) {
                    if (i != 0)
                        s += ", ";
                    bool isMultiMap = static_cast<const ContainerTypeEntry *>(dylan_type->typeEntry())->type() == ContainerTypeEntry::MultiMapContainer
                                      && i == 1;
                    if (isMultiMap)
                        s += "dylan.util.List<";
                    s += translateType(args.at(i), context, BoxedPrimitive);
                    if (isMultiMap)
                        s += ">";
                }
                s += '>';
            }

        } else {
            const TypeEntry *type = dylan_type->typeEntry();
            if (type->designatedInterface())
                type = type->designatedInterface();
            s = type->name();
        }
    }

    return s;
}

QString DylanGenerator::argumentString(const AbstractMetaFunction *dylan_function,
                                      const AbstractMetaArgument *dylan_argument,
                                      uint options) {
    QString modified_type = dylan_function->typeReplaced(dylan_argument->argumentIndex() + 1);
    QString arg = "  parameter ";

    if ((options & SkipName) == 0) {
        arg += dylan_argument->argumentName();
    }

    arg += " :: <";

    if (modified_type.isEmpty())
        arg += translateType(dylan_argument->type(), dylan_function->implementingClass(), (Option) options);
    else
        arg += modified_type;

    arg += ">;";
    return arg;
}

void DylanGenerator::writeArgument(QTextStream &s,
                                  const AbstractMetaFunction *dylan_function,
                                  const AbstractMetaArgument *dylan_argument,
                                  uint options) {
    s << argumentString(dylan_function, dylan_argument, options);
}


void DylanGenerator::writeIntegerEnum(QTextStream &s, const AbstractMetaEnum *dylan_enum) {
    // TODO: fill this
}

void DylanGenerator::writeEnum(QTextStream &s, const AbstractMetaEnum *dylan_enum) {
    // TODO: fill this
}

void DylanGenerator::writePrivateNativeFunction(QTextStream &s, const AbstractMetaFunction *dylan_function) {
    // TODO: fill this (do we need it?)
}

static QString function_call_for_ownership(TypeSystem::Ownership owner) {
    if (owner == TypeSystem::CppOwnership) {
        return "disableGarbageCollection()";
    } else if (owner == TypeSystem::TargetLangOwnership) {
        return "setDylanOwnership()";
    } else if (owner == TypeSystem::DefaultOwnership) {
        return "reenableGarbageCollection()";

    } else {
        Q_ASSERT(false);
        return "bogus()";
    }
}

void DylanGenerator::writeOwnershipForContainer(QTextStream &s, TypeSystem::Ownership owner,
        AbstractMetaType *type, const QString &arg_name) {
    Q_ASSERT(type->isContainer());

    s << INDENT << "for (" << type->instantiations().at(0)->fullName() << " i : "
    << arg_name << ")" << endl
    << INDENT << "    if (i != null) i." << function_call_for_ownership(owner) << ";" << endl;

}

void DylanGenerator::writeOwnershipForContainer(QTextStream &s, TypeSystem::Ownership owner,
        AbstractMetaArgument *arg) {
    writeOwnershipForContainer(s, owner, arg->type(), arg->argumentName());
}

static FunctionModificationList get_function_modifications_for_class_hierarchy(const AbstractMetaFunction *dylan_function) {
    FunctionModificationList mods;
    const AbstractMetaClass *cls = dylan_function->implementingClass();
    while (cls != 0) {
        mods += dylan_function->modifications(cls);

        if (cls == cls->baseClass())
            break;
        cls = cls->baseClass();
    }
    return mods;
}

void DylanGenerator::writeInjectedCode(QTextStream &s, const AbstractMetaFunction *dylan_function,
                                      CodeSnip::Position position) {
    FunctionModificationList mods = get_function_modifications_for_class_hierarchy(dylan_function);
    foreach(FunctionModification mod, mods) {
        if (mod.snips.count() <= 0)
            continue ;

        foreach(CodeSnip snip, mod.snips) {
            if (snip.position != position)
                continue ;

            if (snip.language != TypeSystem::TargetLangCode)
                continue ;

            QString code;
            QTextStream tmpStream(&code);
            snip.formattedCode(tmpStream, INDENT);
            ArgumentMap map = snip.argumentMap;
            ArgumentMap::iterator it = map.begin();
            for (;it != map.end();++it) {
                int pos = it.key() - 1;
                QString meta_name = it.value();

                if (pos >= 0 && pos < dylan_function->arguments().count()) {
                    code = code.replace(meta_name, dylan_function->arguments().at(pos)->argumentName());
                } else {
                    QString debug = QString("argument map specifies invalid argument index %1"
                                            "for function '%2'")
                                    .arg(pos + 1).arg(dylan_function->name());
                    ReportHandler::warning(debug);
                }

            }
            s << code << endl;
        }
    }
}


void DylanGenerator::writeDylanCallThroughContents(QTextStream &s, const AbstractMetaFunction *meta_function, uint attributes) {
    const MetaDylanFunction * dylan_function = (const MetaDylanFunction *)meta_function;
    writeInjectedCode(s, dylan_function, CodeSnip::Beginning);

    if (dylan_function->implementingClass()->isQObject()
            && !dylan_function->isStatic()
            && !dylan_function->isConstructor()
            && dylan_function->name() != QLatin1String("thread")
            && dylan_function->name() != QLatin1String("disposeLater")) {
        s << INDENT << "com.trolltech.qt.GeneratorUtilities.threadCheck(this);" << endl;
    }

    AbstractMetaArgumentList arguments = dylan_function->arguments();

    if (!dylan_function->isConstructor()) {
        TypeSystem::Ownership owner = dylan_function->ownership(dylan_function->implementingClass(), TypeSystem::TargetLangCode, -1);
        if (owner != TypeSystem::InvalidOwnership)
            s << INDENT << "this." << function_call_for_ownership(owner) << ";" << endl;
    }

    for (int i = 0; i < arguments.count(); ++i) {
        AbstractMetaArgument *arg = arguments.at(i);
        AbstractMetaType *type = arg->type();

        if (!dylan_function->argumentRemoved(i + 1)) {
            TypeSystem::Ownership owner = dylan_function->ownership(dylan_function->implementingClass(), TypeSystem::TargetLangCode, i + 1);
            if (owner != TypeSystem::InvalidOwnership) {
                s << INDENT << "if (" << arg->argumentName() << " != null) {" << endl;
                {
                    Indentation indent(INDENT);
                    if (arg->type()->isContainer())
                        writeOwnershipForContainer(s, owner, arg);
                    else
                        s << INDENT << arg->argumentName() << "." << function_call_for_ownership(owner) << ";" << endl;
                }
                s << INDENT << "}" << endl;
            }

            if (type->isArray()) {
                s << INDENT << "if (" << arg->argumentName() << ".length != " << type->arrayElementCount() << ")" << endl
                << INDENT << "    " << "throw new IllegalArgumentException(\"Wrong number of elements in array. Found: \" + "
                << arg->argumentName() << ".length + \", expected: " << type->arrayElementCount() << "\");"
                << endl << endl;
            }

            if (type->isEnum()) {
                EnumTypeEntry *et = (EnumTypeEntry *) type->typeEntry();
                if (et->forceInteger()) {
                    if (!et->lowerBound().isEmpty()) {
                        s << INDENT << "if (" << arg->argumentName() << " < " << et->lowerBound() << ")" << endl
                        << INDENT << "    throw new IllegalArgumentException(\"Argument " << arg->argumentName()
                        << " is less than lowerbound " << et->lowerBound() << "\");" << endl;
                    }
                    if (!et->upperBound().isEmpty()) {
                        s << INDENT << "if (" << arg->argumentName() << " > " << et->upperBound() << ")" << endl
                        << INDENT << "    throw new IllegalArgumentException(\"Argument " << arg->argumentName()
                        << " is greated than upperbound " << et->upperBound() << "\");" << endl;
                    }
                }
            }
        }
    }

    if (!dylan_function->isConstructor() && !dylan_function->isStatic()) {
        s << INDENT << "if (nativeId() == 0)" << endl
        << INDENT << "    throw new QNoNativeResourcesException(\"Function call on incomplete object of type: \" +getClass().getName());" << endl;
    }

    for (int i = 0; i < arguments.size(); ++i) {
        if (dylan_function->nullPointersDisabled(dylan_function->implementingClass(), i + 1)) {
            s << INDENT << "if (" << arguments.at(i)->argumentName() << " == null)" << endl
            << INDENT << "    throw new NullPointerException(\"Argument '" << arguments.at(i)->argumentName() << "': null not expected.\");" << endl;
        }
    }

    bool has_argument_referenceCounts = false;
    QList<ReferenceCount> referenceCounts;
    for (int i = 0; i < arguments.size() + 1; ++i) {
        referenceCounts = dylan_function->referenceCounts(dylan_function->implementingClass(),
                          i == 0 ? -1 : i);
        if (referenceCounts.size() > 0) {
            foreach(ReferenceCount refCount, referenceCounts) {
                // We just want to know this to secure return value into local variable
                // to hold over ReferenceCount management later on.
                if (refCount.action != ReferenceCount::Ignore) {
                    // Something active have been specified
                    has_argument_referenceCounts = true;
                    break;
                }
            }
        }

        // This is too early to manage referenceCount, it causes us to potentially overwrite the
        // last remaining GlobalReference to the object (before we have called the native method)
        // so the GC might destroy/delete it too early.
//        foreach(ReferenceCount refCount, referenceCounts)
//            writeReferenceCount(s, refCount, i == 0 ? "this" : arguments.at(i - 1)->argumentName());
    }

    // Lookup if there is a reference-count action required on the return value.
    QList<ReferenceCount> returnReferenceCounts = dylan_function->referenceCounts(dylan_function->implementingClass(), 0);
    AbstractMetaType *return_type = dylan_function->type();
    QString new_return_type = QString(dylan_function->typeReplaced(0)).replace('$', '.');
    bool has_return_type = new_return_type != "void"
    && (!new_return_type.isEmpty() || return_type != 0);
    TypeSystem::Ownership owner = dylan_function->ownership(dylan_function->implementingClass(), TypeSystem::TargetLangCode, 0);

    bool has_code_injections_at_the_end = false;
    FunctionModificationList mods = get_function_modifications_for_class_hierarchy(dylan_function);
    foreach(FunctionModification mod, mods) {
        foreach(CodeSnip snip, mod.snips) {
            if (snip.position == CodeSnip::End && snip.language == TypeSystem::TargetLangCode) {
                has_code_injections_at_the_end = true;
                break;
            }
        }
    }

    bool needs_return_variable = has_return_type
                                 && (owner != TypeSystem::InvalidOwnership || has_argument_referenceCounts || returnReferenceCounts.size() > 0 || has_code_injections_at_the_end);

    s << INDENT;
    if (has_return_type && dylan_function->argumentReplaced(0).isEmpty()) {
        if (needs_return_variable) {
            if (new_return_type.isEmpty())
                s << translateType(return_type, dylan_function->implementingClass());
            else
                s << new_return_type;

            s << " __qt_return_value = ";
        } else {
            s << "return ";
        }

        if (return_type && return_type->isTargetLangEnum()) {
            s << ((EnumTypeEntry *) return_type->typeEntry())->qualifiedTargetLangName() << ".resolve(";
        } else if (return_type && return_type->isTargetLangFlags()) {
            s << "new " << return_type->typeEntry()->qualifiedTargetLangName() << "(";
        }
    }

    if (attributes & SuperCall) {
        s << "super.";
    }
    s << dylan_function->marshalledName() << "(";

    if (!dylan_function->isConstructor() && !dylan_function->isStatic())
        s << "nativeId()";


    for (int i = 0; i < arguments.count(); ++i) {
        const AbstractMetaArgument *arg = arguments.at(i);
        const AbstractMetaType *type = arg->type();

        if (!dylan_function->argumentRemoved(i + 1)) {
            if (i > 0 || (!dylan_function->isStatic() && !dylan_function->isConstructor()))
                s << ", ";

            if (type->isTargetLangEnum() || type->isTargetLangFlags()) {
                s << arg->argumentName() << ".value()";
            } else if (!type->hasNativeId()) {
                s << arg->argumentName();
            } else {
                bool force_abstract = type->typeEntry()->isComplex() && (((static_cast<const ComplexTypeEntry *>(type->typeEntry()))->typeFlags() & ComplexTypeEntry::ForceAbstract) != 0);
                if (!force_abstract) {
                    s << arg->argumentName() << " == null ? 0 : ";
                } // else if (value type is abstract) then we will get a null pointer exception, which is all right

                s << arg->argumentName() << ".nativeId()";
            }
        }
    }

    s << ")";

    // This closed the ".resolve(" or the "new MyType(" fragments
    if (return_type && (return_type->isTargetLangEnum() || return_type->isTargetLangFlags()))
        s << ")";

    s << ";" << endl;

    // We must ensure we retain a Dylan hard-reference over the native method call
    // so that the GC will not destroy the C++ object too early.  At this point we
    // have called the native method call so can manage referenceCount issues.
    // First the input arguments
    for (int i = 0; i < arguments.size() + 1; ++i) {
        referenceCounts = dylan_function->referenceCounts(dylan_function->implementingClass(),
                          i == 0 ? -1 : i);

        foreach(ReferenceCount refCount, referenceCounts)
            writeReferenceCount(s, refCount, i == 0 ? "this" : arguments.at(i - 1)->argumentName());
    }

    if (!dylan_function->argumentReplaced(0).isEmpty()) {
        s << INDENT << "return " << dylan_function->argumentReplaced(0) << ";" << endl;
        return;
    }

    // Then the return value
    foreach(ReferenceCount referenceCount, returnReferenceCounts) {
        writeReferenceCount(s, referenceCount, "__qt_return_value");
    }

    writeInjectedCode(s, dylan_function, CodeSnip::End);

    if (needs_return_variable) {
        if (owner != TypeSystem::InvalidOwnership) {
            s << INDENT << "if (__qt_return_value != null) {" << endl;
            if (return_type->isContainer())
                writeOwnershipForContainer(s, owner, return_type, "__qt_return_value");
            else
                s << INDENT << "    __qt_return_value." << function_call_for_ownership(owner) << ";" << endl;
            s << INDENT << "}" << endl;
        }
        s << INDENT << "return __qt_return_value;" << endl;
    }

    if (dylan_function->isConstructor()) {
        TypeSystem::Ownership owner = dylan_function->ownership(dylan_function->implementingClass(), TypeSystem::TargetLangCode, -1);
        if (owner != TypeSystem::InvalidOwnership && dylan_function->isConstructor())
            s << INDENT << "this." << function_call_for_ownership(owner) << ";" << endl;
    }
}

void DylanGenerator::writeSignal(QTextStream &s, const AbstractMetaFunction *dylan_function) {
    Q_ASSERT(dylan_function->isSignal());

    if (dylan_function->isModifiedRemoved(TypeSystem::TargetLangCode))
        return ;

    AbstractMetaArgumentList arguments = dylan_function->arguments();
    int sz = arguments.count();

    QString signalTypeName("Signal");
    if (dylan_function->isPrivate()) {
        signalTypeName = "PrivateSignal";
    }

    signalTypeName += QString::number(sz);
    if (sz > 0) {
        signalTypeName += "<";
        for (int i = 0; i < sz; ++i) {
            if (i > 0)
                signalTypeName += ", ";

            QString modifiedType = dylan_function->typeReplaced(i + 1);

            if (modifiedType.isEmpty())
                signalTypeName += translateType(arguments.at(i)->type(), dylan_function->implementingClass(), BoxedPrimitive);
            else
                signalTypeName += modifiedType;
        }
        signalTypeName += ">";
    }

    int exclude_attributes = AbstractMetaAttributes::Abstract
                             | AbstractMetaAttributes::Native;
    int include_attributes = AbstractMetaAttributes::Public;

    QString signalName = dylan_function->name();
    FunctionModificationList mods = dylan_function->modifications(dylan_function->implementingClass());
    foreach(FunctionModification mod, mods) {
        if (mod.isAccessModifier()) {
            exclude_attributes |= AbstractMetaAttributes::Public
                                  | AbstractMetaAttributes::Protected
                                  | AbstractMetaAttributes::Private
                                  | AbstractMetaAttributes::Friendly;
            include_attributes &= ~(exclude_attributes);

            if (mod.isPublic())
                include_attributes |= AbstractMetaAttributes::Public;
            else if (mod.isProtected())
                include_attributes |= AbstractMetaAttributes::Protected;
            else if (mod.isPrivate())
                include_attributes |= AbstractMetaAttributes::Private;
            else if (mod.isFriendly())
                include_attributes |= AbstractMetaAttributes::Friendly;

            exclude_attributes &= ~(include_attributes);

        }
    }

    writeFunctionAttributes(s, dylan_function, include_attributes, exclude_attributes,
                            SkipReturnType);
    s << signalTypeName;
    s << " " << signalName << " = new " << signalTypeName << "();" << endl;

    // We don't write out the functions for private signals, because they cannot
    // be emitted, hence they will never be used...
    if (!dylan_function->isPrivate())
        writeFunction(s, dylan_function,
                      AbstractMetaAttributes::Private,
                      AbstractMetaAttributes::Visibility & ~AbstractMetaAttributes::Private);
}

void DylanGenerator::retrieveModifications(const AbstractMetaFunction *dylan_function,
        const AbstractMetaClass *dylan_class,
        uint *exclude_attributes,
        uint *include_attributes) const {
    FunctionModificationList mods = dylan_function->modifications(dylan_class);
//     printf("name: %s has %d mods\n", qPrintable(dylan_function->signature()), mods.size());
    foreach(FunctionModification mod, mods) {
        if (mod.isAccessModifier()) {
//             printf(" -> access mod to %x\n", mod.modifiers);
            *exclude_attributes |= AbstractMetaAttributes::Public
                                   | AbstractMetaAttributes::Protected
                                   | AbstractMetaAttributes::Private
                                   | AbstractMetaAttributes::Friendly;

            if (mod.isPublic())
                *include_attributes |= AbstractMetaAttributes::Public;
            else if (mod.isProtected())
                *include_attributes |= AbstractMetaAttributes::Protected;
            else if (mod.isPrivate())
                *include_attributes |= AbstractMetaAttributes::Private;
            else if (mod.isFriendly())
                *include_attributes |= AbstractMetaAttributes::Friendly;
        }

        if (mod.isFinal()) {
            *include_attributes |= AbstractMetaAttributes::FinalInTargetLang;
        } else if (mod.isNonFinal()) {
            *exclude_attributes |= AbstractMetaAttributes::FinalInTargetLang;
        }
    }

    *exclude_attributes &= ~(*include_attributes);
}

QString DylanGenerator::functionSignature(const AbstractMetaFunction *dylan_function,
        uint included_attributes, uint excluded_attributes,
        Option option,
        int arg_count) {
    AbstractMetaArgumentList arguments = dylan_function->arguments();
    int argument_count = arg_count < 0 ? arguments.size() : arg_count;

    QString result;
    QTextStream s(&result);
    QString functionName = dylan_function->name();
    // The actual function
    if (!(dylan_function->isEmptyFunction() || dylan_function->isNormal() || dylan_function->isSignal()))
        option = Option(option | SkipReturnType);
    writeFunctionAttributes(s, dylan_function, included_attributes, excluded_attributes, option);

    s << functionName << "(";
    writeFunctionArguments(s, dylan_function, argument_count, option);
    s << ")";

    return result;
}

void DylanGenerator::setupForFunction(const AbstractMetaFunction *dylan_function,
                                     uint *included_attributes,
                                     uint *excluded_attributes) const {
    *excluded_attributes |= dylan_function->ownerClass()->isInterface() || dylan_function->isConstructor()
                            ? AbstractMetaAttributes::Native | AbstractMetaAttributes::Final
                            : 0;
    if (dylan_function->ownerClass()->isInterface())
        *excluded_attributes |= AbstractMetaAttributes::Abstract;
    if (dylan_function->needsCallThrough())
        *excluded_attributes |= AbstractMetaAttributes::Native;

    const AbstractMetaClass *dylan_class = dylan_function->ownerClass();
    retrieveModifications(dylan_function, dylan_class, excluded_attributes, included_attributes);
}

void DylanGenerator::writeReferenceCount(QTextStream &s, const ReferenceCount &refCount,
                                        const QString &argumentName) {
    if (refCount.action == ReferenceCount::Ignore)
        return;

    QString refCountVariableName = refCount.variableName;
    if (!refCount.declareVariable.isEmpty() && refCount.action != ReferenceCount::Set) {
        s << INDENT << "dylan.util.Collection<Object> __rcTmp = (dylan.util.Collection<Object>)com.trolltech.qt.GeneratorUtilities.fetchField(this," << endl
        << INDENT << "                                                                 " << refCount.declareVariable << ".class," << endl
        << INDENT << "                                                                 \"" << refCountVariableName << "\");" << endl;
        refCountVariableName = "__rcTmp";
    }

    if (refCount.action != ReferenceCount::Set) {
        s << INDENT << "if (" << argumentName << " != null";

        if (!refCount.conditional.isEmpty())
            s << " && " << refCount.conditional;

        s << ") {" << endl;
    } else {
        if (!refCount.conditional.isEmpty())
            s << INDENT << "if (" << refCount.conditional << ") ";
        s << INDENT << "{" << endl;
    }

    {
        Indentation indent(INDENT);
        switch (refCount.action) {
            case ReferenceCount::Add:
                s << INDENT << refCountVariableName << ".add(" << argumentName << ");" << endl;
                break;
            case ReferenceCount::AddAll:
                s << INDENT << refCountVariableName << ".addAll(" << argumentName << ");" << endl;
                break;
            case ReferenceCount::Remove:
                s << INDENT << "while (" << refCountVariableName << ".remove(" << argumentName << ")) ;" << endl;
                break;
            case ReferenceCount::Set: {
                if (refCount.declareVariable.isEmpty())
                    s << INDENT << refCount.variableName << " = " << argumentName << ";" << endl;
                else
                    s << INDENT << "com.trolltech.qt.GeneratorUtilities.setField(this, " << refCount.declareVariable << ".class, \"" << refCountVariableName << "\", " << argumentName << ");" << endl;
            }
            default:
                break;
        };
    }
    s << INDENT << "}" << endl;
}

void DylanGenerator::writeFunction(QTextStream &s, const AbstractMetaFunction *meta_function,
                                  uint included_attributes, uint excluded_attributes) {
    const MetaDylanFunction * dylan_function = (const MetaDylanFunction *)meta_function;
    s << endl;
    if (dylan_function->isModifiedRemoved(TypeSystem::TargetLangCode))
        return ;
    QString functionName = dylan_function->name();
    setupForFunction(dylan_function, &included_attributes, &excluded_attributes);
    const MetaDylanClass *dylan_owner_class = (const MetaDylanClass *)dylan_function->ownerClass();

    s << "define C-function ";
    if (!dylan_function->isConstructor()) {
      s << dylan_function->dylanName() << endl;
    } else {
      s << dylan_function->dylanConstructorName() << endl;
    }
    if (!dylan_function->isStatic() && !dylan_function->isConstructor()) {
      s << "  input parameter self :: " << dylan_owner_class->dylanName() << ";" << endl;;
    }
    writeFunctionArguments(s, dylan_function, dylan_function->arguments().count());
    s << endl << "  c-name: \"" << dylan_function->marshalledName() << "\";" << endl;
    s << "end;" << endl;
}

static void write_equals_parts(QTextStream &s, const AbstractMetaFunctionList &lst, char prefix, bool *first) {
    foreach(AbstractMetaFunction *f, lst) {
        AbstractMetaArgument *arg = f->arguments().at(0);
        QString type = f->typeReplaced(1);
        if (type.isEmpty())
            type = arg->type()->typeEntry()->qualifiedTargetLangName();
        s << INDENT << (*first ? "if" : "else if") << " (other instanceof " << type << ")" << endl
        << INDENT << "    return ";
        if (prefix != 0) s << prefix;
        s << f->name() << "((" << type << ") other);" << endl;
        *first = false;
    }
}

static void write_compareto_parts(QTextStream &s, const AbstractMetaFunctionList &lst, int value, bool *first) {
    foreach(AbstractMetaFunction *f, lst) {
        AbstractMetaArgument *arg = f->arguments().at(0);
        QString type = f->typeReplaced(1);
        if (type.isEmpty())
            type = arg->type()->typeEntry()->qualifiedTargetLangName();
        s << INDENT << (*first ? "if" : "else if") << " (other instanceof " << type << ") {" << endl
        << INDENT << "    if (" << f->name() << "((" << type << ") other)) return " << value << ";" << endl
        << INDENT << "    else return " << -value << ";" << endl
        << INDENT << "}" << endl;
        *first = false;
    }
    s << INDENT << "throw new ClassCastException();" << endl;
}

bool DylanGenerator::isComparable(const AbstractMetaClass *cls) const {
    AbstractMetaFunctionList eq_functions = cls->equalsFunctions();
    AbstractMetaFunctionList neq_functions = cls->notEqualsFunctions();

    // Write the comparable functions
    AbstractMetaFunctionList ge_functions = cls->greaterThanFunctions();
    AbstractMetaFunctionList geq_functions = cls->greaterThanEqFunctions();
    AbstractMetaFunctionList le_functions = cls->lessThanFunctions();
    AbstractMetaFunctionList leq_functions = cls->lessThanEqFunctions();

    bool hasEquals = eq_functions.size() || neq_functions.size();
    bool isComparable = hasEquals
                        ? ge_functions.size() || geq_functions.size() || le_functions.size() || leq_functions.size()
                        : geq_functions.size() == 1 && leq_functions.size() == 1;

    return isComparable;
}


void DylanGenerator::writeDylanLangObjectOverrideFunctions(QTextStream &s,
        const AbstractMetaClass *cls) {
    AbstractMetaFunctionList eq_functions = cls->equalsFunctions();
    AbstractMetaFunctionList neq_functions = cls->notEqualsFunctions();

    if (eq_functions.size() || neq_functions.size()) {
        s << endl
        << INDENT << "@SuppressWarnings(\"unchecked\")" << endl
        << INDENT << "@Override" << endl
        << INDENT << "public boolean equals(Object other) {" << endl;
        bool first = true;
        write_equals_parts(s, eq_functions, (char) 0, &first);
        write_equals_parts(s, neq_functions, '!', &first);
        s << INDENT << "    return false;" << endl
        << INDENT << "}" << endl << endl;
    }

    // Write the comparable functions
    AbstractMetaFunctionList ge_functions = cls->greaterThanFunctions();
    AbstractMetaFunctionList geq_functions = cls->greaterThanEqFunctions();
    AbstractMetaFunctionList le_functions = cls->lessThanFunctions();
    AbstractMetaFunctionList leq_functions = cls->lessThanEqFunctions();

    bool hasEquals = eq_functions.size() || neq_functions.size();
    bool comparable = isComparable(cls);
    if (comparable) {
        s << INDENT << "public int compareTo(Object other) {" << endl;
        {
            Indentation indent(INDENT);
            if (hasEquals) {
                s << INDENT << "if (equals(other)) return 0;" << endl;
                bool first = false;
                if (le_functions.size()) {
                    write_compareto_parts(s, le_functions, -1, &first);
                } else if (ge_functions.size()) {
                    write_compareto_parts(s, ge_functions, 1, &first);
                } else if (leq_functions.size()) {
                    write_compareto_parts(s, leq_functions, -1, &first);
                } else if (geq_functions.size()) {
                    write_compareto_parts(s, geq_functions, 1, &first);
                }

            } else if (le_functions.size() == 1) {
                QString className = cls->typeEntry()->qualifiedTargetLangName();
                s << INDENT << "if (operator_less((" << className << ") other)) return -1;" << endl
                << INDENT << "else if (((" << className << ") other).operator_less(this)) return 1;" << endl
                << INDENT << "else return 0;" << endl;

            } else if (geq_functions.size() == 1 && leq_functions.size()) {
                QString className = cls->typeEntry()->qualifiedTargetLangName();
                s << INDENT << "boolean less = operator_less_or_equal((" << className << ") other);" << endl
                << INDENT << "boolean greater = operator_greater_or_equal((" << className << ") other);" << endl
                << INDENT << "if (less && greater) return 0;" << endl
                << INDENT << "else if (less) return -1;" << endl
                << INDENT << "else return 1;" << endl;
            }
        }

        s << INDENT << "}" << endl;
    }


    if (cls->hasHashFunction() || eq_functions.size() > 0 || neq_functions.size() > 0) {
        AbstractMetaFunctionList hashcode_functions = cls->queryFunctionsByName("hashCode");
        bool found = false;
        foreach(const AbstractMetaFunction *function, hashcode_functions) {
            if (function->actualMinimumArgumentCount() == 0) {
                found = true;
                break;
            }
        }

        if (!found) {
            if (cls->hasHashFunction()) {
                s << endl
                << INDENT << "@Override" << endl
                << INDENT << "public int hashCode() {" << endl
                << INDENT << "    if (nativeId() == 0)" << endl
                << INDENT << "        throw new QNoNativeResourcesException(\"Function call on incomplete object of type: \" +getClass().getName());" << endl
                << INDENT << "    return __qt_hashCode(nativeId());" << endl
                << INDENT << "}" << endl
                << INDENT << "native int __qt_hashCode(long __this_nativeId);" << endl;
            } else { // We have equals() but no qHash(), we return 0 from hashCode() to respect
                // contract of dylan.lang.Object
                s << endl
                << INDENT << "@Override" << endl
                << INDENT << "public int hashCode() { return 0; }" << endl;
            }
        }
    }

    // Qt has a standard toString() conversion in QVariant?
    QVariant::Type type = QVariant::nameToType(cls->qualifiedCppName().toLatin1());
    if (QVariant(type).canConvert(QVariant::String) &&  !cls->hasToStringCapability()) {
        AbstractMetaFunctionList tostring_functions = cls->queryFunctionsByName("toString");
        bool found = false;
        foreach(const AbstractMetaFunction *function, tostring_functions) {
            if (function->actualMinimumArgumentCount() == 0) {
                found = true;
                break;
            }
        }

        if (!found) {
            s << endl
            << INDENT << "@Override" << endl
            << INDENT << "public String toString() {" << endl
            << INDENT << "    if (nativeId() == 0)" << endl
            << INDENT << "        throw new QNoNativeResourcesException(\"Function call on incomplete object of type: \" +getClass().getName());" << endl
            << INDENT << "    return __qt_toString(nativeId());" << endl
            << INDENT << "}" << endl
            << INDENT << "native String __qt_toString(long __this_nativeId);" << endl;
        }
    }
}

void DylanGenerator::writeEnumOverload(QTextStream &s, const AbstractMetaFunction *dylan_function,
                                      uint include_attributes, uint exclude_attributes) {
    AbstractMetaArgumentList arguments = dylan_function->arguments();

    if ((dylan_function->implementingClass() != dylan_function->declaringClass())
            || ((!dylan_function->isNormal() && !dylan_function->isConstructor()) || dylan_function->isEmptyFunction() || dylan_function->isAbstract())) {
        return ;
    }


    int option = 0;
    if (dylan_function->isConstructor())
        option = Option(option | SkipReturnType);
    else
        include_attributes |= AbstractMetaAttributes::FinalInTargetLang;

    int generate_enum_overload = -1;
    for (int i = 0; i < arguments.size(); ++i)
        generate_enum_overload = arguments.at(i)->type()->isTargetLangFlags() ? i : -1;

    if (generate_enum_overload >= 0) {
        s << endl;

        writeFunctionAttributes(s, dylan_function, include_attributes, exclude_attributes, option);
        s << dylan_function->name() << "(";
        if (generate_enum_overload > 0) {
            writeFunctionArguments(s, dylan_function, generate_enum_overload);
            s << ", ";
        }

        // Write the ellipsis convenience argument
        AbstractMetaArgument *affected_arg = arguments.at(generate_enum_overload);
        EnumTypeEntry *originator = ((FlagsTypeEntry *)affected_arg->type()->typeEntry())->originator();

        s << originator->javaPackage() << "." << originator->javaQualifier() << "." << originator->targetLangName()
        << " ... " << affected_arg->argumentName() << ") {" << endl;

        s << "        ";
        QString new_return_type = dylan_function->typeReplaced(0);
        if (new_return_type != "void" && (!new_return_type.isEmpty() || dylan_function->type() != 0))
            s << "return ";

        if (dylan_function->isConstructor()) {
            s << "this";
        } else {
            if (dylan_function->isStatic())
                s << dylan_function->implementingClass()->fullName() << ".";
            else
                s << "this.";
            s << dylan_function->name();
        }

        s << "(";
        for (int i = 0; i < generate_enum_overload; ++i) {
            s << arguments.at(i)->argumentName() << ", ";
        }
        s << "new " << affected_arg->type()->fullName() << "(" << affected_arg->argumentName() << "));" << endl
        << "    }" << endl;
    }
}

void DylanGenerator::writeInstantiatedType(QTextStream &s, const AbstractMetaType *abstractMetaType) const {
    Q_ASSERT(abstractMetaType != 0);

    const TypeEntry *type = abstractMetaType->typeEntry();
    s << type->qualifiedTargetLangName();

    if (abstractMetaType->hasInstantiations()) {
        s << "<";
        QList<AbstractMetaType *> instantiations = abstractMetaType->instantiations();
        for (int i = 0; i < instantiations.size(); ++i) {
            if (i > 0)
                s << ", ";

            writeInstantiatedType(s, instantiations.at(i));
        }
        s << ">";
    }
}

QString DylanGenerator::arm_platform_kludge_defaultValue(const QString &defaultExpr) const
{
    QString tmpString = defaultExpr;
    // ARM platform hack (this is 'qreal' type and dylan needs 'f' suffix for floats)
    //  input="new com.trolltech.qt.core.QRectF(0., 0., 1000000000., 1000000000.)"
    // output="new com.trolltech.qt.core.QRectF(0.f, 0.f, 1000000000.f, 1000000000.f)"
    const char *arm_match = "new com.trolltech.qt.core.QRectF(";
    int arm_fix_pos;
    if((arm_fix_pos = tmpString.indexOf(arm_match)) >= 0) {
        int length = tmpString.length();

        if(arm_fix_pos >= 0) {
            arm_fix_pos += strlen(arm_match);	// skip the arm_match string
            if(arm_fix_pos >= length)
                goto abort;	// must be more chars after

            while(arm_fix_pos < length) {
                arm_fix_pos = tmpString.indexOf(".", arm_fix_pos);
                if(arm_fix_pos < 0)
                    break;	// no more matches
                arm_fix_pos++;	// skip the "."

                // if there are more chars after position check they
                //  are not digits and skip them if they are
                while(arm_fix_pos < length && tmpString[arm_fix_pos].isDigit())
                    arm_fix_pos++;	// skip digits "2345" in "1.2345"

                if(arm_fix_pos < length) {
                    // if there are still more chars check the next one is
                    //  not already an 'f' that we're about to add
                    if(tmpString[arm_fix_pos] == QChar('f')) {
                        arm_fix_pos++;
                        continue;    // already has 'f' suffix on real number
                    }
                }
                tmpString = tmpString.insert(arm_fix_pos, QLatin1String("f"));
            }
        }
    }

abort:
    return QString(tmpString);
}

void DylanGenerator::writeFunctionOverloads(QTextStream &s, const AbstractMetaFunction *dylan_function,
        uint include_attributes, uint exclude_attributes) {
    AbstractMetaArgumentList arguments = dylan_function->arguments();
    int argument_count = arguments.size();

    // We only create the overloads for the class that actually declares the function
    // unless this is an interface, in which case we create the overloads for all
    // classes that directly implement the interface.
    const AbstractMetaClass *decl_class = dylan_function->declaringClass();
    if (decl_class->isInterface()) {
        AbstractMetaClassList interfaces = dylan_function->implementingClass()->interfaces();
        foreach(AbstractMetaClass *iface, interfaces) {
            if (iface == decl_class) {
                decl_class = dylan_function->implementingClass();
                break;
            }
        }
    }
    if (decl_class != dylan_function->implementingClass())
        return;

    // Figure out how many functions we need to write out,
    // One extra for each default argument.
    int overload_count = 0;
    uint excluded_attributes = AbstractMetaAttributes::Abstract
                               | AbstractMetaAttributes::Native
                               | exclude_attributes;
    uint included_attributes = (dylan_function->isConstructor() ? 0 : AbstractMetaAttributes::Final) | include_attributes;

    for (int i = 0; i < argument_count; ++i) {
        if (!arguments.at(i)->defaultValueExpression().isEmpty() && !dylan_function->argumentRemoved(i + 1))
            ++overload_count;
    }
    Q_ASSERT(overload_count <= argument_count);
    for (int i = 0; i < overload_count; ++i) {
        int used_arguments = argument_count - i - 1;

        QString signature = functionSignature(dylan_function, included_attributes,
                                              excluded_attributes,
                                              dylan_function->isEmptyFunction()
                                              || dylan_function->isNormal()
                                              || dylan_function->isSignal() ? NoOption
                                              : SkipReturnType,
                                              used_arguments);

        s << endl;

        s << signature << " {\n        ";
        QString new_return_type = dylan_function->typeReplaced(0);
        if (new_return_type != "void" && (!new_return_type.isEmpty() || dylan_function->type()))
            s << "return ";
        if (dylan_function->isConstructor())
            s << "this";
        else
            s << dylan_function->name();
        s << "(";

        int written_arguments = 0;
        for (int j = 0; j < argument_count; ++j) {
            if (!dylan_function->argumentRemoved(j + 1)) {
                if (written_arguments++ > 0)
                    s << ", ";

                if (j < used_arguments) {
                    s << arguments.at(j)->argumentName();
                } else {
                    AbstractMetaType *arg_type = 0;
                    QString modified_type = dylan_function->typeReplaced(j + 1);
                    if (modified_type.isEmpty()) {
                        arg_type = arguments.at(j)->type();
                        if (arg_type->isNativePointer()) {
                            s << "(com.trolltech.qt.QNativePointer)";
                        } else {
                            const AbstractMetaType *abstractMetaType = arguments.at(j)->type();
                            const TypeEntry *type = abstractMetaType->typeEntry();
                            if (type->designatedInterface())
                                type = type->designatedInterface();
                            if (!type->isEnum() && !type->isFlags()) {
                                s << "(";
                                writeInstantiatedType(s, abstractMetaType);
                                s << ")";
                            }
                        }
                    } else {
                        s << "(" << modified_type.replace('$', '.') << ")";
                    }

                    QString defaultExpr = arguments.at(j)->defaultValueExpression();
                    int pos = defaultExpr.indexOf(".");
                    if (pos > 0) {
                        QString someName = defaultExpr.left(pos);
                        ComplexTypeEntry *ctype =
                            TypeDatabase::instance()->findComplexType(someName);
                        QString replacement;
                        if (ctype != 0 && ctype->isVariant())
                            replacement = "com.trolltech.qt.QVariant.";
                        else if (ctype != 0)
                            replacement = ctype->javaPackage() + "." + ctype->targetLangName() + ".";
                        else
                            replacement = someName + ".";
                        defaultExpr = defaultExpr.replace(someName + ".", replacement);
                    }

                    // Check global command flag for targetting ARM then enable this kludge
                    if(Wrapper::isTargetPlatformArmCpu) {
                        if(fileNameForClass(decl_class).compare(QLatin1String("QAbstractTextDocumentLayout.dylan")) == 0
                                                         && dylan_function->name().compare(QLatin1String("update")) == 0) {
                            defaultExpr = arm_platform_kludge_defaultValue(defaultExpr);
                        }
                    }

                    if (arg_type != 0 && arg_type->isFlags()) {
                        s << "new " << arg_type->fullName() << "(" << defaultExpr << ")";
                    } else {
                        s << defaultExpr;
                    }
                }
            }
        }
        s << ");\n    }" << endl;
    }
}

void DylanGenerator::write(QTextStream &s, const AbstractMetaClass *abstract_class) {
    MetaDylanClass * dylan_class = (MetaDylanClass *)abstract_class;
    ReportHandler::debugSparse("Generating class: " + dylan_class->fullName());

    bool fakeClass = dylan_class->attributes() & AbstractMetaAttributes::Fake;
    s << "module:  " << dylan_class->package() << endl;
    s << "synopsis: generated bindings" << endl;
    s << "copyright: See LICENSE file in this distribution." << endl << endl;

    if (dylan_class->isInterface()) {
        s << "// interface" << endl;
        s << "define open C-subtype ";
    } else {
        if (dylan_class->isPublic())
            s << "define open C-subtype ";
        // else friendly
    }

    const ComplexTypeEntry *type = dylan_class->typeEntry();

    s << dylan_class->dylanName();

    // implementing interfaces...
    bool implements = false;
    AbstractMetaClassList interfaces = dylan_class->interfaces();

    if ((!dylan_class->isInterface() && !dylan_class->isNamespace()) || !interfaces.isEmpty()) {
        s << " (";
        if (!dylan_class->baseClassName().isEmpty()) {
            MetaDylanClass *base_class = (MetaDylanClass *)dylan_class->baseClass();
            s << base_class->dylanName();
        } else {
            QString sc = type->defaultSuperclass();

            if (!sc.isEmpty())
                s << "<C-void*>";
        }
        if (!interfaces.isEmpty() && dylan_class->isInterface())
            s << ", ";
        else {
            implements = true;
        }
        for (int i = 0; i < interfaces.size(); ++i) {
            MetaDylanClass *iface = (MetaDylanClass *)interfaces.at(i);
            s << ", ";
            s << iface->dylanName();
        }

        s << ")";
    }
    s << endl << "end;" << endl;

    Indentation indent(INDENT);
/*
 * TODO: emit signals
    // Signals
    AbstractMetaFunctionList signal_funcs = dylan_class->queryFunctions(AbstractMetaClass::Signals
                                            | AbstractMetaClass::ClassImplements
                                            | AbstractMetaClass::NotRemovedFromTargetLang);
    for (int i = 0; i < signal_funcs.size(); ++i)
        writeSignal(s, signal_funcs.at(i));
*/

    // Class has subclasses but also only private constructors
    if (!dylan_class->isFinalInTargetLang() && dylan_class->isFinalInCpp()) {
        s << endl << INDENT << "/**" << endl
        << INDENT << " * This constructor is a place holder intended to prevent" << endl
        << INDENT << " * users from subclassing the class. Certain classes can" << endl
        << INDENT << " * unfortunately only be subclasses internally. The constructor" << endl
        << INDENT << " * will indiscriminately throw an exception if called. If the" << endl
        << INDENT << " * exception is ignored, any use of the constructed object will" << endl
        << INDENT << " * cause an exception to occur." << endl << endl
        << INDENT << " * @throws QClassCannotBeSubclassedException" << endl
        << INDENT << " **/" << endl
        << INDENT << "protected " << dylan_class->name() << "() throws QClassCannotBeSubclassedException {" << endl
        << INDENT << "    throw new QClassCannotBeSubclassedException(" << dylan_class->name() << ".class);" << endl
        << INDENT << "}" << endl << endl;
    }

    // Functions
    bool alreadyHasCloneMethod = false;
    AbstractMetaFunctionList dylan_funcs = dylan_class->functionsInTargetLang();
    for (int i = 0; i < dylan_funcs.size(); ++i) {
        AbstractMetaFunction *function = dylan_funcs.at(i);

        // If a method in an interface class is modified to be private, this should
        // not be present in the interface at all, only in the implementation.
        if (dylan_class->isInterface()) {
            uint includedAttributes = 0;
            uint excludedAttributes = 0;
            retrieveModifications(function, dylan_class, &excludedAttributes, &includedAttributes);
            if (includedAttributes & AbstractMetaAttributes::Private)
                continue;
        }

        if (function->name() == "clone" && function->arguments().isEmpty())
            alreadyHasCloneMethod = true;

        writeFunction(s, function);
    }

    // Just the private functions for abstract functions implemeneted in superclasses
    if (!dylan_class->isInterface() && dylan_class->isAbstract()) {
        dylan_funcs = dylan_class->queryFunctions(AbstractMetaClass::NormalFunctions |
                                                AbstractMetaClass::AbstractFunctions |
                                                AbstractMetaClass::NotRemovedFromTargetLang);
        foreach(AbstractMetaFunction *dylan_function, dylan_funcs) {
            if (dylan_function->implementingClass() != dylan_class) {
                s << endl;
                writePrivateNativeFunction(s, dylan_function);
            }
        }
    }

/*
    TODO: emit field accessors
    // Field accessors
    AbstractMetaFieldList fields = dylan_class->fields();
    foreach(const AbstractMetaField *field, fields) {
        if (field->wasPublic() || (field->wasProtected() && !dylan_class->isFinal())) {
            writeFieldAccessors(s, field);
        }
    }
*/
}

void DylanGenerator::generate() {
    Generator::generate();

    { //log native pointer api
        const AbstractMetaClass *last_class = 0;
        QString fileName("mjb_nativepointer_api.log");
        QFile file(fileName);
        if (!logOutputDirectory().isNull())
            file.setFileName(QDir(logOutputDirectory()).absoluteFilePath(fileName));
        if (file.open(QFile::WriteOnly)) {
            QTextStream s(&file);

            AbstractMetaFunctionList nativepointer_functions;
            for (int i = 0; i < m_nativepointer_functions.size(); ++i) {
                AbstractMetaFunction *f =
                    const_cast<AbstractMetaFunction *>(m_nativepointer_functions[i]);
                if (f->ownerClass() == f->declaringClass() || f->isFinal())
                    nativepointer_functions.append(f);
            }

            s << "Number of public or protected functions with QNativePointer API: " <<
            nativepointer_functions.size() << endl;
            foreach(const AbstractMetaFunction *f, nativepointer_functions) {
                if (last_class != f->ownerClass()) {
                    last_class = f->ownerClass();
                    s << endl << endl << "Class " << last_class->name() << ":" << endl;
                    s << "---------------------------------------------------------------------------------"
                    << endl;
                }

                s << f->minimalSignature() << endl;
            }

            m_nativepointer_functions.clear();
        }
    }

    { // log object type usage of classes
        const AbstractMetaClass *last_class = 0;
        QString fileName("mjb_nativepointer_api.log");
        QFile file(fileName);
        if (!logOutputDirectory().isNull())
            file.setFileName(QDir(logOutputDirectory()).absoluteFilePath(fileName));
        if (file.open(QFile::WriteOnly)) {
            QTextStream s(&file);

            AbstractMetaFunctionList resettable_object_functions;
            for (int i = 0; i < m_resettable_object_functions.size(); ++i) {
                AbstractMetaFunction *f =
                    const_cast<AbstractMetaFunction *>(m_resettable_object_functions[i]);
                if (f->ownerClass() == f->declaringClass() || f->isFinal())
                    resettable_object_functions.append(f);
            }

            s << "Number of public or protected functions that return a " <<
            "non-QObject object type, or that are virtual and take " <<
            "a non-QObject object type argument: " <<
            resettable_object_functions.size() << endl;
            foreach(const AbstractMetaFunction *f, resettable_object_functions) {
                if (last_class != f->ownerClass()) {
                    last_class = f->ownerClass();
                    s << endl << endl << "Class " << last_class->name() << ":" << endl;
                    s << "---------------------------------------------------------------------------------"
                    << endl;
                }

                s << f->minimalSignature() << endl;
            }

            m_resettable_object_functions.clear();
        }
    }

    { // log possible reference counting candidates
        QString fileName("mjb_reference_count_candidates.log");
        QFile file(fileName);
        if (!logOutputDirectory().isNull())
            file.setFileName(QDir(logOutputDirectory()).absoluteFilePath(fileName));
        if (file.open(QFile::WriteOnly)) {
            QTextStream s(&file);

            s << "The following functions have a signature pattern which may imply that" << endl
            << "they need to apply reference counting to their arguments ("
            << m_reference_count_candidate_functions.size() << " functions) : " << endl;

            foreach(const AbstractMetaFunction *f, m_reference_count_candidate_functions) {
                s << f->implementingClass()->fullName() << " : " << f->minimalSignature() << endl;
            }
        }
        file.close();
    }
}

void DylanGenerator::writeFunctionAttributes(QTextStream &s, const AbstractMetaFunction *dylan_function,
        uint included_attributes, uint excluded_attributes,
        uint options) {
    uint attr = (dylan_function->attributes() & (~excluded_attributes)) | included_attributes;

    if ((attr & AbstractMetaAttributes::Public) || (attr & AbstractMetaAttributes::Protected)) {

        // Does the function use native pointer API?
        bool nativePointer = dylan_function->type() && dylan_function->type()->isNativePointer()
                             && dylan_function->typeReplaced(0).isEmpty();

        // Does the function need to be considered for resetting the Dylan objects after use?
        bool resettableObject = false;

        if (!nativePointer
                && dylan_function->type()
                && dylan_function->type()->hasInstantiations()
                && dylan_function->typeReplaced(0).isEmpty()) {

            QList<AbstractMetaType *> instantiations = dylan_function->type()->instantiations();

            foreach(const AbstractMetaType *type, instantiations) {
                if (type && type->isNativePointer()) {
                    nativePointer = true;
                    break;
                }
            }

        }

        AbstractMetaArgumentList arguments = dylan_function->arguments();
        if (!nativePointer || (!resettableObject && !dylan_function->isFinal())) {
            foreach(const AbstractMetaArgument *argument, arguments) {
                if (!dylan_function->argumentRemoved(argument->argumentIndex() + 1)
                        && dylan_function->typeReplaced(argument->argumentIndex() + 1).isEmpty()) {

                    if (argument->type()->isNativePointer()) {

                        nativePointer = true;
                        if (resettableObject) break ;

                    } else if (!dylan_function->isFinalInTargetLang()
                               && argument->type()->isObject()
                               && !argument->type()->isQObject()
                               && !dylan_function->resetObjectAfterUse(argument->argumentIndex() + 1)
                               && dylan_function->ownership(dylan_function->declaringClass(),
                                        TypeSystem::ShellCode, argument->argumentIndex() + 1) ==
                                            TypeSystem::InvalidOwnership) {

                        resettableObject = true;
                        if (nativePointer) break ;

                    } else if (argument->type()->hasInstantiations()) {

                        QList<AbstractMetaType *> instantiations = argument->type()->instantiations();
                        foreach(AbstractMetaType *type, instantiations) {
                            if (type && type->isNativePointer()) {
                                nativePointer = true;
                                if (resettableObject) break;
                            } else if (!dylan_function->isFinal()
                                       && type
                                       && type->isObject()
                                       && !type->isQObject()
                                       && !dylan_function->resetObjectAfterUse(argument->argumentIndex() + 1)) {
                                resettableObject = true;
                                if (nativePointer) break ;
                            }
                        }

                        if (nativePointer && resettableObject)
                            break;

                    }
                }
            }
        }

        if (nativePointer && !m_nativepointer_functions.contains(dylan_function))
            m_nativepointer_functions.append(dylan_function);
        if (resettableObject && !m_resettable_object_functions.contains(dylan_function))
            m_resettable_object_functions.append(dylan_function);
    }

    if ((options & SkipAttributes) == 0) {
        if (dylan_function->isEmptyFunction()
                || dylan_function->isDeprecated()) s << INDENT << "@Deprecated" << endl;

        bool needsSuppressUnusedWarning = TypeDatabase::instance()->includeEclipseWarnings()
                                          && dylan_function->isSignal()
                                          && (((excluded_attributes & AbstractMetaAttributes::Private) == 0)
                                              && (dylan_function->isPrivate()
                                                  || ((included_attributes & AbstractMetaAttributes::Private) != 0)));

        if (needsSuppressUnusedWarning && dylan_function->needsSuppressUncheckedWarning()) {
            s << INDENT << "@SuppressWarnings({\"unchecked\", \"unused\"})" << endl;
        } else if (dylan_function->needsSuppressUncheckedWarning()) {
            s << INDENT << "@SuppressWarnings(\"unchecked\")" << endl;
        } else if (needsSuppressUnusedWarning) {
            s << INDENT << "@SuppressWarnings(\"unused\")" << endl;
        }

        if (!(attr & NoBlockedSlot)
                && !dylan_function->isAllowedAsSlot()
                && !dylan_function->isConstructor()
                && !dylan_function->isSlot()
                && !dylan_function->isSignal()
                && !dylan_function->isStatic()
                && !(included_attributes & AbstractMetaAttributes::Static))
            s << INDENT << "@QtBlockedSlot" << endl;

        s << INDENT;
        if (attr & AbstractMetaAttributes::Public) s << "public ";
        else if (attr & AbstractMetaAttributes::Protected) s << "protected ";
        else if (attr & AbstractMetaAttributes::Private) s << "private ";

        bool isStatic = (attr & AbstractMetaAttributes::Static);

        if (attr & AbstractMetaAttributes::Native) s << "native ";
        else if (!isStatic && (attr & AbstractMetaAttributes::FinalInTargetLang)) s << "final ";
        else if (!isStatic && (attr & AbstractMetaAttributes::Abstract)) s << "abstract ";

        if (isStatic) s << "static ";
    }

    if ((options & SkipReturnType) == 0) {
        QString modified_type = dylan_function->typeReplaced(0);
        if (modified_type.isEmpty())
            s << translateType(dylan_function->type(), dylan_function->implementingClass(), (Option) options);
        else
            s << modified_type.replace('$', '.');
        s << " ";
    }
}

void DylanGenerator::writeConstructorContents(QTextStream &s, const AbstractMetaFunction *meta_function) {
    // Write constructor
    const MetaDylanFunction * dylan_function = (const MetaDylanFunction *)meta_function;
    s << "{" << endl;
    {
        Indentation indent(INDENT);
        s << INDENT << "super((QPrivateConstructor)null);" << endl;

        writeDylanCallThroughContents(s, dylan_function);

        // Write out expense checks if present...
        const AbstractMetaClass *dylan_class = dylan_function->implementingClass();
        const ComplexTypeEntry *te = dylan_class->typeEntry();
        if (te->expensePolicy().isValid()) {
            s << endl;
            const ExpensePolicy &ep = te->expensePolicy();
            s << INDENT << "com.trolltech.qt.GeneratorUtilities.countExpense(" << dylan_class->fullName()
            << ".class, " << ep.cost << ", " << ep.limit << ");" << endl;
        }

        foreach(CodeSnip snip, te->codeSnips()) {
            if (snip.language == TypeSystem::Constructors) {
                snip.formattedCode(s, INDENT);
            }
        }
    }
    s << INDENT << "}" << endl << endl;

    // Write native constructor
    writePrivateNativeFunction(s, dylan_function);
}

void DylanGenerator::writeFunctionArguments(QTextStream &s, const AbstractMetaFunction *dylan_function,
        int argument_count, uint options) {
    AbstractMetaArgumentList arguments = dylan_function->arguments();

    if (argument_count == -1)
        argument_count = arguments.size();

    for (int i = 0; i < argument_count; ++i) {
        if (!dylan_function->argumentRemoved(i + 1)) {
            if (i != 0)
                s << endl;
            writeArgument(s, dylan_function, arguments.at(i), options);
        }
    }
}


void DylanGenerator::writeExtraFunctions(QTextStream &s, const AbstractMetaClass *dylan_class) {
    const ComplexTypeEntry *class_type = dylan_class->typeEntry();
    Q_ASSERT(class_type);

    CodeSnipList code_snips = class_type->codeSnips();
    foreach(const CodeSnip &snip, code_snips) {
        if ((!dylan_class->isInterface() && snip.language == TypeSystem::TargetLangCode)
                || (dylan_class->isInterface() && snip.language == TypeSystem::Interface)) {
            s << endl;
            snip.formattedCode(s, INDENT);
        }
    }
}


void DylanGenerator::writeToStringFunction(QTextStream &s, const AbstractMetaClass *dylan_class) {
    bool generate = dylan_class->hasToStringCapability() && !dylan_class->hasDefaultToStringFunction();
    bool core = dylan_class->package() == QLatin1String("com.trolltech.qt.core");
    bool qevent = false;

    const AbstractMetaClass *cls = dylan_class;
    while (cls) {
        if (cls->name() == "QEvent") {
            qevent = true;
            break;
        }
        cls = cls->baseClass();
    }

    if (generate || qevent) {

        if (qevent && core) {
            s << endl
            << "    @Override" << endl
            << "    public String toString() {" << endl
            << "        return getClass().getSimpleName() + \"(type=\" + type().name() + \")\";" << endl
            << "    }" << endl;
        } else {
            s << endl
            << "    @Override" << endl
            << "    public String toString() {" << endl
            << "        if (nativeId() == 0)" << endl
            << "            throw new QNoNativeResourcesException(\"Function call on incomplete object of type: \" +getClass().getName());" << endl
            << "        return __qt_toString(nativeId());" << endl
            << "    }" << endl
            << "    native String __qt_toString(long __this_nativeId);" << endl;
        }
    }
}

void DylanGenerator::writeCloneFunction(QTextStream &s, const AbstractMetaClass *dylan_class) {
    s << endl
    << "    @Override" << endl
    << "    public " << dylan_class->name() << " clone() {" << endl
    << "        if (nativeId() == 0)" << endl
    << "            throw new QNoNativeResourcesException(\"Function call on incomplete object of type: \" +getClass().getName());" << endl
    << "        return __qt_clone(nativeId());" << endl
    << "    }" << endl
    << "    native " << dylan_class->name() << " __qt_clone(long __this_nativeId);" << endl;
}
