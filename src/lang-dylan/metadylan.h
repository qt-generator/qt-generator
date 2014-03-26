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

#ifndef METADYLAN_H
#define METADYLAN_H

#include "abstractmetalang.h"

class MetaDylanClass;
class MetaDylanField;
class MetaDylanFunction;
class MetaDylanType;
class MetaDylanVariable;
class MetaDylanArgument;
class MetaDylanEnumValue;
class MetaDylanEnum;



class MetaDylanType : public AbstractMetaType {};

class MetaDylanArgument : public AbstractMetaArgument {};

class MetaDylanField : public AbstractMetaField {};

class MetaDylanFunction : public AbstractMetaFunction {
    public:
        MetaDylanFunction()
              : AbstractMetaFunction() {
        }

        /*override*/ bool needsCallThrough() const;
        QString dylanName() const;
        QString dylanConstructorName() const;
	QString package() const;
};

class MetaDylanEnumValue : public AbstractMetaEnumValue {};

class MetaDylanEnum : public AbstractMetaEnum {};

class MetaDylanClass : public AbstractMetaClass {
    public:
        QString dylanName() const;
        QString package() const;
};

#endif // METADYLAN_H
