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

#include "metadylan.h"

bool MetaDylanFunction::needsCallThrough() const {
    if (ownerClass()->isInterface())
        return false;
    return AbstractMetaFunction::needsCallThrough();
}

QString MetaDylanFunction::dylanName() const {
  // convert from camelCase to dylan-style
  static QRegExp re1("(.)([A-Z][a-z]+)");
  static QRegExp re2("([a-z0-9])([A-Z])");
  QString function_name = name();
  const MetaDylanClass *owner_class = (const MetaDylanClass *)ownerClass();

  function_name.replace(re1, "\\1-\\2");
  function_name.replace(re2, "\\1-\\2");
  function_name = function_name.toLower();

  // prefix with class name
  function_name = owner_class->name().toLower() + "-" + function_name;
  return function_name;
}

QString MetaDylanFunction::dylanConstructorName() const {
  QString function_name = name();
  return function_name;
}

QString MetaDylanClass::dylanName() const {
  return "<" + name() + ">";
}

// TODO: return the real module name
QString MetaDylanClass::package() const {
  return "qt-core";
}
