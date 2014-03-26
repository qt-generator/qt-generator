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

#ifndef LIBRARYGENERATOR_H
#define LIBRARYGENERATOR_H

#include "generator.h"

#include <QStringList>

class LibraryGenerator : public Generator {
        Q_OBJECT

    public:
        virtual void generate();

        /*virtual*/ QString resolveOutputDirectory() const { return cppOutputDirectory(); }

        QString cppOutputDirectory() const {
            if (!m_cpp_out_dir.isNull())
                return m_cpp_out_dir;
            return outputDirectory() + QLatin1String("/dylan");
        }

        void addBinding(const QString &binding);

    private:
        QSet<QString> m_bindings;
        QString m_cpp_out_dir;

};
#endif // LIBRARYGENERATOR_H

