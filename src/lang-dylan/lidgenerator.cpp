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

#include "lidgenerator.h"
#include "reporthandler.h"
#include "fileout.h"

void LidGenerator::addHeader(const QString &folder, const QString &header) {
    lidHash[folder].headers << header;
}

void LidGenerator::addSource(const QString &folder, const QString &source) {
    lidHash[folder].sources << source;
}

void LidGenerator::addBinding(const QString &folder, const QString &binding) {
    lidHash[folder].bindings << binding;
}

void LidGenerator::generate() {
    QHashIterator<QString, Lid> lid(lidHash);
    while (lid.hasNext()) {
        lid.next();

        FileOut file(resolveOutputDirectory() + "/" + lid.key());
        file.stream << "Library: xxx\n";
        file.stream << "Target-Type: dll\n";
        file.stream << "Files: library.dylan\n";
        QStringList list = lid.value().bindings;
        qSort(list.begin(), list.end());
        foreach(const QString &entry, list) {
            file.stream << "        " << entry << "\n";
        }

        file.stream << "C-Header-Files: ";
        bool first = true;
        list = lid.value().headers;
        qSort(list.begin(), list.end());
        foreach(const QString &entry, list) {
            if (!first) {
                file.stream << "        ";
            }
            file.stream << entry << "\n";
            first = false;
        }

        file.stream << "C-Source-Files: ";
        first = true;
        list = lid.value().sources;
        qSort(list.begin(), list.end());
        foreach(const QString &entry, list) {
            if (!first) {
                file.stream << "        ";
            }
            file.stream << entry << "\n";
            first = false;
        }

        if (file.done())
            ++m_num_generated_written;
        ++m_num_generated;
    }
}
