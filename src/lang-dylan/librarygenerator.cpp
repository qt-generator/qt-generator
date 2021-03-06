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

#include "librarygenerator.h"
#include "reporthandler.h"
#include "fileout.h"

void LibraryGenerator::addBinding(const QString &module, const QString &binding) {
    m_modules[module].bindings << binding;
}

void LibraryGenerator::generate() {
    QHashIterator<QString, Module> module(m_modules);

    while (module.hasNext()) {
      module.next();

      FileOut file(resolveOutputDirectory() + "/" + module.key() + "/library.dylan");
      file.stream << "module: dylan-user\n";
      file.stream << "copyright: See LICENSE file in this distribution.\n";
      QStringList list = module.value().bindings.values();
      qSort(list.begin(), list.end());
      file.stream << "\ndefine library " << module.key() << endl;
      file.stream << "  use dylan;\n";
      file.stream << "  use common-dylan;\n";
      file.stream << "  use c-ffi;\n\n";

      file.stream << "  export " << module.key() << ";\n";
      file.stream << "end library;\n";

      file.stream << "\ndefine module " << module.key() << endl;
      file.stream << "  use dylan;\n";
      file.stream << "  use common-dylan;\n";
      file.stream << "  use c-ffi;\n\n";
      file.stream << "  export\n";
      bool is_first = true;
      foreach(const QString &entry, list) {
          if (!is_first) {
            file.stream << ",\n";
          }
          is_first = false;
          file.stream << "    " << entry;
      }
      file.stream << ";\n";
      file.stream << "end module;\n";

      if (file.done())
          ++m_num_generated_written;
      ++m_num_generated;
    }
}
