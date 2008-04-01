/****************************************************************************
**
** Copyright (C) 1992-$THISYEAR$ $TROLLTECH$. All rights reserved.
** Copyright (C) 2002-2005 Roberto Raggi <roberto@kdevelop.org>
**
** This file is part of $PRODUCT$.
**
** $CPP_LICENSE$
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/


#ifndef FASTLIST_H
#define FASTLIST_H

#include "smallobject.h"

template <typename Tp>
struct ListNode
{
  Tp element;
  int index;
  mutable const ListNode<Tp> *next;

  static ListNode *create(const Tp &element, pool *p)
  {
    ListNode<Tp> *node = new (p->allocate(sizeof(ListNode))) ListNode();
    node->element = element;
    node->index = 0;
    node->next = node;

    return node;
  }

  static ListNode *create(const ListNode *n1, const Tp &element, pool *p)
  {
    ListNode<Tp> *n2 = ListNode::create(element, p);

    n2->index = n1->index + 1;
    n2->next = n1->next;
    n1->next = n2;

    return n2;
  }

  inline ListNode<Tp>() { }

  inline const ListNode<Tp> *at(int index) const
  {
    const ListNode<Tp> *node = this;
    while (index != node->index)
      node = node->next;

    return node;
  }

  inline bool hasNext() const
  { return index < next->index; }

  inline int count() const
  { return 1 + toBack()->index; }

  inline const ListNode<Tp> *toFront() const
  { return toBack()->next; }

  inline const ListNode<Tp> *toBack() const
  {
    const ListNode<Tp> *node = this;
    while (node->hasNext())
      node = node->next;

    return node;
  }
};

template <class Tp>
inline const ListNode<Tp> *snoc(const ListNode<Tp> *list,
                const Tp &element, pool *p)
{
  if (!list)
    return ListNode<Tp>::create(element, p);

  return ListNode<Tp>::create(list->toBack(), element, p);
}

#endif // FASTLIST_H

// kate: space-indent on; indent-width 2; replace-tabs on;

