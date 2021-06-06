#ifndef __LIST_H__
#define __LIST_H__

typedef struct list {
  int head;
  struct list *next;
} list;
int car(list *lst);
list* cdr(list *lst);
list* cons(int x, list *lst);
list* singleton (int x);
list* toList(int arr [], int len);
void printList(list *lst);
list* reverse(list *lst);

#endif