#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct list {
  int head;
  struct list *next;
} list;

int car (list *lst) {
  return lst->head;
}

list* cdr (list *lst) {
  return lst->next;
}

list* cons (int x, list *lst) {
  list *p = malloc(sizeof(list));
  p->head = x;
  p->next = lst;
  return p;
}

list* singleton (int x) {
  return cons(x, NULL);
}

list* toList (int arr [], int len) {
  list *p = NULL;
  for (int i = len - 1; i >= 0; i--) {
    p = cons(arr[i], p);
  }
  return p;
}

void printList (list *lst) {
  list *p = lst;
  while (p != NULL) {
    printf("%d ", car(p));
    p = cdr(p);
  }
  printf("\n");
}

list* reverseHelper (list *lst, list *acc) {
  if (lst == NULL)
    return acc;
  else
    return reverseHelper(cdr(lst), cons(car(lst), acc));
}

list* reverse (list *lst) {
  return reverseHelper(lst, NULL);
}

bool member (int x, list *lst) {
  list *p = lst;
  if (p == NULL)
    return false;
  else if (car(p) == x)
    return true;
  else return member(x, cdr(p));
}