#include <stdio.h>
#include "List.h"

void* doubleInt(void* ignore, void* i)
{
    return (void*)(((intptr_t)i)*2);
}

list_t * doubleAll(list_t * xs)
{
    return map(doubleInt, NULL, xs);
}

void* ntimes(void* n, void* i)
{
    return (void*)(((intptr_t)i) * ((intptr_t)n));
}

list_t * ntimesAll(int n, list_t * xs)
{
    return map(ntimes, (void*)n, xs);
}

int isN(void*n, void* i)
{
    return ((intptr_t)n) == ((intptr_t)i);
}

int countNs(list_t * xs, intptr_t n)
{
    return length(filter(isN, (void*)n, xs));
}

int main(void)
{
    list_t t1,t2,t3,t4,t5;
    list_t *t6, *t10;
    int t7, t8, t9;
    t1.head = (void*)1;
    t2.head = (void*)2;
    t3.head = (void*)10;
    t4.head = (void*)1;
    t5.head = (void*)1;
    t1.tail = &t2;
    t2.tail = &t3;
    t3.tail = &t4;
    t4.tail = &t5;
    t5.tail = NULL;
    printlist(&t1);
    t6 = doubleAll(&t1);
    printlist(t6);
    t7 = countNs(&t1, 1);
    t8 = countNs(t6, 2);
    t9 = countNs(t6, 3);
    printf("%d, %d, %d\n", t7, t8, t9);

    t10 = ntimesAll(10, &t1);
    printlist(t10);

    return 0;
}
