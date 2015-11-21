#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "List.h"

list_t * makelist (void * x, list_t * xs)
{
    list_t * ans = (list_t *)malloc(sizeof(list_t));
    ans->head = x;
    ans->tail = xs;
    return ans;
}

list_t * map(void* (*f)(void*, void*), void* env, list_t * xs)
{
    if (xs == NULL)
        return NULL;
    return makelist(f(env, xs->head), map(f, env, xs->tail));
}

list_t * filter(int (*f)(void*, void*), void* env, list_t * xs)
{
    if (xs == NULL)
        return NULL;
    if (f(env,xs->head))
        return makelist(xs->head, filter(f,env,xs->tail));
    return filter(f, env, xs->tail);
}

int length(list_t * xs)
{
    int ans = 0;
    /* using loop instead of recursive function call */
    while (xs != NULL)
    {
        ++ans;
        xs = xs->tail;
    }
    return ans;
}

void printlist(list_t * xs)
{
    /* using loop instead of recursive function call */
    while (xs != NULL)
    {
        printf("%d, ", xs->head);
        xs = xs->tail;
    }
    printf("\n");
}
