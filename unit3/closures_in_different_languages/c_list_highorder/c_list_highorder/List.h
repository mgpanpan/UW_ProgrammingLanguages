typedef struct List list_t;
struct List
{
    void *head;
    list_t *tail;
};

extern list_t * map(void* (*f)(void*, void*), void* env, list_t * xs);
extern list_t * filter(int (*f)(void*, void*), void* env, list_t * xs);
extern int length(list_t * xs);
extern void printlist(list_t * xs);
