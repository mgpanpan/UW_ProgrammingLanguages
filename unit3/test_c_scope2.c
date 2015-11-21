#include <stdio.h>

int x = 1;

int addx(int y)
{
    return x + y;
}

int main(void)
{
    x = 2;
    int z = addx(3);
    
    printf("%d\n", z);
    
    return 0;
}
