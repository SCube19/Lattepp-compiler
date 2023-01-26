#include <stdio.h>

int foo(int x)
{
    int a = x * x;
    int b = x + x;
    int c = a + b;
    int d = b / a;
    int e = c + d;
    int f = d * e - 1;
    printf("%d\n", f);
    while (f > 0)
    {
        a = a + b;
        c = c / d;
        e = a * c;
        f--;
    }
    return a;
}

int main()
{
    int x;
    scanf("%d", &x);
    printf("%d\n", foo(x));
    return 0;
}