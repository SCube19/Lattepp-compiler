#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define TRUE ~0

extern void printInt(int v)
{
    printf("%d\n", v);
}
extern void printString(char *v)
{
    if (v != NULL)
        printf("%s\n", v);
}
extern void error()
{
    fprintf(stderr, "runtime error");
    exit(1);
}
extern int readInt()
{
    int a;
    if (scanf("%d", &a) != 0)
        error();
    return a;
}

static char *_readString(char *original, char input, int *end, int *len)
{
    if (*end == *len)
    {
        (*len) = (*len) * 2 + 1;
        original = realloc(original, *len);
    }
    original[*end] = input;
    (*end)++;
    return original;
}

extern char *readString()
{
    int input;
    char *str = NULL;
    int end = 0;
    int len = 0;
    while ((input = getchar()) != '\n')
    {
        if (end == len)
        {
            len = len * 2 + 1;
            str = realloc(str, len);
        }
        str[end] = input;
        end++;
    }
    return str;
}

extern char *__concat(char *str1, char *str2)
{
    if (str1 == NULL && str2 == NULL)
        return NULL;
    else if (str1 == NULL)
        return str2;
    else if (str2 == NULL)
        return str1;

    int len1 = strlen(str1);
    int len2 = strlen(str2);

    int buffsize = len1 + len2 + 1;
    char *cnc = malloc(sizeof(char) * buffsize);
    memcpy(cnc, str1, len1);
    memcpy(cnc + len1, str2, len2);
    cnc[buffsize - 1] = 0;
    return cnc;
}

extern int __equals(char *str1, char *str2)
{
    if (str1 == NULL || str2 == NULL)
        error();
    if (strcmp(str1, str2) == 0)
        return TRUE;
    else
        return 0;
}

extern int __notequals(char *str1, char *str2)
{
    return !__equals(str1, str2);
}