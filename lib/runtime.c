#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#define TRUE ~0

extern void printInt(int32_t v)
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
    fprintf(stderr, "runtime error\n");
    exit(1);
}
extern int64_t readInt()
{
    int64_t a;
    if (scanf("%ld", &a) == 0)
        error();
    getchar();
    return a;
}

extern char *readString()
{
    int64_t input;
    char *str = NULL;
    int64_t end = 0;
    int64_t len = 0;
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

    int64_t len1 = strlen(str1);
    int64_t len2 = strlen(str2);

    int64_t buffsize = len1 + len2 + 1;
    char *cnc = malloc(sizeof(char) * buffsize);
    memcpy(cnc, str1, len1);
    memcpy(cnc + len1, str2, len2);
    cnc[buffsize - 1] = 0;
    return cnc;
}

extern int64_t __equals(char *str1, char *str2)
{
    if (str1 == NULL || str2 == NULL)
        error();
    if (strcmp(str1, str2) == 0)
        return TRUE;
    else
        return 0;
}

extern int64_t __notequals(char *str1, char *str2)
{
    return !__equals(str1, str2);
}

extern void *__heap(int64_t size)
{
    return calloc(size, 8);
}