#include <stdio.h>

struct Test
{
    float m;
    float b;
    // char a;
    // char b;
    double x;
};

void testStruct(struct Test a)
{
    printf("%f %f %f\n", a.b, a.x, a.m);
}