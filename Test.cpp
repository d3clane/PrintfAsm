#include <stdio.h>

extern "C" int MyPrintf(const char* format, ...);

int main()
{
    //MyPrintf("%d\n", -0);
    int len = MyPrintf("Ded, aka %s\n"
             "age: %o / %x / %d / %b\n"
             "favourite char:%c,\n"
             "random stuff: %d, %c, percents: %%%%%%\n" 
             "BRB: %% + jm r1\n",
             "LOX", -0116, -78, -78, -(0), 'Z', 0, '\n');

    MyPrintf("len - %d\n", len);
}