#include <stdio.h>

extern "C" int MyPrintf(const char* format, ...);

int main()
{
    MyPrintf("Ded, aka %s\n"
             "age: %o / %x / %d / %b\n"
             "favourite char:%c,\n"
             "random stuff:%d, %c, percents: %%%%%%\n" 
             "BRB: %%%b + jm r1\n",
             "LOX", 0116, 0x4E, 78, 0b1001110, 'Z', 56, 'O', 0b1101111);
}