#include <stdio.h>

extern "C" int MyPrintf(const char* format, ...);

int main()
{
    //int len = MyPrintf("%%%%\n", 'R');
    
    MyPrintf("Ded, aka %s\n"
             "age: %o / %x / %d / %b\n"
             "favourite char:%c,\n"
             "random stuff  :%c, %cpercents: %%%%%%%%:%c\n" 
             "status: %s\n"
             "%d %s %x %d%%%c%b\n",
             "LOX", -0116, -0x4E12378F, -78, -(0), 'C', 'O', '\n', 'R', "Likvidirovan",
             -1, "Love", 3802, 100, 33, 31);
}