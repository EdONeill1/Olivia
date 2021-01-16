#include <stdio.h>
int main(int argc, char ** argv){

        int x = 0;
        int y = 1;
        int z = 1;


        while (x < 100){
                x = x + 1;
                y = x * 2;
                z = y / 2;
        }

        printf("%d\n", x);
        printf("%d\n", y);
        printf("%d\n", z);

        return 0;
}

