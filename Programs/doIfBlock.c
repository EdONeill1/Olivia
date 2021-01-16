#include <stdio.h>

int main(int argc, char ** argv){

        int x = 0;
        int y = 44;
        int z = 25;
        int n = 0;

        while (n < 10){

                if (x < 5){
                        x = x + 2;
                        z = z + x;
                }

                if (y > 5){
                        y = y - 45;
                        z = y;
                }

                n = n + 1;
        }

        printf("x : %d \t y : %d \t z : %d\n", x, y, z);

        return 0;
}
