#include <stdio.h>

int main(int argc, char ** argv){

        int x = 0;
        int n = 0;
        int y = 6;

        while (n < 10){
                if (x < 5){
                        x = x + 1;
                        n = n + 1;
                }
                if (y > 5){
                        y = y * 2;
                        n = n + 1;
                }
        }

        printf("x : %d \ny : %d\nn : %d\n", x, y,  n);

        return 0;
}
