#include <stdio.h>


// Naive approach
int main(int argc, char ** argv){

        printf("The following program calculates the number of integers in a list that are greater than twenty five.\n");
        int f[10] = {1, 2, 56, 78, 100, 45, 31, 32, 49, 67};
        int x = 0;
        int n = 0;
        int N = 10;

        while (n != 10){
                if (f[n] > 25){
                        x = x + 1;
                }else if (f[n] < 25){
                        x = x + 0;
                }else if (f[n] == 0){
                        x = x + 0;
                }
                n = n + 1;
        }

        printf("%d\n", x);

        return 0;
}

