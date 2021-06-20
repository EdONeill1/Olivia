
#include <stdio.h>

int main(int argc, char ** argv){

        printf("The following program is a Reduction which finds the factorial of the integer ten.\n");

        int n = 1;
        int N = 10;
        int r = 1;

        while (n != N){
                n = n + 1;
                r = r * n;
        }

        printf("%d\n", r);

}
