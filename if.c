#include <stdio.h>
#include <stdlib.h>

int main(int argc, char ** argv){

        int * X;
        X = malloc(10 * sizeof(int));
        for(int i = 0; i < 10; i++){
                X[i] = i + 1;
        }

        int n = 0;
        int x = 0;
        while (n < 10){
                x = X[n];

                if ((x / 2) < 2){
                        printf("%d\n", 1);
                }
                if ((x / 2) > 5){
                        printf("%d\n", 2);
                }
                n = n + 1;
        }

        printf("%d\n", x);

        return 0;
}
