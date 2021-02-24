#include <stdio.h>

int main(){

        int n = 10;
        int a = 1;
        int b = 0;

        while (n > 0){


                if ( (n % 2) == 0){
                        n /= 2;
                        a += b;
                        b = b;
                }else if ( (n % 2) == 1){
                        n = (n - 1) / 2;
                        a = a;
                        b += a;
                }
        }

        printf("%d\n",b);

        return 0;
}
