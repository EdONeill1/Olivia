#include <stdio.h>
#include <stdlib.h>

// Naive approach for cons, car, cdr


int main(int argc, char ** argv){

   printf("The Following Programs Displays Cons, Cdr, Car functionality\n");
   printf("Cons the following arrays\n");

   int x[4] = {1, 2, 3, 4};
   int y[2] = {5, 6};
        
   int * z;
   z = malloc((4 + 2) * sizeof(int));
  
   //Print x
   printf("[");
   for(int i = 0; i < 4; i++){ 
           printf("%d ", x[i]);
   }
   printf("]\n");

   //Print y
   printf("[");
   for(int i = 0; i < 2; i++){
           printf("%d ", y[i]);
   }
   printf("]\n");

   //Cons here is essentially appending
   for(int i = 0; i < 4; i++){
           z[i] = x[i];
   }
   for(int j = 4; j < 6; j++){
           z[j] = y[j - 4];
   }

   printf("[");
   for(int i = 0; i < 4 + 2; i++){
           printf("%d ", z[i]);
   }
   printf("]\n");

   //Cdr requires making another array
   printf("Cdr and Car respectively\n");
   int cdr[5];
   for(int i = 0; i < 5; i++){
           cdr[i] = z[i + 1];
   }
   printf("[");
   for(int i = 0; i < 5; i++){
           printf("%d ", cdr[i]);
   }
   printf("]\n");

   //Car is the head of the list
   int car = z[0];
   printf("%d\n", car);

   return 0;
}


