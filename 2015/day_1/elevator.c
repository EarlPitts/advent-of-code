#include <stdio.h>

int main() {

   FILE *fp;

   fp = fopen("input", "r");
   int next;
   int count = 0;

   while ((next = fgetc(fp)) != EOF) {
      switch (next)
      {
      case (int) '(' : count++;
         break;
      
      case (int) ')' : count--;
         break;
      }
   }
   
   printf("%i", count);
}
