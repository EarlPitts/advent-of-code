#include <stdio.h>
#include <stdlib.h>

int smallest(int a, int b, int c) {
    int bigger = (a > b) ? a : b;
    int biggest =  (bigger > c) ? bigger : c;  
    return 2 * (a + b + c - biggest);
}

int main() {

    FILE *fp;

    fp = fopen("input.txt", "r");
    int l, w, h;
    int total = 0;

    while (fscanf(fp, "%ix%ix%i", &l, &w, &h) != EOF) {
        total += smallest(l, w, h) + l * w * h;
    }

    printf("%i", total);

}
