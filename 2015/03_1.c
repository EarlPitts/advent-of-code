#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
    int x;
    int y;
} pos;

int visited(pos *start, pos *curr) {
    while (start != curr) {
        if (start->x == curr->x && start->y == curr->y)
            return 1;
        start++;
    }
    return 0;
}

int main() {

    FILE* fp;

    fp = fopen("input.txt", "r");

    pos p;
    p.x = 0;
    p.y = 0;
    
    pos *arr = malloc(sizeof(pos) * 10000);
    pos *arr_start = arr;

    memcpy(arr, &p, sizeof(p));
    arr++;
    int count = 1;

    char dir;
    while((dir = fgetc(fp)) != EOF) {
        switch (dir) {

            case '^':
                p.y++;
                break;

            case 'v':
                p.y--;
                break;

            case '<':
                p.x--;
                break;

            case '>':
                p.x++;
                break;
            }

        memcpy(arr, &p, sizeof(p));
        if (!visited(arr_start, arr))
            count++; 

        arr++;
    }

    free(arr_start);

    printf("%i\n", count);

    return 0;
}
