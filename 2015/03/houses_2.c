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

    pos p, p2;

    p.x = 0;
    p.y = 0;
    p2.x = 0;
    p2.y = 0;

    
    pos *arr = malloc(sizeof(pos) * 10000);
    pos *arr_start = arr;

    memcpy(arr, &p, sizeof(p));
    arr++;
    int count = 1;

    int robot = 0;
    char dir;
    while((dir = fgetc(fp)) != EOF) {

        if (!robot) {
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
            robot = 1;
        } else {
            switch (dir) {

                case '^':
                    p2.y++;
                    break;

                case 'v':
                    p2.y--;
                    break;

                case '<':
                    p2.x--;
                    break;

                case '>':
                    p2.x++;
                    break;
                }
            memcpy(arr, &p2, sizeof(p2));
            robot = 0;
        }

        if (!visited(arr_start, arr))
            count++; 

        arr++;
    }

    free(arr_start);

    printf("%i\n", count);

    return 0;
}
