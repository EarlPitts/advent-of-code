#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef struct {
    int x;
    int y;
} pos;

typedef struct {
    pos start;
    pos end;
    int option;
} step;

int main() {

    FILE* fp = fopen("input.txt", "r");

    int matrix[1000][1000];

    for (int i = 0; i < 1000; i++) {
        for (int j = 0; j < 1000; j++) {
            matrix[i][j] = 0;
        }
    }
    
    char line[100];

    step steps[1000];

    int count = 0;

    while (fgets(line, sizeof(line), fp) != NULL) {
        char* token = strtok(line, " ");
        if (strcmp(token, "toggle") == 0) steps[count].option = 2;
        else {
            token = strtok(NULL, " ");
            if (strcmp(token, "on") == 0) steps[count].option = 1;
            if (strcmp(token, "off") == 0) steps[count].option = 0;
        }

        token = strtok(NULL, ",");
        steps[count].start.x = atoi(token);

        token = strtok(NULL, " ");
        steps[count].start.y = atoi(token);

        token = strtok(NULL, " ");

        token = strtok(NULL, ",");
        steps[count].end.x = atoi(token);

        token = strtok(NULL, "\n");
        steps[count].end.y = atoi(token);

        count++;
    }

    for (int i = 0; i < count; i++) {
        step* s = &steps[i];
        for (int j = s->start.x - 1; j < s->end.x; j++) {
            for (int k = s->start.y - 1; k < s->end.y; k++) {
                if (s->option == 0) matrix[j][k] = 0;
                if (s->option == 1) matrix[j][k] = 1;
                if (s->option == 2) {
                    if (matrix[j][k] == 1) matrix[j][k] = 0;
                    else matrix[j][k] = 1;
                }
            }
        }
    }

    int cnt = 0;
    for (int i = 0; i < 1000; i++) {
        for (int j = 0; j < 1000; j++) {
            if (matrix[i][j] == 1) cnt++;
        }
    }

    printf("%i\n", cnt);


    return 0;
}