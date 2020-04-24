#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NOT 1
#define AND 2
#define OR 3
#define LSHIFT 4
#define RSHIFT 5

typedef struct {
    char** key;
    int* value;
    int cnt;
} map;

typedef struct {
    int gate;
    char* in1;
    char* in2;
    char* out; 
} instruction;

int get(map* m, char* key) {
    int i = 0;

    while (i < m->cnt) {
        if (strcmp(m->key[i], key) == 0) return m->value[i];
    }
    
    return -1;
}

void set(map* m, char* key, int value) {
    m->cnt++;
    m->key = realloc(m->key, sizeof(char*) * m->cnt);
    m->key[m->cnt-1] = malloc(strlen(key) + 1);
    strcpy(m->key[m->cnt-1], key);
    m->value = realloc(m->value, sizeof(int*) * m->cnt);
    m->value[m->cnt-1] = value;
}

void make_instr(char* line, int cnt, instruction* instructions) {
    instruction i;
    memset(&i, 0, sizeof(instruction));

    char* next = strtok(line, " ");

    while (strcmp(next, "->") != 0) {

        if (strcmp(next, "NOT") == 0) i.gate = NOT;
        else if (strcmp(next, "AND") == 0) i.gate = AND;
        else if (strcmp(next, "OR") == 0) i.gate = OR;
        else if (strcmp(next, "LSHIFT") == 0) i.gate = LSHIFT;
        else if (strcmp(next, "RSHIFT") == 0) i.gate = RSHIFT;
        else {
            if (i.in1 == NULL) {
                i.in1 = malloc(strlen(next) + 1);
                strcpy(i.in1, next);
            }
            else {
                i.in2 = malloc(strlen(next) + 1);
                strcpy(i.in2, next);
            }
        }

        next = strtok(NULL, " ");
    }

    next = strtok(NULL, "\n");

    i.out = malloc(strlen(next) + 1);
    i.out = strcpy(i.out, next);

    instructions[cnt] = i;
}

int main() {

    FILE* fp = fopen("input.txt", "r");

    instruction instructions[400];
    memset(instructions, 0, sizeof(instruction) * 400);

    int cnt = 0;
    char line[100];
    while(fgets(line, sizeof(line), fp) != NULL) {
        make_instr(line, cnt, instructions);
        cnt++;
    }

    map* m;
    m = malloc(sizeof(map));
    m->key = malloc(sizeof(char*));

    m->cnt = 0;
    

    
    return 0;
}
