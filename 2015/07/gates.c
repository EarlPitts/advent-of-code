#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NOT 1
#define AND 2
#define OR 3
#define LSHIFT 4
#define RSHIFT 5

struct wire {
    char* id;
    int value;
};

typedef struct {
    int gate;
    char* in1;
    char* in2;
    char* out; 
} instruction;

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
            if (i.in1 == NULL) i.in1 = next;
            else i.in2 = next;
        }

        next = strtok(NULL, " ");
    }

    i.out = strtok(NULL, "\n");

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

    

    return 0;
}