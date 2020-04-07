#include <stdio.h>
#include <string.h>

int check_string(char* string) {

    char* copy = string + 2;
    char two_char[3];
    int found = 0;

    for (int i = 0; i < strlen(string) - 1; i++) {

    strncpy(two_char,  copy - 2, 2);
    
    if (strstr(copy, two_char) != NULL) found = 1;

    copy += 1;
    }

    if (found == 0) return 1;

    found = 0;

    for (int i = 0; i < strlen(string) - 3; i++) {
        if (string[i] == string[i+2]) found = 1;
    }

    if (found == 0) return 1;
    return 0;

}

int main() {

    FILE* fp = fopen("input.txt", "r");
    
    char line[100];
    int cnt = 0;

    while (fgets(line, sizeof(line), fp) != NULL)
        if (!check_string(line)) cnt++;

    printf("%i", cnt);

    return 0;
}


