#include <stdio.h>
#include <string.h>

int check_string(char* string) {

    if (strstr(string, "ab") != NULL) return 1;
    if (strstr(string, "cd") != NULL) return 1;
    if (strstr(string, "pq") != NULL) return 1;
    if (strstr(string, "xy") != NULL) return 1;

    char* copy = string;

    if ((copy = strpbrk(copy, "aeiou")) == NULL) return 1;
    copy++;
    if ((copy = strpbrk(copy, "aeiou")) == NULL) return 1;
    copy++;
    if ((copy = strpbrk(copy, "aeiou")) == NULL) return 1;

    int not_found = 1;
    for (int i = 0; i < strlen(string); i++) {
        if (string[i] == string[i+1]) not_found = 0;
    }

    return not_found;

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


