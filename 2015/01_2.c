#include <stdio.h>

int main()
{

    FILE *fp;

    fp = fopen("input", "r");
    int next;
    int count = 0;
    int pos = 0;

    while ((next = fgetc(fp)) != EOF)
    {
        pos++;

        switch (next) {
        case '(':
            count++;
            break;

        case ')':
            count--;
            break;
        }

        if (count < 0) {
            printf("%i", pos);
            return 0;
        }
    }

    printf("%i", count);
}
