#include <openssl/md5.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char* argv[]) {
    
    unsigned char digest[MD5_DIGEST_LENGTH];
    //char* string = "ckczppom";
    char* key = argv[1];

    int num = 1;

    char string[16];
    sprintf(string, "%s%i", key, num);

    MD5(string, strlen(string), digest);

    char md5string[33];
    for(int i = 0; i < 16; ++i)
        sprintf(&md5string[i*2], "%02x", (unsigned int)digest[i]);

    while (strncmp(md5string, "000000", 6) != 0) {

        sprintf(string, "%s%i", key, num);

        MD5(string, strlen(string), digest);

        for(int i = 0; i < 16; ++i)
            sprintf(&md5string[i*2], "%02x", (unsigned int)digest[i]);
        
        printf("%s\t%s\n", md5string, string);
        num++;
    }

    printf("%i", --num);
    //printf("%s", md5string);

}
