#include <stdio.h>
#include <stdint.h>

void mod_chez_str(char * str, int32_t len) {
    printf("before modify, str: %s\n", str);

    snprintf(str, len, "????");

    printf("after modify, str: %s\n", str);

    return;
}
