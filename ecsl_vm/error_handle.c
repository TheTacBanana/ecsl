#include "error_handle.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define RED "\e[0;31m"
#define RESET "\e[0m"

// Return from the process while outputting an error message
void throw_error(char *error_message) {
  printf(RED "Error: " RESET "%s", error_message);
  exit(1);
}

void throw_error_assert(char *error_message, bool assert) {
  if (!assert) {
    printf(RED "Error: " RESET "%s", error_message);
    exit(1);
  }
}
