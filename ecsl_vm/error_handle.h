#ifndef ERROR_HANDLE_H
#define ERROR_HANDLE_H

#include <stdbool.h>

void throw_error(char* error_message);

void throw_error_assert(char* error_message, bool condition);

#endif
