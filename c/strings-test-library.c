#include <stdint.h>
#include <string.h>

const char* const* get_strings(uint32_t *count) {
  static const char* strings[] = {"string_1", "string_2"};

  *count = sizeof(strings) / sizeof(strings[0]);

  return strings;
}

uint32_t count_strings_chars(const char* strings[], uint32_t count) {
  uint32_t result = 0;

  for (uint32_t i = 0; i < count; ++i) {
    result += strlen(strings[i]);
  }

  return result;
}
