#include <stdint.h>
#include <string.h>

extern const char** get_strings(uint32_t *count) {
  static const char* strings[] = {"string_1", "string_2"};

  *count = sizeof(strings) / sizeof(strings[0]);

  return strings;
}

extern uint32_t count_strings_chars(const char* const* strings, uint32_t count) {
  uint32_t result = 0;

  for (uint32_t i = 0; i < count; ++i) {
    result += strlen(strings[i]);
  }

  return result;
}

struct chars_strings_holder {
  const char* const* strings;
  uint32_t count;
};

extern uint32_t count_strings(const struct chars_strings_holder* holder) {
  return count_strings_chars(holder->strings, holder->count);
}
