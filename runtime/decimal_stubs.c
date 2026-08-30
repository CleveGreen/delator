#include <caml/alloc.h>
#include <caml/mlvalues.h>

#include <stdint.h>
#include <string.h>

static value write_decimal(value destination, uint64_t magnitude, int negative)
{
  char temporary[21];
  char *end = temporary + sizeof(temporary);
  char *cursor = end;

  do {
    uint64_t quotient = magnitude / 10;
    *--cursor = (char)('0' + (magnitude - (quotient * 10)));
    magnitude = quotient;
  } while (magnitude != 0);

  if (negative) *--cursor = '-';

  size_t length = (size_t)(end - cursor);
  memcpy(Bytes_val(destination), cursor, length);
  return Val_long(length);
}

CAMLprim value delator_write_int(value destination, value input)
{
  intnat signed_value = Long_val(input);
  uintnat magnitude = (uintnat)signed_value;
  int negative = signed_value < 0;
  if (negative) magnitude = (uintnat)0 - magnitude;
  return write_decimal(destination, (uint64_t)magnitude, negative);
}

CAMLprim value delator_write_int64(value destination, value input)
{
  int64_t signed_value = Int64_val(input);
  uint64_t magnitude = (uint64_t)signed_value;
  int negative = signed_value < 0;
  if (negative) magnitude = (uint64_t)0 - magnitude;
  return write_decimal(destination, magnitude, negative);
}
