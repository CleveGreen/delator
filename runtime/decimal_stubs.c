#define _POSIX_C_SOURCE 200809L

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#endif

#if defined(__GNUC__) && (defined(__x86_64__) || defined(__i386__))
#include <cpuid.h>
#include <x86intrin.h>
#define DELATOR_X86_TSC 1
#else
#define DELATOR_X86_TSC 0
#endif

static value write_decimal(value destination, uint64_t magnitude, int negative)
{
  if (caml_string_length(destination) < 21) return Val_long(-1);

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

static uint64_t monotonic_nanoseconds(void)
{
#ifdef _WIN32
  LARGE_INTEGER counter;
  LARGE_INTEGER frequency;
  QueryPerformanceCounter(&counter);
  QueryPerformanceFrequency(&frequency);
  return ((uint64_t)counter.QuadPart / (uint64_t)frequency.QuadPart) * UINT64_C(1000000000)
    + (((uint64_t)counter.QuadPart % (uint64_t)frequency.QuadPart) * UINT64_C(1000000000))
      / (uint64_t)frequency.QuadPart;
#else
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return 0;
  return (uint64_t)now.tv_sec * UINT64_C(1000000000) + (uint64_t)now.tv_nsec;
#endif
}

CAMLprim value delator_monotonic_now_ns(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(caml_copy_int64((int64_t)monotonic_nanoseconds()));
}

CAMLprim int64_t delator_monotonic_now_ns_unboxed(int64_t unit)
{
  (void)unit;
  return (int64_t)monotonic_nanoseconds();
}

#if DELATOR_X86_TSC
static int tsc_supported(void)
{
  unsigned int maximum = __get_cpuid_max(0x80000000, NULL);
  unsigned int eax;
  unsigned int ebx;
  unsigned int ecx;
  unsigned int edx;
  if (maximum < 0x80000007) return 0;
  __cpuid(0x80000007, eax, ebx, ecx, edx);
  return (edx & (1U << 8)) != 0;
}

static uint64_t read_tsc(void)
{
  return __rdtsc();
}
#endif

CAMLprim value delator_tsc_supported(value unit)
{
  (void)unit;
#if DELATOR_X86_TSC
  return Val_bool(tsc_supported());
#else
  return Val_false;
#endif
}

CAMLprim value delator_tsc_now(value unit)
{
  CAMLparam1(unit);
#if DELATOR_X86_TSC
  CAMLreturn(caml_copy_int64((int64_t)read_tsc()));
#else
  CAMLreturn(caml_copy_int64(0));
#endif
}

CAMLprim int64_t delator_tsc_now_unboxed(int64_t unit)
{
  (void)unit;
#if DELATOR_X86_TSC
  return (int64_t)read_tsc();
#else
  return 0;
#endif
}

CAMLprim value delator_tsc_nanoseconds_per_tick(value unit)
{
  CAMLparam1(unit);
  double scale = 0.0;
#if DELATOR_X86_TSC
  if (tsc_supported()) {
    uint64_t started_ns = monotonic_nanoseconds();
    uint64_t started_tsc = read_tsc();
#ifdef _WIN32
    Sleep(5);
#else
    struct timespec delay = { .tv_sec = 0, .tv_nsec = 5000000 };
    while (nanosleep(&delay, &delay) != 0 && errno == EINTR) {}
#endif
    uint64_t finished_tsc = read_tsc();
    uint64_t finished_ns = monotonic_nanoseconds();
    if (finished_tsc > started_tsc && finished_ns > started_ns)
      scale = (double)(finished_ns - started_ns)
        / (double)(finished_tsc - started_tsc);
  }
#endif
  CAMLreturn(caml_copy_double(scale));
}
