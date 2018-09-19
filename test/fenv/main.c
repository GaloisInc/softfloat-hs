// Compile with
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include <fenv.h>
#include <string.h>
#pragma STDC FENV_ACCESS on

#define DEBUG_PRINT 0
#define MAX_NUM_OPERANDS 3 // 3 for MulAdd

union operand {
  float f32;    // 32bit
  uint32_t i32; // 32bit
  uint32_t i[2]; // 2x 32bit
  double f64;   // 64 bit
  uint64_t i64; // 64bit
};

enum arch
{
  F16,
  F32,
  F64
};

enum op
{
  ADD,
  SUB,
  DIV,
  MUL,
  REM,
  SQRT,
  LE,
  MUL_ADD,
  EQ,
  LT,
  EQ_SIGNALING,
  LE_QUIET,
  LT_QUIET,
  F16_TO_F32,
  F16_TO_F64,
  F32_TO_F16,
  F32_TO_F64,
  F64_TO_F16,
  F64_TO_F32,
};

enum arch current_arch = F32;
enum op current_op = ADD;
union operand inputs[MAX_NUM_OPERANDS] = {0};
union operand result = {0};
int rounding_mode = 0;


// TODO: the rounding modes don't really match:-/
void set_rounding_mode(char *rounding_mode_str)
{
  if (strcmp(rounding_mode_str, "-rnear_even") == 0)
  {
    rounding_mode = FE_TONEAREST;
  }
  else if (strcmp(rounding_mode_str, "-rmin") == 0)
  {
    rounding_mode = FE_DOWNWARD;
  }
  else if (strcmp(rounding_mode_str, "-rmax") == 0)
  {
    rounding_mode = FE_UPWARD;
  }
  else if (strcmp(rounding_mode_str, "-rodd") == 0)
  {
    rounding_mode = FE_TOWARDZERO;
  } else {
    printf("Error, unknown rounding mode\n");
  }

  feclearexcept(FE_ALL_EXCEPT);
  fesetround(rounding_mode);
}

// get exception flags
uint8_t get_flags(void)
{
  uint8_t flags = 0;
  if (DEBUG_PRINT)
    printf("flags:");
  if (fetestexcept(FE_INEXACT))
  {
    if (DEBUG_PRINT)
      printf("x");
    flags |= 0x1;
  }
  if (fetestexcept(FE_UNDERFLOW))
  {
    if (DEBUG_PRINT)
      printf("u");
    flags |= 0x2;
  }
  if (fetestexcept(FE_OVERFLOW))
  {
    if (DEBUG_PRINT)
      printf("o");
    flags |= 0x4;
  }
  if (fetestexcept(FE_DIVBYZERO))
  {
    if (DEBUG_PRINT)
      printf("i");
    flags |= 0x8;
  }
  if (fetestexcept(FE_INVALID))
  {
    if (DEBUG_PRINT)
      printf("v");
    flags |= 0x10;
  }
  if (DEBUG_PRINT)
    printf("\n");

  if (DEBUG_PRINT)
    printf("result: ");

  return flags;
}

/*
// execute operation
void execute_op(char *op)
{
  if (strcmp(op, "f32_add") == 0)
  {
    if (DEBUG_PRINT)
      printf("Got f32_add %s\n", op);
    result.f = (float)inputs[0].f + (float)inputs[1].f;
  }
}


  // parse operands
  for (uint8_t idx = 3; idx < argc; idx++)
  {
    inputs[idx - 3].i = (uint32_t)strtol(argv[idx], NULL, 16);
    if (DEBUG_PRINT)
      printf("input: %x\n", inputs[idx].i);
  }

*/

/**
 * Expected input: op rounding_mode operand_1 ... operand_n
 * 
 * HW1: f32_add -rnear_even  683F7FF C07F3FFF C07F3FFF
 * HW2: f32_add -rnear_even 7FADC332 FFFFFFFE 7FEDC332
 * HW3: "ui32_to_f16" "-rnear_even" "FF7BBFFE","7C00"]
 * 
 */
int main(int argc, char *argv[])
{
  if (DEBUG_PRINT)
    printf("Got : %i arguments\n", argc);

  if (argc < 4)
  {
    printf("Not enought arguments\n");
    return 1;
  }

  char *op = argv[1];
  set_rounding_mode(argv[2]);

  if (strcmp(op, "f16_to_f32") == 0)
  {
    if (DEBUG_PRINT)
      printf("Unsupported operation\n");
    return 1;
  }
  else if (strcmp(op, "f16_to_f64") == 0)
  {
    if (DEBUG_PRINT)
      printf("Unsupported operation\n");
    return 1;
  }
  else if (strcmp(op, "f32_to_f16") == 0)
  {
    if (DEBUG_PRINT)
      printf("Unsupported operation\n");
    return 1;
  }
  else if (strcmp(op, "f32_to_f64") == 0)
  {
    inputs[0].i32 = (uint32_t)strtol(argv[3], NULL, 16);
    result.f64 = (double)inputs[0].i32;
    uint8_t flags = get_flags();
    printf("%08x%08x %x\n ", result.i[1], result.i[0], flags);
    //printf("%lf\n", result.f64);
  }
  else if (strcmp(op, "f64_to_f16") == 0)
  {
    if (DEBUG_PRINT)
      printf("Unsupported operation\n");
    return 1;
  }
  else if (strcmp(op, "f64_to_f32") == 0)
  {
    //inputs[0].i64 = (uint64_t)strtol(argv[3], NULL, 16);
    inputs[0].i64 = (uint64_t)strtol(argv[3], NULL, 16);
    //printf("%lf\n", inputs[0].f64);
    result.f32 = (float)inputs[0].f64;
    uint8_t flags = get_flags();
    printf("%x %x\n ", result.i32, flags);
    //printf("%f\n", result.f32);
  }
  return 0;
}

/*
float64_t subj_ui32_to_f64( uint32_t a )
{
    union f64_d uZ;

    uZ.d = a;
    return uZ.f64;

}
*/
