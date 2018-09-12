// Compile with
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h> /* strtol */
#include <math.h>
#include <fenv.h>
#include <string.h>
#pragma STDC FENV_ACCESS on


#define DEBUG_PRINT 0

#define MAX_NUM_OPERANDS 3 // 3 for MulAdd

union operand {
  float f;
  uint32_t i;
};

uint8_t num_operands = 0;
union operand inputs[MAX_NUM_OPERANDS] = {0};
union operand result = {0};
int rounding_mode = 0;

/**
 * Expected input: "f16_add" "-rnear_even" "87FF" "E850" "E850" 
 * HW: f32_add -rnear_even 683F7FF C07F3FFF C07F3FFF
 * 
 */
int main(int argc, char *argv[])
{
  if (DEBUG_PRINT)
        printf("Got : %i arguments\n", argc);

  if (argc < 4) {
    printf("Not enought arguments\n");
    return 1;
  }

  char *op = argv[1];
  char *rounding_mode_str = argv[2];

  // TODO: the rounding modes don't really match:-/
  if (strcmp(rounding_mode_str, "-rnear-even") == 0)
  {
    rounding_mode = FE_TONEAREST;
  }
  if (strcmp(rounding_mode_str, "-rmin") == 0)
  {
    rounding_mode = FE_DOWNWARD;
  }
  if (strcmp(rounding_mode_str, "-rmax") == 0)
  {
    rounding_mode = FE_UPWARD;
  }
  if (strcmp(rounding_mode_str, "-rodd") == 0)
  {
    rounding_mode = FE_UPWARD;
  }

  feclearexcept(FE_ALL_EXCEPT);
  fesetround(rounding_mode);

// Only f32_add for now
  if (strcmp(op, "f32_add") == 0)
  {
    if (DEBUG_PRINT)
      printf("Got f32_add %s\n",op);
    num_operands = 2;
    for (uint8_t idx = 0; idx < num_operands; idx++)
    {
      inputs[idx].i = (uint32_t)strtol(argv[idx + 3], NULL, 16);
      if (DEBUG_PRINT)
        printf("input: %x\n", inputs[idx].i);
    }

    result.f = (float)inputs[0].f + (float)inputs[1].f;
  }

  if (num_operands == 0) {
    printf("Parse error\n");
    return 1;
  }

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

  printf("%x %x\n ", result.i, flags);
  return 0;
}
