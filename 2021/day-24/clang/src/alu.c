#include "alu.h"

#include <string.h>
#include <stdlib.h>

ALU alu_init_from_input(int ic, int* iv)
{
  ALU alu;
  alu.inputs = iv;
  alu.input_cnt = 0;
  alu.input_len = ic;

  // default start values for 'registers'
  alu.w = 0;
  alu.x = 0;
  alu.y = 0;
  alu.z = 0;
  return alu;
}

void print_alu_registers(ALU* a)
{
  printf("=| ALU w=%d x=%d y=%d z=%d\n", a->w, a->x, a->y, a->z);
}

int* alu_access_register(ALU* alu, char identifier)
{
  int* ptr = NULL;
  switch (identifier)
  {
    case 'w':
      ptr = &alu->w;
      break;
    case 'x':
      ptr = &alu->x;
      break;
    case 'y':
      ptr = &alu->y;
      break;
    case 'z':
      ptr = &alu->z;
      break;
  }
  return ptr;
}

int alu_inp(ALU* alu, int wordc, char** wordv)
{
  int ret = -1;
  if (wordc == 2)
  {
    if (alu->input_cnt == alu->input_len)
    {
      // can't pop next input
      return -3;
    }
    int* var = alu_access_register(alu, wordv[1][0]);
    if (var == NULL)
    {
      return -2;
    }
    *var = alu->inputs[alu->input_cnt++];
    ret = 0;
  }
  return ret;
}

typedef struct _TuplePtr
{
  int* a;
  int* b;
  char clear_a;
  char clear_b;
} TuplePtr;

void clear_tuple_ptr(TuplePtr* ptr)
{
  if (ptr != NULL)
  {
    if (ptr->clear_a)
      free(ptr->a);
    if (ptr->clear_b)
      free(ptr->b);
    free(ptr);
  }
}

// @return ptr to two separate int ptr (var a and var b)
TuplePtr* parse_two_vars(ALU* alu, char* a, char* b)
{
  char b_on_heap = 0;
  // var a is required to be a register
  int* var_a = alu_access_register(alu, a[0]);
  if (var_a == NULL)
  {
    printf("ERR: var a is not a valid register: '%s'\n", a);
    return NULL;
  }

  int* var_b = alu_access_register(alu, b[0]);
  if (var_b == NULL)
  {
    // try to parse word b as int val
    int val = atoi(b);
    // check if value is actually zero or invalid
    if (val == 0 && strcmp(b, "0") != 0)
    {
      printf("ERR: var b is not a valid register or number: '%s'\n", b);
      return NULL;
    }
    var_b = (int*) malloc(sizeof(int));
    b_on_heap = 1;
    *var_b = val;
  }

  TuplePtr* ret = (TuplePtr*) malloc(sizeof(TuplePtr));
  ret->a = var_a;
  ret->b = var_b;
  ret->clear_a = 0;
  ret->clear_b = b_on_heap;
  return ret;
}

int alu_add(ALU* alu, int wordc, char** wordv)
{
  int ret = -1;
  if (wordc == 3)
  {
    TuplePtr* vars = parse_two_vars(alu, wordv[1], wordv[2]);
    if (vars == NULL)
    {
      ret = -2;
    }
    else
    {
      // add value of var b to var a
      *vars->a += *vars->b;
      clear_tuple_ptr(vars);
      ret = 0;
    }
  }
  return ret;
}

int alu_mult(ALU* alu, int wordc, char** wordv)
{
  int ret = -1;
  if (wordc == 3)
  {
    TuplePtr* vars = parse_two_vars(alu, wordv[1], wordv[2]);
    if (vars == NULL)
    {
      ret = -2;
    }
    else
    {
      // multiply values a and b and store in a
      *vars->a *= *vars->b;
      clear_tuple_ptr(vars);
      ret = 0;
    }
  }
  return ret;
}

int alu_div(ALU* alu, int wordc, char** wordv)
{
  int ret = -1;
  if (wordc == 3)
  {
    TuplePtr* vars = parse_two_vars(alu, wordv[1], wordv[2]);
    if (vars == NULL)
    {
      ret = -2;
    }
    else
    {
      // divide a by b and store result in a
      *vars->a /= *vars->b;
      clear_tuple_ptr(vars);
      ret = 0;
    }
  }
  return ret;
}

int alu_mod(ALU* alu, int wordc, char** wordv)
{
  int ret = -1;
  if (wordc == 3)
  {
    TuplePtr* vars = parse_two_vars(alu, wordv[1], wordv[2]);
    if (vars == NULL)
    {
      ret = -2;
    }
    else
    {
      // divide a by b and store the reminder in a
      *vars->a = *vars->a % *vars->b;
      clear_tuple_ptr(vars);
      ret = 0;
    }
  }
  return ret;
}

int alu_eql(ALU* alu, int wordc, char** wordv)
{
  int ret = -1;
  if (wordc == 3)
  {
    TuplePtr* vars = parse_two_vars(alu, wordv[1], wordv[2]);
    if (vars == NULL)
    {
      ret = -2;
    }
    else
    {
      // compare equality of a and b and store bool result in a
      *vars->a = *vars->a == *vars->b;
      clear_tuple_ptr(vars);
      ret = 0;
    }
  }
  return ret;
}

int parse_instruction(ALU* alu, int wordc, char** wordv)
{
  int ret = 0;
  if (wordc > 0) {
    if (strcmp(wordv[0], "inp") == 0)
      ret = alu_inp(alu, wordc, wordv);
    else if (strcmp(wordv[0], "add") == 0)
      ret = alu_add(alu, wordc, wordv);
    else if (strcmp(wordv[0], "mul") == 0)
      ret = alu_mult(alu, wordc, wordv);
    else if (strcmp(wordv[0], "div") == 0)
      ret = alu_div(alu, wordc, wordv);
    else if (strcmp(wordv[0], "mod") == 0)
      ret = alu_mod(alu, wordc, wordv);
    else if (strcmp(wordv[0], "eql") == 0)
      ret = alu_eql(alu, wordc, wordv);
  }
  return ret;
}
