#include <stdio.h>

typedef struct ALU {
  int* inputs;
  int input_cnt;
  int input_len;

  int w;
  int x;
  int y;
  int z;
} ALU;

ALU alu_init_from_input(int ic, int* iv);

void print_alu_registers(ALU* a);

int alu_inp(ALU* alu, int wordc, char** wordv);

int parse_instruction(ALU* alu, int wordc, char** wordv);

