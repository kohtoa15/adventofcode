#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "fsio.h"
#include "alu.h"

#define INPUT_DIGITS 14

int main(int argc, char** argv)
{
  char* filename;
  if (argc == 2)
  {
    size_t len = strlen(argv[1]);
    filename = (char*) malloc(len + 1);
    strcpy(filename, argv[1]);
  } else {
    printf("Invalid args. (2 required)\n");
    return -1;
  }

  char* content = read_file_to_string(filename);
  size_t len = strlen(content);
  printf("Read file %s (length: %lu)\n", filename, len);

  char input_str[INPUT_DIGITS + 1];
  int inputs[INPUT_DIGITS];

  char line[64];
  char* parts[8];

  // find the largest valid fourteen-digit model number that contains no 0 digits
  for (long int i = 99999999999999; i >= 11111111111111; i--)
  {
    sprintf((char*) &input_str, "%0*ld", INPUT_DIGITS, i);
    int skip_round = 0;
    for (int k = 0; k < INPUT_DIGITS; k++)
    {
      inputs[k] = input_str[k] - '0';
      // check that we have no 0 digits
      if (inputs[k] == 0)
      {
        skip_round = 1;
        break;
      }
    }
    if (skip_round)
      continue;

    printf("\r%s", input_str);

    ALU alu = alu_init_from_input(INPUT_DIGITS, inputs);

    size_t offset = 0;
    size_t line_end = 0;

    while (offset < len) {
      // print_alu_registers(&alu);

      line_end = next_line(content, len, offset);
      // printf("Next line, offset: [%lu -> %lu]\n", offset, line_end);
      if (line_end == offset)
      {
        // skip further parsing on empty line
        break;
      }

      strncpy((char*) &line, content + offset, line_end - offset);
      line[line_end - offset] = 0; // EOL
      // printf("+ %s\n", line);

      char* sep = " ";
      size_t part_cnt = 0;
      char* ptr = strtok((char*) &line, sep);
      while (ptr != NULL)
      {
        // printf("Next part: %s\n", ptr);
        parts[part_cnt] = (char*) malloc(strlen(ptr) + 1);
        strcpy(parts[part_cnt], ptr);
        part_cnt++;
        ptr = strtok(NULL, sep);
      }
      int res = parse_instruction(&alu, part_cnt, parts);
      if (res != 0)
      {
        // Error with execution
        printf("command '%s' failed: %d\n", line, res);
        return -1;
      }
    
      offset = line_end + 1;
    }
    // print_alu_registers(&alu);

    // check z register at the end
    if (alu.z == 0)
    {
      printf("\nlargest model number accepted by MONAD: %ld\n", i);
      break;
    }
  }

  free(content);
  return 0;
}
