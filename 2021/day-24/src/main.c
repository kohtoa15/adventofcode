#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fsio.h"

typedef struct ALU {
  int* inputs;
  int input_cnt;
  int input_len;

  int w;
  int x;
  int y;
  int z;
} ALU;

int alu_inp(ALU* alu, int wordc, char** wordv)
{
  if (wordc == 2)
  {
    if (input_cnt == input_len)
    {
      // can't pop next input
      return -3;
    }
    if (strcmp(wordv[1], "w") == 0)
    {
      alu->w = alu->inputs[alu->input_cnt++];
    } else if (strcmp(wordv[1], "x") == 0)
    {
      alu->x = alu->inputs[alu->input_cnt++];
    } else if (strcmp(wordv[1], "y") == 0)
    {
      alu->y = alu->inputs[alu->input_cnt++];
    } else if (strcmp(wordv[1], "z") == 0)
    {
      alu->z = alu->inputs[alu->input_cnt++];
    } else {
      return -2;
    }
  }
  return -1;
}

int parse_instruction(ALU* alu, int wordc, char** wordv)
{
  int ret = 0;
  if (wordc > 0) {
    if (strcmp(wordv[0], "inp") == 0)
    {
      ret = alu_inp(alu, wordc, wordv);
    }
    else if (strcmp(wordv[0], "add") == 0)
    {}
    else if (strcmp(wordv[0], "mul") == 0)
    {}
    else if (strcmp(wordv[0], "div") == 0)
    {}
    else if (strcmp(wordv[0], "mod") == 0)
    {}
    else if (strcmp(wordv[0], "eql") == 0)
    {}
  }
  return ret
}

int main(int argc, char** argv)
{
  char* filename;
  if (argc > 1)
  {
    size_t len = strlen(argv[1]);
    filename = (char*) malloc(len + 1);
    strcpy(filename, argv[1]);
  } else {
    filename = "input";
  }

  char* content = read_file_to_string(filename);

  size_t offset = 0;
  size_t line_end = 0;
  size_t len = strlen(content);
  printf("Read file %s (length: %lu)\n", filename, len);

  ALU alu;

  while (offset < len) {
    line_end = next_line(content, len, offset);
    printf("Next line, offset: [%lu -> %lu]\n", offset, line_end);
    if (line_end == offset)
    {
      // skip further parsing on empty line
      break;
    }

    char* line = (char*) malloc(sizeof(char) * (line_end - offset + 1));
    strncpy(line, content + offset, line_end - offset);

    char* sep = " ";
    char** parts = (char**) malloc(sizeof(char*) * 4);
    size_t part_cnt = 0;
    char* ptr = strtok(line, sep);
    while (ptr != NULL)
    {
      ptr = strtok(NULL, sep);
      if (ptr != NULL)
      {
        printf("Next part: %s\n", ptr);
        parts[part_cnt] = (char*) malloc(strlen(ptr) + 1);
        strcpy(parts[part_cnt], ptr);
        part_cnt++;
      }
    }
    int res = parse_instruction(&alu, part_cnt, parts);
    free(parts);
    free(line);
  
    offset = line_end + 1;
  }

  free(content);
  return 0;
}
