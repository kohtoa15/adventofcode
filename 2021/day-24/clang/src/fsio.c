#include "fsio.h"

char * read_file_to_string(const char* filename)
{
  FILE* fp = fopen(filename, "r");

  if (fp == NULL)
  {
    fprintf(stderr, "Could not open file '%s'\n", filename);
    return NULL;
  }

  // get file size
  fseek(fp, 0, SEEK_END);
  long int fsize = ftell(fp);
  // reset file ptr to start
  fseek(fp, 0, SEEK_SET);

  // realloc required mem
  char* s = (char*) malloc(fsize);
  size_t n = fread(s, 1, fsize, fp);

  if (n != fsize)
  {
    fprintf(stderr, "Could not read entire file content: %li != %lu", fsize, n);
  }

  fclose(fp);
  return s;
}

size_t next_line(char* stream, size_t len, size_t offset)
{
  if (stream == NULL)
  {
    fprintf(stderr, "Could not read from stream\n");
    return 0;
  }

  size_t cursor = offset;
  // skip if cursor on newline
  if (stream[cursor] == 0xa) { cursor++; }

  // increase cursor until newline or EOF
  while (stream[cursor] != 0x0 && stream[cursor] != 0xa && cursor < len) { cursor++; }

  return cursor;
}
