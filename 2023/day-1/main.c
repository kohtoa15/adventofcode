#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFSIZE 4096

char* readInput()
{
  // let's be lazy and just malloc a huge virtmem chunk
  size_t cap = BUFSIZE * BUFSIZE;
  char* str = (char*) malloc(cap);
  if (str == NULL) return NULL;
  
  size_t len = 0;
  char* buf = (char*)malloc(BUFSIZE);
  if (buf == NULL) return NULL;

  // read lines until eof
  while (0 < 1)
  {
    void* p = fgets(buf, BUFSIZE, stdin);
    if (p != NULL) {
      size_t nlen = strlen(buf);
      size_t next_len = len + nlen;
      if (next_len > cap) {
        free(str);
        free(buf);
        fprintf(stderr, "Input larger than 16MiB!");
        return NULL;
      }
      strncpy(str + len, buf, nlen);
      len = next_len;
      str[len] = '\n';
    } else {
      break;
    }
  }
  free(buf);
  return str;
}

int digit(char c) {
  int i = c - '0';
  return i >= 0 && i <= 9 ? i : -1;
}

char* letteredDigits[] = {
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine",
};
int isLetteredDigit(char* c, size_t len) {
  if (len < 3) return -1;
  char buf[1024];
  strncpy(buf, c, len);
  buf[len] = '\0';

  for (size_t i = 0; i < 9; i++) {
    size_t dlen = strlen(letteredDigits[i]);
    if (len < dlen)
      continue;

    char* s = buf + len - dlen;
    if (strcmp(letteredDigits[i], s) == 0)
    {
      return i + 1;
    }
  }
  return -1;
}

int* getCalibrations(char* str)
{
  size_t k = 1;
  size_t cap = BUFSIZE / sizeof(int);
  int* cals = (int*)malloc(cap);
  if (cals == NULL) return NULL;

  int current = -11;
  size_t len = strlen(str);
  size_t lineStart = 0;
  for (size_t i = 0; i < len; i++)
  {
    if (str[i] == '\n')
    {
      if (k == cap)
      {
        fprintf(stderr, "Reached max %lu lines!", cap);
        free(cals);
        return NULL;
      }
      // store calibration for line
      if (current > 0) {
        cals[k++] = current;
        // reset current var
        current = -11;
      }
      lineStart = i + 1;
      continue;
    }

    int d1 = digit(str[i]);
    int d2 = isLetteredDigit(str + lineStart, i - lineStart + 1);
    int d = d2 >= 0 ? d2 : d1;
    if (d < 0)
      continue;

    current = current / 10;
    if (current < 0)
      current = d;
    current *= 10;
    current += d;
  }
  cals[0] = k;
  return cals;
}

int main()
{
  char* s = readInput();
  if (s == NULL) return 1;

  int* c = getCalibrations(s);
  if (c == NULL) return 1;

  int sum = 0;
  for (size_t i = 1; i < c[0]; i++)
  {
    sum += c[i];
  }
  fprintf(stdout, "=> %d\n", sum);
}
