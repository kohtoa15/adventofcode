#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fsio.h"
#include "chunk.h"

typedef  long unsigned int ulong;

// Set to 1 to get debug output
#define DEBUG 0
#define debug_trace(fmt, ...) \
  do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__, __LINE__, __func__, __VA_ARGS__); } while(0);
#define debug_print(fmt, ...) \
  do { if (DEBUG) fprintf(stderr, fmt, __VA_ARGS__); } while(0);

enum ScoreType { SYNTAX_CHECK, AUTOCOMPLETE, NONE };

struct Score {
  ulong value;
  enum ScoreType type;
};

struct Score read_chunks(char* s, size_t start, size_t end) {
  struct Score score;
  score.value = 0;
  score.type = NONE;

  ulong errscore = 0;
  // outer "pseudo-chunk" that finishes when we read newline
  struct OpenChunk** c = (struct OpenChunk**) malloc(sizeof(struct OpenChunk*));
  *c = open_chunk(0xa, NULL);
  for (size_t i = start; i < end; i++) {
    if (chunk_feed_char(c, s[i]) == 1) {
      debug_print("Corrupted: illegal char %lu: %c\n", i - start, s[i]);
      // calculate syntax error score of illegal character
      switch (s[i]) {
        case ')':
          errscore += 3;
          break;
        case ']':
          errscore += 57;
          break;
        case '}':
          errscore += 1197;
          break;
        case '>':
          errscore += 25137;
          break;
      }
      // stop after first incorrect char
      score.value = errscore;
      score.type = SYNTAX_CHECK;
      break;
    }
  }
  // try to complete chunk if not already
  if (score.type == NONE && *c != NULL && (*c)->parent_chunk != NULL) {
    char* completion = complete_chunk(c);
    debug_print("incomplete, missing: %s\n", completion);
    // calculate autocomplete score
    ulong ac_score = 0;
    int len = strlen(completion);
    for (int i = 0; i < len; i++) {
      ac_score = ac_score * 5;
      switch (completion[i]) {
        case ')':
          ac_score += 1;
          break;
        case ']':
          ac_score += 2;
          break;
        case '}':
          ac_score += 3;
          break;
        case '>':
          ac_score += 4;
          break;
      }
    }
    score.value = ac_score;
    score.type = AUTOCOMPLETE;
  }
  free(c);
  return score;
}

void list_insert_sorted(ulong* buf, int len, ulong newval) {
  // get insert index
  int insert = 0;
  for (int i = len - 1; i >= 0; i--) {
    if (newval > buf[i]) {
      insert = i + 1;
      break;
    }
  }
  // shift all ge insert to the right
  for (int i = len; i > insert; i--) {
    buf[i] = buf[i - 1];
  }
  // insert newval at the found index
  debug_print("Insert %lu at index %d (len %d)\n", newval, insert, len);
  buf[insert] = newval;
}

int main(int argc, char** argv) {
  if (argc != 2) {
    fprintf(stderr, "Invalid args\n");
    return -1;
  }
  char* filename = (char*) malloc(strlen(argv[1]) + 1);
  strcpy(filename, argv[1]);

  char* s = read_file_to_string(filename);
  size_t len = strlen(s);
  printf("%s -> %luB\n", filename, len);
  
  ulong total_errscore = 0;
  int ac_num = 64;
  int alloc_interval = ac_num;
  int ac_i = 0;
  ulong* ac_scores = (ulong*) calloc(ac_num, sizeof(ulong)); 

  size_t start = 0;
  size_t end = 0;
  int linecnt = 0;
  while (1) {
    end = next_line(s, len, start);
    debug_print("readline %lu->%lu\n", start, end);
    // if offset hasn't moved, we have reached EOF
    if (end == start) { break; }
    // read chunk data
    struct Score score = read_chunks(s, start, end);
    switch (score.type) {
      case SYNTAX_CHECK:
        // tally total syntax error scores
        printf("line %d: syntax error score = %lu\n", linecnt, score.value);
        total_errscore += score.value;
        break;
      case AUTOCOMPLETE:
        printf("line %d: autocomplete error score = %lu\n", linecnt, score.value);
        // keep all autocomplete scores as sorted list
        list_insert_sorted(ac_scores, ac_i++, score.value);
        // check if current buf is large enough
        if (ac_i == ac_num) {
          ac_num += alloc_interval;
          alloc_interval = alloc_interval * 2;
          debug_print("realloc ac_scores: %d\n", ac_num);
          ac_scores = (ulong*) realloc(ac_scores, sizeof(ulong) * ac_num);
        }
        break;
      case NONE:
        // nothing to do
        break;
    }
    // slide start ptr forward for next line
    start = end;
    linecnt++;
  }
  printf("\ntotal syntax error score: %lu\n", total_errscore);

  if (DEBUG) {
    for (int i = 0; i < ac_i; i++) {
      printf("%d: %lu\n", i, ac_scores[i]);
    }
  }
  ulong ac_median = ac_scores[ac_i / 2];
  printf("\nautocomplete score median: %lu\n", ac_median);

  return 0;
}
