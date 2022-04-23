#include <stdlib.h>

#include "chunk.h"

struct OpenChunk* open_chunk(char closing, struct OpenChunk *parent) {
  struct OpenChunk* c = (struct OpenChunk*) malloc(sizeof(struct OpenChunk));
  c->parent_chunk = parent;
  c->close_parenthesis = closing;
  return c;
}

struct OpenChunk* pop_chunk(struct OpenChunk* cur) {
  struct OpenChunk* parent = cur->parent_chunk;
  free(cur);
  return parent;
}

int chunk_feed_char(struct OpenChunk** cur, char next) {
  struct OpenChunk* open = *cur;
  // try closing current chunk
  if (open != NULL && next == open->close_parenthesis) {
    // close and discard cur chunk and move to parent
    *cur = pop_chunk(open);
    return 0;
  }
  // try opening new chunk with different parenthesis
  if (next == '(') {
    *cur = open_chunk(')', open);
    return 0;
  }
  if (next == '[') {
    *cur = open_chunk(']', open);
    return 0;
  }
  if (next == '{') {
    *cur = open_chunk('}', open);
    return 0;
  }
  if (next == '<') {
    *cur = open_chunk('>', open);
    return 0;
  }
  // Invalid char
  return 1;
}

char* complete_chunk(struct OpenChunk** cur) {
  // add needed chars until the next needed closing paren is newline
  int num_open = 0;
  // get number of chunks that need to be closed
  struct OpenChunk* cnt = *cur;
  while (cnt->parent_chunk != NULL) {
    num_open++;
    cnt = cnt->parent_chunk;
  }
  // create buffer for return chars
  char* buf = (char*) malloc(sizeof(char) * num_open + 1);
  buf[num_open] = 0x0;
  // go through chunks and get each char needed for closing
  struct OpenChunk* open = *cur;
  int i = 0;
  while (open != NULL && open->close_parenthesis != 0xa) {
    buf[i++] = open->close_parenthesis;
    open = open->parent_chunk;
  }
  buf[i] = 0x0;

  return buf;
}
