
struct OpenChunk {
  char close_parenthesis;
  struct OpenChunk* parent_chunk;
};

struct OpenChunk* open_chunk(char closing, struct OpenChunk* parent);

int chunk_feed_char(struct OpenChunk** cur, char next);

char* complete_chunk(struct OpenChunk **cur);
