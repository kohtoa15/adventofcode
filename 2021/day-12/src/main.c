#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "gpath.h"
#include "fsio.h"

// Set to 1 to get debug output
#define DEBUG 0
#define debug_trace(fmt, ...) \
  do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__, __LINE__, __func__, __VA_ARGS__); } while(0);
#define debug_print(fmt, ...) \
  do { if (DEBUG) fprintf(stderr, fmt, __VA_ARGS__); } while(0);

size_t read_path(struct Graph* g, size_t offset, char* stream) {
  // read next line
  size_t ret_offset = next_line(stream, strlen(stream), offset);
  // when we have at least 3 chars, we split by dash
  if (ret_offset - offset > 2) {
    int split_at = 0;
    for (int i = offset; i < ret_offset; i++) {
      if (stream[i] == '-') {
        split_at = i;
        break;
      }
    }
    if (split_at > 0 && split_at < (ret_offset - 1)) {
      int first_len = split_at - offset;
      int second_len = ret_offset - split_at - 1;
      // use string buffer on stack for copies
      char strbuf[64];
      if (first_len >= 64 || second_len >= 64) {
        perror("node names are too long for strbuf!\n");
        return offset;
      }

      struct GraphNode* node1;
      struct GraphNode* node2;
      // try to insert first node
      char* stream_first = (char*) stream + offset;
      strncpy(strbuf, stream_first, first_len);
      strbuf[first_len] = 0;
      if ((node1 = graph_find_node(g, strbuf)) == NULL) {
        // first not yet in graph
        node1 = graph_insert_node(g, strbuf);
        // allow recursion when first char is uppercase
        if (strbuf[0] >= 'A' && strbuf[0] <= 'a') {
          node1->allow_recursion = true;
        }
      }

      // try to insert second node, reuse string buffer
      char* stream_second = (char*) stream + split_at + 1;
      strncpy(strbuf, stream_second, second_len);
      strbuf[second_len] = 0;
      if ((node2 = graph_find_node(g, strbuf)) == NULL) {
        // second not yet in graph
        node2 = graph_insert_node(g, strbuf);
        // allow recursion when first char is uppercase
        if (strbuf[0] >= 'A' && strbuf[0] <= 'a') {
          node2->allow_recursion = true;
        }
      }

      // insert edge between the nodes
      graph_insert_edge(g, node1->name, node2->name);
    }
  }
  return ret_offset;
}

int main(int argc, char** argv) {
  if (argc != 2) {
    printf("invalid args\n");
    return -1;
  }
  char* filename = (char*) malloc(sizeof(char) * (strlen(argv[1]) + 1));
  strcpy(filename, argv[1]);
  // read file to mem buf
  char* s = read_file_to_string(filename);
  // init graph
  struct Graph g = graph_init(64, 64);
  // read graph input lines
  size_t offset = 0;
  size_t line_len = 0;
  while (line_len > 0 || offset == 0) {
    size_t n = read_path(&g, offset, s);
    line_len = n - offset;
    offset = n + 1;
    // printf("offset: %lu\n", offset);
  }

  // debug: print nodes and edges
  printf("* Nodes: (%d)\n", g.nodes_num);
  for (int i = 0; i < g.nodes_num; i++) {
    struct GraphNode* node = g.nodes[i];
    char* name;
    if (node != NULL) {
      name = node->name;
    } else {
      name = "<null>";
    }
    printf("  '%s'\n", name);
  }
  printf("* Edges: (%d)\n", g.edges_num);
  for (int i = 0; i < g.edges_num; i++) {
    struct GraphEdge* edge = g.edges[i];
    if (edge != NULL) {
      char* first;
      if (edge->node_a != NULL) {
        first = edge->node_a->name;
      } else {
        first = "<null>";
      }

      char* second;
      if (edge->node_b != NULL) {
        second = edge->node_b->name;
      } else {
        second = "<null>";
      }

      printf("  '%s' - '%s'\n", first, second);
    } else {
      printf("  <null>\n");
    }
  }
  
  // find path from start -> end
  struct GraphNode* start = graph_find_node(&g, "start");
  if (start == NULL) {
    printf("ERROR: cannot find node 'start'\n");
    return -1;
  }
  struct GraphNode* end = graph_find_node(&g, "end");
  if (end == NULL) {
    printf("ERROR: cannot find node 'end'\n");
    return -1;
  }
  
  struct SubPaths paths = path_from_to(start, end);
  // print all possible paths
  if (DEBUG) {
    printf("\nAll posible paths:\n");
    for (int i = 0; i < paths.len; i++) {
      struct NodePath* path = paths.paths[i];
      path_print(path);
    }
  }
  printf("\n1. there are %d possible paths from start to end.\n", paths.len);

  return 0;
}
