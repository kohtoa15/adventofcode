#include <stdlib.h>
#include <stdbool.h>

#include "graph.h"

// Path from one Graph Node to another, without loops (no nodes repeating)
struct NodePath {
  // of type size_t (graph indices)
  struct GraphNode** nodes;
  int len;
  int cap;
};

struct NodePath nodepath_init(int cap);

// Append path by one node
bool path_push_node(struct NodePath* path, struct GraphNode* node);

// Pop last node from path
struct GraphNode* path_pop_node(struct NodePath* path);

// checks whether one path is included in another
// does not imply equalness 
bool path_includes(struct NodePath* outer, struct NodePath* inner);

void path_print(struct NodePath* path);

struct SubPaths{
  struct NodePath** paths;
  int len;
  int cap;
};

struct SubPaths create_subpaths(struct NodePath* path);

struct SubPaths merge_subpaths(struct SubPaths dest, struct SubPaths src);

struct SubPaths path_from_to(struct GraphNode* start, struct GraphNode* end);
