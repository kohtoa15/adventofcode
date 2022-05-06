#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "gpath.h"
#include "debug.h"

struct NodePath nodepath_init(int cap) {
  struct NodePath p;
  p.len = 0;
  p.cap = cap;
  p.nodes = (struct GraphNode**) calloc(cap, sizeof(struct GraphNode*));
  return p;
}

bool path_push_node(struct NodePath* path, struct GraphNode* node) {
  // append node to list
  if (path->cap > path->len) {
    path->nodes[path->len++] = node;
    return true;
  }
  printf("ERR: reached cap of nodepath\n");
  return false;
}

struct GraphNode* path_pop_node(struct NodePath *path) {
  // verify that we have a node to pop
  if (path->len > 0) {
    // get return addr from item ptr
    struct GraphNode* ptr = path->nodes[--path->len];
    return ptr;
  }
  return NULL;
}

bool path_includes(struct NodePath *outer, struct NodePath *inner) {
  // verify that the inner path is shorter or of equal length
  // if not it's not included
  if (outer->len < inner->len)
    return false;
  for (int i = 0; i < outer->len && i < inner->len; i++) {
    if (outer->nodes[i] != inner->nodes[i])
      return false;
  }
  return true;
}

void path_print(struct NodePath *path) {
  if (path->len == 0) {
    printf("<empty path>\n");
  } else {
    for (int i = 0; i < path->len; i++) {
      if (path->nodes[i] == NULL) {
        printf("\nERROR: node %d of path is null!\n", i);
        return;
      }
      printf("%s", path->nodes[i]->name);
      if (i < (path->len-1))
        printf(",");
    }
    printf("\n");
  }
}

bool path_find_node(struct NodePath* path, struct GraphNode* node) {
  for (int i = 0; i < path->len; i++) {
    if (path->nodes[i] == node) {
      // found node
      return true;
    }
  }
  return false;
}

int path_count_max_node(struct NodePath* path) {
  int count = 0;
  for (int i = 0; i < path->len; i++) {
    int count_i = 1;
    // don't count nodes that allow recursion
    if (path->nodes[i]->allow_recursion)
      continue;
    for (int k = 0; k < path->len; k++) {
      // don't compare node to itself
      if (i == k)
        continue;
      // don't count nodes that allow recursion
      if (path->nodes[k]->allow_recursion)
        continue;
      // check if we have other same nodes
      if (path->nodes[i] == path->nodes[k]) {
        ++count_i;
      }
    }
    if (count_i > count)
      count = count_i;
  }
  return count;
}

struct SubPaths create_subpaths(struct NodePath* path, int max_visits, bool skip_start) {
  struct SubPaths ret;
  ret.cap = 0;
  ret.len = 0;
  ret.paths = NULL;
  struct GraphNode* cur = path->nodes[path->len - 1];
  if (cur == NULL) {
    printf("ERROR: invalid root path\n");
    return ret;
  }
  // try to create a new subpath for each connection of current node
  ret.cap = cur->connection_num;
  ret.paths = (struct NodePath**) calloc(ret.cap, sizeof(struct NodePath*));

  for (int i = 0; i < cur->connection_num; i++) {
    struct GraphEdge* edge = cur->connections[i];
    struct GraphNode* other = graph_edge_other_node(edge, cur);
    if (other != NULL) {
      // don't allow recursion with start node if specified
      if (skip_start && other == path->nodes[0])
        continue;
      // is other already in path? (ignored if allowed recursions)
      if (!other->allow_recursion) {
        if (path_find_node(path, other)) {
          // check whether we still can visit again
          // (only if we have not yet reached max_visits)
          if (path_count_max_node(path) >= max_visits)
            // cannot add subpath for this node, skip
            continue;
        }
      }
      // can add other as subpath
      struct NodePath new = nodepath_init(path->len + 1);
      for (int p = 0; p < path->len; p++) {
        path_push_node(&new, path->nodes[p]);
      }
      // add other node on top
      path_push_node(&new, other);
      ret.paths[ret.len] = (struct NodePath*) malloc(sizeof(struct NodePath));
      *ret.paths[ret.len] = new;
      ret.len++;
    }
  }
  return ret;
}

struct SubPaths merge_subpaths(struct SubPaths dest, struct SubPaths src) {
  // append items of second part
  for (int i = 0; i < src.len; i++) {
    // append item to dest
    dest.paths[dest.len + i] = src.paths[i];
  }
  dest.len += src.len;
  return dest;
}

struct SubPaths path_from_to(struct GraphNode* start, struct GraphNode* end, int max_visits) {
  struct SubPaths ret;
  ret.len = 0;
  ret.cap = 256;
  ret.paths = (struct NodePath**) malloc(sizeof(struct NodePath*) * ret.cap);

  struct NodePath root = nodepath_init(32);
  path_push_node(&root, start);

  // use RW swap lists (one to write, one to read, swap after each round)
  struct SubPaths sub_read = create_subpaths(&root, max_visits, true);
  sub_read.cap = 256;
  sub_read.paths = (struct NodePath**) realloc(sub_read.paths, sizeof(struct NodePath*) * sub_read.cap);
  struct SubPaths sub_write;
  sub_write.cap = 256;
  sub_write.paths = (struct NodePath**) malloc(sizeof(struct NodePath*) * sub_write.cap);
  do {
      sub_write.len = 0;
      for (int i = 0; i < sub_read.len; i++) {
        // printf("%d> mem stats (sub W %d->%d) (sub R %d->%d) (ret %d->%d)\n", i, sub_write.len, sub_write.cap, subpaths.len, subpaths.cap, ret.len, ret.cap);
        struct NodePath* path = sub_read.paths[i];
        // path_print(path);
        // check if path ends with target end
        if (path->nodes[path->len - 1] == end) {
          // check capacity on ret
          if (ret.len == ret.cap) {
            ret.cap = ret.cap * 2;
            ret.paths = (struct NodePath**) realloc(ret.paths, sizeof(struct NodePath*) * ret.cap);
          }
          // add path to ret
          ret.paths[ret.len++] = path;
          continue;
        }
        struct SubPaths sub = create_subpaths(path, max_visits, true);
        // verify capacity for sub_write
        if (sub.len + sub_write.len > sub_write.cap) {
          // increase write buffer cap in orders of 2 until it fits
          do {
            sub_write.cap = sub_write.cap * 2;
          } while (sub.len + sub_write.len > sub_write.cap);
          debug_print("extend sub W to %d\n", sub_write.cap);
          sub_write.paths = (struct NodePath**) realloc(sub_write.paths, sizeof(struct NodePath*) * sub_write.cap);
        }
        // append to sub_write
        sub_write = merge_subpaths(sub_write, sub);
        // delete old subpath
        free(path);
        sub_read.paths[i] = NULL;
      }
      // check if reallocs are necessary
      if (sub_write.cap > sub_read.cap) {
        // readjustment necessary
        sub_read.cap = sub_write.cap;
        debug_print("extend sub R to %d\n", sub_read.cap);
        sub_read.paths = (struct NodePath**) realloc(sub_read.paths, sizeof(struct NodePath*) * sub_read.cap);
      }

      // Swap Read/Write lists
      struct NodePath** sub_swap = sub_write.paths;
      sub_write.paths = sub_read.paths;
      sub_read.paths = sub_swap;
      sub_read.len = sub_write.len;
      debug_print("* next subpath round (%d subpaths)\n", sub_read.len);
  } while (sub_read.len > 0);

  return ret;
}
