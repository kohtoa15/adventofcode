#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "gpath.h"

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

struct SubPaths create_subpaths(struct NodePath* path) {
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
      // is other already in path? (ignored if allowed recursions)
      if (!other->allow_recursion) {
        if (path_find_node(path, other))
          // cannot add subpath for this node, skip
          continue;
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

struct SubPaths path_from_to(struct GraphNode* start, struct GraphNode* end) {
  struct SubPaths ret;
  ret.len = 0;
  ret.cap = 256;
  ret.paths = (struct NodePath**) malloc(sizeof(struct NodePath*) * ret.cap);

  struct NodePath root = nodepath_init(32);
  path_push_node(&root, start);
  // path_print(&root);
  struct SubPaths subpaths = create_subpaths(&root);
  subpaths.cap = 256;
  subpaths.paths = (struct NodePath**) realloc(subpaths.paths, sizeof(struct NodePath*) * subpaths.cap);
  // reuse tmp subpaths for merging sub-sub-paths
  struct SubPaths tmp;
  tmp.cap = 256;
  tmp.paths = (struct NodePath**) malloc(sizeof(struct NodePath*) * tmp.cap);
  do {
      tmp.len = 0;
      for (int i = 0; i < subpaths.len; i++) {
        // printf("%d> mem stats (tmp %d->%d) (subpaths %d->%d) (ret %d->%d)\n", i, tmp.len, tmp.cap, subpaths.len, subpaths.cap, ret.len, ret.cap);
        struct NodePath* path = subpaths.paths[i];
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
        struct SubPaths sub = create_subpaths(path);
        // verify capacity for tmp
        if (sub.len + tmp.len > tmp.cap) {
          do {
            tmp.cap = tmp.cap * 2;
          } while (sub.len + tmp.len > tmp.cap);
          tmp.paths = (struct NodePath**) realloc(tmp.paths, sizeof(struct NodePath*) * tmp.cap);
        }
        // merge with tmp
        tmp = merge_subpaths(tmp, sub);
        // delete old subpath
        free(path);
        subpaths.paths[i] = NULL;
        // printf("%d< mem stats (tmp %d->%d) (subpaths %d->%d) (ret %d->%d)\n", i, tmp.len, tmp.cap, subpaths.len, subpaths.cap, ret.len, ret.cap);
      }
      // check if reallocs are necessary
      // preactive overallocating to cope with exponential subpath numbers
      if (tmp.len > tmp.cap / 2 || subpaths.len > subpaths.cap / 2) {
        subpaths.cap = subpaths.cap * 2;
        subpaths.paths = (struct NodePath**) realloc(subpaths.paths, sizeof(struct NodePath*) * subpaths.cap);
        tmp.cap = subpaths.cap;
        tmp.paths = (struct NodePath**) realloc(tmp.paths, sizeof(struct NodePath*) * tmp.cap);
      } else if (tmp.cap > subpaths.cap) {
        // readjustment necessary
        subpaths.cap = tmp.cap;
        subpaths.paths = (struct NodePath**) realloc(subpaths.paths, sizeof(struct NodePath*) * subpaths.cap);
      }

      // Swap tmp over to subpaths
      struct NodePath** new_subpaths = tmp.paths;
      tmp.paths = subpaths.paths;
      subpaths.paths = new_subpaths;
      subpaths.len = tmp.len;
      // printf("* next subpath round (%d subpaths)\n", subpaths.len);
  } while (subpaths.len > 0);

  // printf(">< mem stats (tmp %d->%d) (subpaths %d->%d) (ret %d->%d)\n", tmp.len, tmp.cap, subpaths.len, subpaths.cap, ret.len, ret.cap);
  return ret;
}
