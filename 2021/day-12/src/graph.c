#include <stdio.h>
#include <string.h>
#include "graph.h"

struct GraphNode* graph_node_from_name(const char* name) {
  struct GraphNode* n = (struct GraphNode*) malloc(sizeof(struct GraphNode));
  n->name = (char*) malloc(sizeof(char) * (strlen(name) + 1));
  strcpy(n->name, name);
  n->connections = (struct GraphEdge**) malloc(sizeof(struct GraphEdge*) * 10);
  n->connection_num = 0;
  n->allow_recursion = false;
  return n;
}

void graph_node_add_connection(struct GraphNode* node, struct GraphEdge* edge) {
  // realloc every 10 connections
  if (node->connection_num > 0 && node->connection_num % 10 == 0) {
    node->connections = (struct GraphEdge**) calloc(node->connection_num + 10, sizeof(struct GraphEdge*));
  }
  // set last element
  node->connections[node->connection_num] = edge;
  node->connection_num++;
}

struct GraphEdge* graph_edge_from_data(struct GraphNode* node_a, struct GraphNode* node_b) {
  struct GraphEdge* e = (struct GraphEdge*) malloc(sizeof(struct GraphEdge));
  e->node_a = node_a;
  e->node_b = node_b;
  return e;
}

struct GraphNode* graph_edge_other_node(struct GraphEdge* edge, struct GraphNode* node_a) {
  if (edge->node_a == node_a)
    return edge->node_b;
  if (edge->node_b == node_a)
    return edge->node_a;
  return NULL;
}

struct Graph graph_init(int nodes_cap, int edges_cap) {
  struct Graph g;
  g.nodes_cap = nodes_cap;
  g.nodes_num = 0;
  g.nodes = (struct GraphNode**) calloc(nodes_cap, sizeof(struct GraphNode*));
  g.edges_cap = edges_cap;
  g.edges_num = 0;
  g.edges = (struct GraphEdge**) calloc(edges_cap, sizeof(struct GraphEdge*));
  return g;
}

struct GraphNode* graph_find_node(struct Graph* g, const char* name) {
  for (int i = 0; i < g->nodes_num; i++) {
    struct GraphNode* it = g->nodes[i];
    if (strcmp(it->name, name) == 0) {
      // found a node with same name
      return it;
    }
  }
  // could not find node with this name
  return NULL;
}

// returns id for the newly created graph node
//
// if insert is not possible -1 is returned.
struct GraphNode* graph_insert_node(struct Graph* g, const char* name) {
  // check if we have cap for another item
  if (g->nodes_num == g->nodes_cap) { return NULL; }
  // check if same name already exists
  if (graph_find_node(g, name) != NULL) { return NULL; }
  // create new item and add to the list
  struct GraphNode* item = graph_node_from_name(name);
  g->nodes[g->nodes_num++] = item;
  return item;
}

bool graph_insert_edge(struct Graph* g, const char* node_a, const char* node_b) {
  // check if we have cap for another item
  if (g->edges_num == g->edges_cap) { return false; }
   // try to find both nodes
  struct GraphNode* item_a = graph_find_node(g, node_a);
  if (item_a == NULL)
    return false;
  struct GraphNode* item_b = graph_find_node(g, node_b);
  if (item_b == NULL)
    return false;
  // create new edge and insert to list
  struct GraphEdge* edge = graph_edge_from_data(item_a, item_b);
  g->edges[g->edges_num++] = edge;
  // add connections for both nodes
  graph_node_add_connection(item_a, edge);
  graph_node_add_connection(item_b, edge);
  return true;
}

