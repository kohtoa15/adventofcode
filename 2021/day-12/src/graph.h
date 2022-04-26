#include <stdlib.h>
#include <stdbool.h>

// Single graph node with name data and edges
struct GraphNode {
  char* name;
  struct GraphEdge** connections;
  int connection_num;
  bool allow_recursion;
};

struct GraphNode* graph_node_from_name(const char* name);

// graph edge between two nodes
//
// edges are non-directional
struct GraphEdge {
  struct GraphNode* node_a;
  struct GraphNode* node_b;
};

struct GraphEdge* graph_edge_from_data(struct GraphNode* node_a, struct GraphNode* node_b);

struct GraphNode* graph_edge_other_node(struct GraphEdge* edge, struct GraphNode* node_a);

// Collection for all the nodes and edges of a graph
struct Graph {
  // list of GraphNodes used in this Graph
  struct GraphNode** nodes;
  int nodes_num;
  // used without realloc, watch you node count
  int nodes_cap;
  // list of GraphEdges between GraphNodes
  struct GraphEdge** edges;
  int edges_num;
  // used without realloc, watch you edge count 
  int edges_cap;
};

struct Graph graph_init(int nodes_cap, int edges_cap);

struct GraphNode* graph_find_node(struct Graph* g, const char* name);

struct GraphNode* graph_insert_node(struct Graph* g, const char* name);

bool graph_insert_edge(struct Graph* g, const char* node_a, const char* node_b);

