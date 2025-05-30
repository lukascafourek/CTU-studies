#include <stdio.h>
#include <stdlib.h>

#include "graph.h"

graph_t* allocate_graph() {
   graph_t *graph = (graph_t*)malloc(sizeof(graph_t));
   if (!graph) {
      fprintf(stderr, "Graph malloc failed!");
      exit(-1);
   }
   graph->capacity = 10;
   graph->count = 0;
   graph->edges = (edge_t*)malloc(graph->capacity * sizeof(edge_t));
   if (!graph->edges) {
      fprintf(stderr, "Edge malloc failed!");
      exit(-1);
   }
   return graph;
}
graph_t* enlarge_graph(graph_t *graph) {
   graph->capacity *= 2;
   graph->edges = realloc(graph->edges, graph->capacity * sizeof(edge_t));
   if (!graph->edges) {
      fprintf(stderr, "Edge realloc failed!");
      exit(-1);
   }
   return graph;
}
void free_graph(graph_t **graph) {
   free((*graph)->edges);
   free(*graph);
   *graph = NULL;
}
void load_txt(const char *fname, graph_t *graph) {
   int ret = 0;
   FILE *f = fopen(fname, "r");
   while (f && !ret) {
      if (graph->count == graph->capacity) {
         enlarge_graph(graph);
      }
      edge_t *edge = graph->edges + graph->count;
      while (graph->count < graph->capacity) {
         int r = fscanf(f, "%d %d %d\n", &(edge->from), &(edge->to), &(edge->cost));
         if (r == 3) {
            graph->count += 1;
            edge += 1;
         } else {
            ret = 1;
            break;
         }
      }
   }
   if (f) {
      fclose(f);
   }
}
void load_bin(const char *fname, graph_t *graph) {
   int ret = 0;
   FILE *f = fopen(fname, "rb");
   while (f && !ret) {
      if (graph->count == graph->capacity) {
         enlarge_graph(graph);
      }
      edge_t *edge = graph->edges + graph->count;
      while (graph->count < graph->capacity) {
         int r = fread(&edge->from, sizeof(edge->from), 1, f);
         int s = fread(&edge->to, sizeof(edge->to), 1, f);
         int t = fread(&edge->cost, sizeof(edge->cost), 1, f);
         if (r && s && t) {
            graph->count += 1;
            edge += 1;
         } else {
            ret = 1;
            break;
         }
      }
   }
   if (f) {
      fclose(f);
   }
}
void save_txt(const graph_t * const graph, const char *fname) {
   edge_t *edge = graph->edges;
   FILE *f = fopen(fname, "w");
   if (f) {
      for (int i = 0; i < graph->count; ++i, ++edge) {
         fprintf(f, "%d %d %d\n", edge->from, edge->to, edge->cost);
      }
      fclose(f);
   }
}
void save_bin(const graph_t * const graph, const char *fname) {
   edge_t *edge = graph->edges;
   FILE *f = fopen(fname, "wb");
   if (f) {
      for (int i = 0; i < graph->count; ++i, ++edge) {
         fwrite(&edge->from, sizeof(edge->from), 1, f);
         fwrite(&edge->to, sizeof(edge->to), 1, f);
         fwrite(&edge->cost, sizeof(edge->cost), 1, f);
      }
      fclose(f);
   }
}
