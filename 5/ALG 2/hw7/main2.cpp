#include <iostream>
#include <vector>

using namespace std;

struct Edge {
    int cost, from, to;
};

struct GraphNode {
    int n_in = 0, n_out = 0;
    int i_in = 0, i_out = 0;
    vector<Edge> edges_in, edges_out;
    bool blocked_out = false, is_tight = false;
    bool accept_A = false, accept_B = false;
    int score_A = 0, score_B = 0;
    bool blocked_A = false, blocked_B = false;
};

int n_nodes, n_edges;
vector<GraphNode> NODE_STORAGE;
vector<int> idx_to_blocked_order;

int rec_block_search(int node_idx) {
    if (node_idx == n_nodes) {
        return idx_to_blocked_order[node_idx];
    }
    GraphNode *n = &NODE_STORAGE[node_idx];
    n->blocked_out = true;
    int ret = -1;
    for (const auto &e : n->edges_out) {
        GraphNode *next = &NODE_STORAGE[e.to];
        if (next->blocked_out) {
            ret = max(ret, idx_to_blocked_order[e.to]);
        } else {
            ret = max(ret, rec_block_search(e.to));
        }
    }
    return ret;
}

void rec_accept(int idx, bool A, bool B, bool start) {
    GraphNode *n = &NODE_STORAGE[idx];
    if (n->is_tight && start) {
        n->accept_A = true;
    } else if (n->is_tight && !start) {
        return;
    }
    if (!start && n->accept_A == A && n->accept_B == B) {
        return;
    }
    n->accept_A = n->accept_A || A;
    n->accept_B = n->accept_B || B;
    for (const auto &e : n->edges_in) {
        rec_accept(e.from, n->accept_B, n->accept_A, false);
    }
}

void dfs(int idx, bool AB) {
    GraphNode *n = &NODE_STORAGE[idx];
    for (const auto &e : n->edges_out) {
        GraphNode *m = &NODE_STORAGE[e.to];
        if (m->is_tight) {
            if (m->score_A < n->score_B + e.cost) {
                m->score_A = n->score_B + e.cost;
            }
            continue;
        }
        if (!AB && m->accept_B && m->score_B < n->score_A + e.cost) {
            m->score_B = n->score_A + e.cost;
            dfs(e.to, !AB);
        }
        if (AB && m->accept_A && m->score_A < n->score_B + e.cost) {
            m->score_A = n->score_B + e.cost;
            dfs(e.to, !AB);
        }
    }
}

int main() {
    cin >> n_nodes >> n_edges;
    NODE_STORAGE.resize(n_nodes + 1);
    vector<Edge> INPUT_STORAGE(n_edges);
    for (int i = 0; i < n_edges; ++i) {
        cin >> INPUT_STORAGE[i].from >> INPUT_STORAGE[i].to >> INPUT_STORAGE[i].cost;
    }
    for (const auto &e : INPUT_STORAGE) {
        NODE_STORAGE[e.from].n_out++;
        NODE_STORAGE[e.to].n_in++;
    }
    for (int i = 1; i <= n_nodes; ++i) {
        NODE_STORAGE[i].edges_in.resize(NODE_STORAGE[i].n_in);
        NODE_STORAGE[i].edges_out.resize(NODE_STORAGE[i].n_out);
    }
    for (const auto &e : INPUT_STORAGE) {
        NODE_STORAGE[e.from].edges_out[NODE_STORAGE[e.from].i_out++] = e;
        NODE_STORAGE[e.to].edges_in[NODE_STORAGE[e.to].i_in++] = e;
    }
    NODE_STORAGE[n_nodes].blocked_out = true;
    NODE_STORAGE[n_nodes].is_tight = true;
    vector<int> blocked_nodes;
    blocked_nodes.push_back(1);
    GraphNode *n = &NODE_STORAGE[1];
    while (!n->edges_out.empty()) {
        n->blocked_out = true;
        n->is_tight = true;
        int next_node = n->edges_out[0].to;
        blocked_nodes.push_back(next_node);
        n = &NODE_STORAGE[next_node];
    }
    idx_to_blocked_order.assign(n_nodes + 1, -1);
    for (int i = 0; i < blocked_nodes.size(); ++i) {
        idx_to_blocked_order[blocked_nodes[i]] = i;
    }
    for (int i = 0; i < blocked_nodes.size(); ++i) {
        int temp = rec_block_search(blocked_nodes[i]);
        for (int j = temp - 1; j > i; --j) {
            NODE_STORAGE[blocked_nodes[j]].is_tight = false;
        }
    }
    NODE_STORAGE[1].accept_A = true;
    for (int i = 2; i <= n_nodes; ++i) {
        if (NODE_STORAGE[i].is_tight) {
            rec_accept(i, true, false, true);
        }
    }
    int res_tight = 0;
    for (int i : blocked_nodes) {
        if (NODE_STORAGE[i].is_tight) {
            dfs(i, 0);
            ++res_tight;
        }
    }
    cout << NODE_STORAGE[n_nodes].score_A << " " << res_tight << "\n";
    return 0;
}
