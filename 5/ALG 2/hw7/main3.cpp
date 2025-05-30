#include <iostream>
#include <vector>
#include <stack>
#include <climits>

using namespace std;

struct Track {
    int to;
    int cost;
    Track(int t, int c) : to(t), cost(c) {}
};

struct GraphNode {
    int key;
    bool is_cottage;
    int indegree;
    vector<Track> children;
    GraphNode(int k) : key(k), is_cottage(true), indegree(0) {}
};

void topological_sort(vector<GraphNode>& graph, vector<int>& sorted) {
    stack<int> s;
    for (int i = 0; i < graph.size(); ++i) {
        if (graph[i].indegree == 0) {
            s.push(i);
        }
    }
    while (!s.empty()) {
        int curr = s.top();
        s.pop();
        sorted.push_back(curr);
        for (auto& ch : graph[curr].children) {
            graph[ch.to].indegree--;
            if (graph[ch.to].indegree == 0) {
                s.push(ch.to);
            }
        }
    }
}

int main() {
    int R, T;
    vector<GraphNode> nodes;
    cin >> R >> T;
    for (int i = 0; i < R; ++i) {
        nodes.emplace_back(GraphNode(i));
    }
    for (int i = 0; i < T; ++i) {
        int R1, R2, D;
        cin >> R1 >> R2 >> D;
        nodes[R1 - 1].children.emplace_back(Track(R2 - 1, D));
        nodes[R2 - 1].indegree++;
    }
    vector<int> sorted;
    topological_sort(nodes, sorted);
    vector<int> sorted_idx_to_node_idx(R, 0);
    for (int i = 0; i < R; ++i) {
        sorted_idx_to_node_idx[sorted[i]] = i;
    }
    vector<int> node_idx_to_sorted_idx(R, 0);
    for (int i = 0; i < R; ++i) {
        node_idx_to_sorted_idx[i] = sorted_idx_to_node_idx[i];
    }
    for (int idx_el = 0; idx_el < sorted.size(); ++idx_el) {
        auto el = sorted[idx_el];
        for (auto& ch : nodes[el].children) {
            int idx = sorted_idx_to_node_idx[ch.to];
            for (int i = idx_el + 1; i < idx; ++i) {
                nodes[sorted[i]].is_cottage = false;
            }
        }
    }
    vector<int> max_even(R, 0);
    vector<int> max_odd(R, 0);
    for (int i = 0; i < sorted.size(); ++i) {
        int curr = sorted[i];
        if (nodes[curr].is_cottage) {
            max_odd[i] = -INT_MAX;
        }
        for (auto& ch : nodes[curr].children) {
            int idx = sorted_idx_to_node_idx[ch.to];
            max_even[idx] = max(max_even[idx], max_odd[i] + ch.cost);
            max_odd[idx] = max(max_odd[idx], max_even[i] + ch.cost);
        }
    }
    int num_cottages = 0;
    for (int i = 0; i < R; ++i) {
        if (nodes[i].is_cottage) {
            num_cottages++;
        }
    }
    cout << max_even[max_even.size() - 1] << " " << num_cottages << endl;
    return 0;
}
