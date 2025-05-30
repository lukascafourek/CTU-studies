#include <iostream>
#include <vector>
#include <stack>

using namespace std;

struct GraphNode {
    int indegree;
    vector<int> adjacentNodes;
    vector<int> edgeCosts;
    GraphNode() : indegree(0) {}
};

struct DPState {
    int odd;
    int even;
    bool isCottage;
    DPState() : odd(-1), even(-1), isCottage(true) {}
};

vector<int> topologicalSort(vector<GraphNode>& graph, const int numNodes) {
    stack<int> stack;
    vector<int> topologicalOrder;
    for (int i = 0; i < numNodes; i++) {
        if (graph[i].indegree == 0) {
            stack.push(i);
        }
    }
    while (!stack.empty()) {
        int currentNode = stack.top();
        stack.pop();
        topologicalOrder.emplace_back(currentNode);
        for (auto adjacentNode : graph[currentNode].adjacentNodes) {
            graph[adjacentNode].indegree--;
            if (graph[adjacentNode].indegree == 0) {
                stack.push(adjacentNode);
            }
        }
    }
    return topologicalOrder;
}

int main() {
    int numNodes, numEdges;
    cin >> numNodes >> numEdges;
    vector<GraphNode> graph(numNodes);
    for (int i = 0; i < numEdges; i++) {
        int from, to, cost;
        cin >> from >> to >> cost;
        graph[from - 1].adjacentNodes.emplace_back(to - 1);
        graph[from - 1].edgeCosts.emplace_back(cost);
        graph[to - 1].indegree++;
    }
    auto topologicalOrder = topologicalSort(graph, numNodes);
    vector<DPState> dp(numNodes);
    dp[0].even = 0;
    for (int i = 0; i < numNodes; i++) {
        auto currentNode = topologicalOrder[i];
        for (auto adjacentNode : graph[currentNode].adjacentNodes) {
            for (int k = i + 1; k < numNodes; k++) {
                if (topologicalOrder[k] == adjacentNode) {
                    break;
                }
                dp[topologicalOrder[k]].isCottage = false;
            }
        }
        for (int j = 0; j < graph[currentNode].adjacentNodes.size(); j++) {
            auto adjacentNode = graph[currentNode].adjacentNodes[j];
            if (dp[currentNode].even != -1) {
                dp[adjacentNode].odd = max(dp[adjacentNode].odd, dp[currentNode].even + graph[currentNode].edgeCosts[j]);
            }
            if (dp[currentNode].odd != -1 && !dp[currentNode].isCottage) {
                dp[adjacentNode].even = max(dp[adjacentNode].even, dp[currentNode].odd + graph[currentNode].edgeCosts[j]);
            }
        }
    }
    int cottageCount = 0;
    for (int i = 0; i < numNodes; i++) {
        if (dp[i].isCottage) {
            cottageCount++;
        }
    }
    cout << dp[numNodes - 1].even << " " << cottageCount << endl;
    return 0;
}
