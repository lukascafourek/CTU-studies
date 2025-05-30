#include <iostream>
#include <vector>
#include <climits>

struct Node {
    int value;
    int numberOfNodes;
    long long attractivity;
    explicit Node(const int i) : value(i), numberOfNodes(1), attractivity(LONG_LONG_MIN) {}
};

int main() {
    int N, DH, H1, H2;
    if (!(std::cin >> N >> DH >> H1 >> H2)) {
        std::cerr << "Error: Chybny vstup!" << std::endl;
        return 1;
    }
    std::vector<std::vector<int>> arr(N, std::vector<int>(N, 0));
    std::vector<std::vector<long long>> atr(N, std::vector<long long>(N, 0));
    std::vector<int> num(8000, 0);
    std::vector<long long> best_atr(8000, LONG_LONG_MIN);
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            if (!(std::cin >> arr[i][j])) {
                std::cerr << "Error: Chybny vstup!" << std::endl;
                return 1;
            }
            num[arr[i][j]]++;
        }
    }
    int max_num = 0;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            if (i == 0 && j == 0) {
                atr[i][j] = 0;
                best_atr[arr[i][j]] = atr[i][j];
            } else if (j == 0) {
                atr[i][j] = atr[i - 1][j];
                for (int k = j; k < N; ++k) {
                    atr[i][j] -= arr[i - 1][k];
                }
                int x = arr[i][j] - DH;
                int y = std::max(0, arr[i - 1][j] - DH);
                if (x > y) {
                    for (int k = y; k < x; ++k) {
                        atr[i][j] += 2 * num[k] * k;
                    }
                }
                if (atr[i][j] > best_atr[arr[i][j]]) {
                    best_atr[arr[i][j]] = atr[i][j];
                }
            } else {
                atr[i][j] = atr[i][j - 1];
                for (int k = i; k < N; ++k) {
                    atr[i][j] -= arr[k][j - 1];
                }
                int x = arr[i][j] - DH;
                int y = std::max(0, arr[i][j - 1] - DH);
                if (x > y) {
                    for (int k = y; k < x; ++k) {
                        atr[i][j] += 2 * num[k] * k;
                    }
                }
                if (atr[i][j] > best_atr[arr[i][j]]) {
                    best_atr[arr[i][j]] = atr[i][j];
                }
            }
            if (i == N - 1 && j == N - 1) {
                max_num = arr[i][j];
            }
        }
    }
    std::vector<Node> nodes;
    for (int i = 0; i <= max_num; ++i) {
        if (num[i] != 0) {
            auto node = Node(i);
            nodes.emplace_back(node);
        }
    }
    const int n = static_cast<int>(nodes.size());
    std::vector<std::vector<Node*>> edges;
    edges.resize(n);
    for (int i = n - 1; i >= 0; i--) {
        int count = H2 - H1 + 1;
        for (int j = i - 1; j >= 0; j--) {
            if (count == 0) {
                break;
            }
            if (nodes[i].value - nodes[j].value >= H1 && nodes[i].value - nodes[j].value <= H2) {
                edges[i].emplace_back(&nodes[j]);
                count--;
            }
        }
    }
    for (int i = 0; i < n; i++) {
        auto& node = nodes[i];
        if (edges[i].empty()) {
            node.attractivity = best_atr[node.value];
            continue;
        }
        for (const auto& edge : edges[i]) {
            if (node.numberOfNodes == edge->numberOfNodes + 1) {
                if (node.attractivity < edge->attractivity) {
                    node.attractivity = edge->attractivity;
                }
            } else if (node.numberOfNodes < edge->numberOfNodes + 1) {
                node.numberOfNodes = edge->numberOfNodes + 1;
                node.attractivity = edge->attractivity;
            }
        }
        node.attractivity += best_atr[node.value];
    }
    std::cout << nodes[n - 1].numberOfNodes << " " << nodes[n - 1].attractivity << std::endl;
    return 0;
}
