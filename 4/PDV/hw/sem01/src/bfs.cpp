#include "bfs.h"
#include <cstdint>
#include <limits>
#include <queue>
#include <omp.h>
#include <atomic>
#include <unordered_set>

static bool operator<(const state_ptr& s1, const state_ptr& s2) {
    if (!s1) return false;
    if (!s2) return true;
    return s1->get_identifier() < s2->get_identifier();
}

state_ptr bfs(state_ptr root) {
    std::queue<state_ptr> queue{};
    std::unordered_set<uint64_t> closed_list{};
    std::atomic<uint64_t> depth{std::numeric_limits<uint64_t>::max()};
    state_ptr goal = nullptr;
    queue.push(root);
    closed_list.insert(root->get_identifier());
    while (!queue.empty()) {
        #pragma omp parallel
        {
            std::atomic<uint64_t> local_depth = depth.load();
            state_ptr local_goal = nullptr;
            #pragma omp for schedule(static)
            for (size_t i = 0; i < queue.size(); ++i) {
                state_ptr node;
                #pragma omp critical
                {
                    if (!queue.empty()) {
                        node = queue.front();
                        queue.pop();
                    }
                }
                if (node != nullptr) {
                    if (node->current_cost() >= local_depth) {
                        continue;
                    }
                    for (state_ptr &next: node->next_states()) {
                        bool ret = false;
                        #pragma omp critical
                        ret = closed_list.insert(next->get_identifier()).second;
                        if (!ret) {
                            continue;
                        }
                        if (next->is_goal() && next < local_goal) {
                            local_depth.store(next->current_cost());
                            local_goal = next;
                            if (local_depth < depth ||
                            (depth == local_depth && local_goal < goal)) {
                                depth.store(local_depth);
                                goal = local_goal;
                            }
                        }
                        if (local_goal == nullptr) {
                            #pragma omp critical
                            queue.push(next);
                        }
                    }
                }
            }
        }
    }
    return goal;
}
