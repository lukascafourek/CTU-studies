#include <unordered_set>
#include <atomic>
#include <limits>
#include <omp.h>
#include "iddfs.h"

std::atomic<uint64_t> global_cost{std::numeric_limits<uint64_t>::max()};

static bool operator<(const state_ptr& s1, const state_ptr& s2) {
    if (!s1) return false;
    if (!s2) return true;
    return s1->get_identifier() < s2->get_identifier();
}

state_ptr limited_dfs(size_t depth, const state_ptr& root, std::unordered_set<uint64_t> closed_list, size_t threshold) {
    std::atomic<uint64_t> local_cost{std::numeric_limits<uint64_t>::max()};
    if (root->current_cost() > global_cost.load()) {
        return nullptr;
    }
    bool check;
    #pragma omp critical
    check = closed_list.insert(root->get_identifier()).second;
    if (!check) {
        return nullptr;
    }
    if (root->is_goal()) {
        uint64_t expected = global_cost.load();
        while (root->current_cost() < expected &&
            !global_cost.compare_exchange_strong(expected, root->current_cost())) {}
        return root;
    }
    if (depth == 0) return nullptr;
    state_ptr best_found = nullptr;
    if (threshold == 1) {
        for (state_ptr& next : root->next_states()) {
            auto ret = limited_dfs(depth - 1, next, closed_list, 1);
            #pragma omp critical
            if ((ret < best_found && ret->current_cost() == local_cost) || (ret && ret->current_cost() < local_cost)) {
                local_cost = ret->current_cost();
                best_found = ret;
            }
        }
    } else {
        #pragma omp parallel for schedule(dynamic)
        for (state_ptr& next : root->next_states()) {
            auto ret = limited_dfs(depth - 1, next, closed_list, threshold >> 1);
            #pragma omp critical
            if ((ret < best_found && ret->current_cost() == local_cost) || (ret && ret->current_cost() < local_cost)) {
                local_cost = ret->current_cost();
                best_found = ret;
            }
        }
    }
    uint64_t expected = global_cost.load();
    while (local_cost < expected && !global_cost.compare_exchange_strong(expected, local_cost)) {}
    return best_found;
}

std::shared_ptr<const state> iddfs(std::shared_ptr<const state> root) {
    for (size_t depth = 0; true; depth++) {
        std::unordered_set<uint64_t> closed_list;
        auto ret = limited_dfs(depth, root, closed_list, omp_get_num_threads() * 8);
        if (ret) {
            return ret;
        }
    }
}
