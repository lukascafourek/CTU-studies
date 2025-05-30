#include <iostream>
#include <vector>
#include <memory>
#include <algorithm>

using namespace std;

int compute_preventive_value(const unique_ptr<vector<vector<int>>>& prefix_sum, int top, int bottom, int left, int right) {
    int total = (*prefix_sum)[bottom][right];
    if (top > 0) total -= (*prefix_sum)[top - 1][right];
    if (left > 0) total -= (*prefix_sum)[bottom][left - 1];
    if (top > 0 && left > 0) total += (*prefix_sum)[top - 1][left - 1];
    if (bottom > 0) total += (*prefix_sum)[bottom - 1][left];
    if (right > 0) total += (*prefix_sum)[top][right - 1];
    if (bottom > 0 && right > 0) {
        total -= (*prefix_sum)[bottom - 1][right - 1];
        total -= (*prefix_sum)[top][left];
    }
    return total;
}

int main() {
    int R, S;
    cin >> R >> S;
    auto grid = make_unique<vector<vector<int>>>(R, vector<int>(S));
    auto prefix_sum = make_unique<vector<vector<int>>>(R, vector<int>(S, 0));
    auto zeros_up = make_unique<vector<vector<int>>>(R, vector<int>(S, 0));
    auto sectors = make_unique<vector<vector<int>>>(R, vector<int>(S, 0));
    int total_sectors = 0;
    for (int i = 0; i < R; ++i) {
        for (int j = 0; j < S; ++j) {
            cin >> (*grid)[i][j];
            if ((*grid)[i][j] == 0 && (i == 0 || (*grid)[i - 1][j] != 0) && (j == 0 || (*grid)[i][j - 1] != 0)) total_sectors++;
            int sector_prefix = 0;
            int zero_up = 0;
            bool zero_in_place = false;
            int sum_prefix = (*grid)[i][j];
            if ((*grid)[i][j] == 0) {
                zero_up++;
                zero_in_place = true;
            }
            if (i > 0) {
                sum_prefix += (*prefix_sum)[i - 1][j];
                zero_up += (*zeros_up)[i - 1][j];
                sector_prefix = max(sector_prefix, (*sectors)[i - 1][j]);
                if (zero_in_place && (*grid)[i - 1][j] == 0) zero_in_place = false;
            }
            if (j > 0) {
                sum_prefix += (*prefix_sum)[i][j - 1];
                sector_prefix = max(sector_prefix, (*sectors)[i][j - 1]);
                if (zero_in_place && (*grid)[i][j - 1] == 0) zero_in_place = false;
            }
            if (zero_in_place) sector_prefix++;
            if (i > 0 && j > 0) {
                sum_prefix -= (*prefix_sum)[i - 1][j - 1];
                int sector_left = (*sectors)[i][j - 1];
                int sector_up = (*sectors)[i - 1][j];
                int sector_up_left = (*sectors)[i - 1][j - 1];
                if (sector_up > sector_up_left && sector_left > sector_up_left)
                    sector_prefix += min(sector_up, sector_left) - sector_up_left;
            }
            (*zeros_up)[i][j] = zero_up;
            (*sectors)[i][j] = sector_prefix;
            (*prefix_sum)[i][j] = sum_prefix;
        }
    }
    int required_sectors = total_sectors / 2;
    int max_preventive_value = 0;
    for (int top = 0; top < R; ++top) {
        for (int left = 0; left < S; ++left) {
            if ((*grid)[top][left] == 0) continue;
            for (int bottom = top; bottom < R; ++bottom) {
                if ((*grid)[bottom][left] == 0) break;
                for (int right = left; right < S; ++right) {
                    int sectors_inside = (*sectors)[bottom][right] - (*sectors)[bottom][left] -
                        (*sectors)[top][right] + (*sectors)[top][left];
                    if (sectors_inside == required_sectors) {
                        if ((*grid)[top][right] == 0 || (*grid)[bottom][right] == 0) break;
                        if ((*zeros_up)[bottom][right] - (*zeros_up)[top][right] > 0) continue;
                        int value = compute_preventive_value(prefix_sum, top, bottom, left, right);
                        max_preventive_value = max(max_preventive_value, value);
                    }
                }
            }
        }
    }
    cout << max_preventive_value << endl;
    return 0;
}
