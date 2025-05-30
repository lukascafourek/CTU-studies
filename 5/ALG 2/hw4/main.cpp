#include <iostream>
#include <queue>

using namespace std;

struct Node {
    short value;
    short x;
    short y;
    short direction;
    short speed;
    short distance;
    bool discovered;
};

int main() {
    short M, N, R;
    const short D = 4;
    cin >> M >> N >> R;
    vector<vector<vector<vector<Node>>>> grids(D, vector<vector<vector<Node>>>(R, vector<vector<Node>>(M, vector<Node>(N))));
    for (short i = 0; i < D; i++) {
        for (short j = 0; j < R; j++) {
            for (short k = 0; k < M; k++) {
                for (short l = 0; l < N; l++) {
                    const auto node = &grids[i][j][k][l];
                    if (i == 0 && j == 0) {
                        cin >> node->value;
                    } else {
                        node->value = grids[0][0][k][l].value;
                    }
                    node->x = k;
                    node->y = l;
                    node->direction = -1;
                    node->speed = 1;
                    node->distance = 0;
                    node->discovered = false;
                }
            }
        }
    }
    queue<Node*> q;
    q.push(&grids[0][0][M - 1][0]);
    while (!q.empty()) {
        const auto node = q.front();
        q.pop();
        if (node->speed == 1) {
            vector<pair<short, short>> directions = {
                {-2, 0}, // UP
                {2, 0},  // DOWN
                {0, -2}, // LEFT
                {0, 2}   // RIGHT
            };
            short i = 0;
            for (const auto&[fst, snd] : directions) {
                const short x = node->x + fst;
                const short y = node->y + snd;
                if (x >= 0 && x < M && y >= 0 && y < N) {
                    const auto n = &grids[i][0][x][y];
                    const short x1 = fst == -2 ? x + 1 : fst == 2 ? x - 1 : x;
                    const short y1 = snd == -2 ? y + 1 : snd == 2 ? y - 1 : y;
                    if (n->value == 0 && !n->discovered && grids[i][0][x1][y1].value <= 1) {
                        n->direction = i;
                        n->speed = 1;
                        n->distance = node->distance + 1;
                        n->discovered = true;
                        if (x == 0 && y == N - 1) {
                            cout << n->distance << endl;
                            return 0;
                        }
                        q.push(n);
                    } else if (n->value > 2 || grids[i][0][x1][y1].value > 1) {
                        i++;
                        continue;
                    }
                    if (i == node->direction) {
                        const short x2 = x + fst;
                        const short y2 = y + snd;
                        if (x2 >= 0 && x2 < M && y2 >= 0 && y2 < N) {
                            const auto m = &grids[i][1][x2][y2];
                            const short x3 = fst == -2 ? x2 + 1 : fst == 2 ? x2 - 1 : x2;
                            const short y3 = snd == -2 ? y2 + 1 : snd == 2 ? y2 - 1 : y2;
                            if (m->value == 0 && !m->discovered && grids[i][1][x3][y3].value <= 1) {
                                m->direction = i;
                                m->speed = 2;
                                m->distance = node->distance + 1;
                                m->discovered = true;
                                if (x2 == 0 && y2 == N - 1) {
                                    cout << m->distance << endl;
                                    return 0;
                                }
                                q.push(m);
                            }
                        }
                    }
                }
                i++;
            }
        } else {
            short speed = node->speed;
            short direction = node->direction;
            vector<pair<short, short>> speeds;
            switch (direction) {
                case 0: // UP
                    speeds = {
                            {(-2 * speed) + 2, 0},
                            {(-2 * speed), 0},
                            {(-2 * speed) - 2, 0}
                    };
                    break;
                case 1: // DOWN
                    speeds = {
                            {(2 * speed) - 2, 0},
                            {(2 * speed), 0},
                            {(2 * speed) + 2, 0}
                    };
                    break;
                case 2: // LEFT
                    speeds = {
                            {0, (-2 * speed) + 2},
                            {0, (-2 * speed)},
                            {0, (-2 * speed) - 2}
                    };
                    break;
                case 3: // RIGHT
                    speeds = {
                            {0, (2 * speed) - 2},
                            {0, (2 * speed)},
                            {0, (2 * speed) + 2}
                    };
                    break;
            }
            short i = 0;
            bool previous = false;
            for (const auto&[fst, snd] : speeds) {
                if (i >= 2 && speed >= R) {
                    break;
                }
                const short x = node->x + fst;
                const short y = node->y + snd;
                bool valid = true;
                if (x >= 0 && x < M && y >= 0 && y < N) {
                    const auto n = &grids[direction][speed - 2 + i][x][y];
                    if (n->value == 0 && !n->discovered) {
                        if (!previous) {
                            const short idx = fst != 0 ? abs(fst) / 2 : abs(snd) / 2;
                            for (short j = 1; j <= idx; j++) {
                                const short x1 = node->x + j * (fst == 0 ? 0 : (fst / abs(fst)));
                                const short y1 = node->y + j * (snd == 0 ? 0 : (snd / abs(snd)));
                                if (grids[direction][speed - 2 + i][x1][y1].value > j) {
                                    valid = false;
                                    break;
                                }
                            }
                            if (!valid) {
                                break;
                            }
                            for (short j = idx - 1; j >= 1; j--) {
                                const short x1 = x - j * (fst == 0 ? 0 : (fst / abs(fst)));
                                const short y1 = y - j * (snd == 0 ? 0 : (snd / abs(snd)));
                                if (grids[direction][speed - 2 + i][x1][y1].value > j) {
                                    valid = false;
                                    break;
                                }
                            }
                        } else {
                            const short x1 = x - (fst == 0 ? 0 : (fst / abs(fst)));
                            const short y1 = y - (snd == 0 ? 0 : (snd / abs(snd)));
                            if (grids[direction][speed - 2 + i][x1][y1].value > 1) {
                                valid = false;
                                previous = false;
                            }
                        }
                        if (valid) {
                            previous = true;
                            n->direction = direction;
                            n->speed = speed - 1 + i;
                            n->distance = node->distance + 1;
                            n->discovered = true;
                            if (x == 0 && y == N - 1) {
                                cout << n->distance << endl;
                                return 0;
                            }
                            q.push(n);
                        }
                    } else {
                        previous = false;
                    }
                } else {
                    previous = false;
                }
                i++;
            }
        }
    }
    return 0;
}
