#include <vector>
#include <cstdio>

void fn(std::vector<int>& v) {
    v.push_back(6);
}

std::vector<int>& fn2() {
    std::vector<int> v{1, 2};
}

int main() {
    std::vector<int> v{1, 2, 3};

    int& i = v[0];

    i = 1;
    v[2] = v[0];

    v.push_back(4);
    v.push_back(5);

    fn(v);

    for (auto n: v) {
        printf("%d\n", n);
    }

    printf("%d\n", v[0]);

    return 0;
}
