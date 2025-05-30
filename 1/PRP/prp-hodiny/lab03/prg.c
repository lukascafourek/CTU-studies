#include <stdio.h>

int main() {
    int amount;
    int r = scanf("%d", &amount);
    if (r == 1) {
        if (amount > 0) {
            printf("%d CZK = %.2lf euro\n", amount, amount / 24.52);
            int counter = 0;
            for (int coin = 20; coin >= 1; coin /= 2) {
                if (amount / coin != 0)
                    printf("Number of %2d CZK: %d\n", coin, amount / coin);
                counter += amount / coin;
                amount = amount % coin;
            }
            printf("Count of coins: %d\n", counter);
            printf("%lf\n", (double)counter / 5);
        } else
            printf("Wrong input, select amount > 0\n");
    } else
        printf("Wrong input!\n");
    return 0;
}
