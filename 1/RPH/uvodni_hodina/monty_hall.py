import random

def monty(strategy):
    """1. The host randomly chooses a door for the car
    2. The contestant randomly selects a door as pick
    3. The host randomly selects a non-car, non-pick door to be opened
    4. If strategy == 'switch', contestant changes pick
    5. return true if the pick is the door with the car
"""
    doors = (1,2,3)
    car = random.choice(doors)
    pick = random.choice(doors)
    opened = random.choice([d for d in doors if d != car and d != pick])
    if strategy == 'switch':
        pick = next(d for d in doors if d != pick and d != opened)
    return pick == car

if __name__ == "__main__":
    from collections import Counter
    print(Counter(monty('switch') for _ in range(10 ** 5)))