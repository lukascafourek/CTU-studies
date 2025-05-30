class Cuboid:
    def __init__(self, a, b, c):
        self.a = a
        self.b = b
        self.c = c

    def compute_surface(self):
        return 2 * (self.a * self.b + self.b * self.c + self.a * self.c)

    def make_enlarged_copy(self, delta_a, delta_b, delta_c):
        return Cuboid(self.a + delta_a, self.b + delta_b, self.c + delta_c)

if __name__ == "__main__":
    c1 = Cuboid(1,2,3)
    print(c1.compute_surface())
    c3 = c1.make_enlarged_copy(1,2,1)
    print(type(c3))
    print(c3.compute_surface())