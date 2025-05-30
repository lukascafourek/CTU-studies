class MyVector:
    def __init__(self, vv):
        self.vv = vv

    def get_vector(self):
        return self.vv

    def __mul__(self, other):
        total = 0
        for i in range(len(self.vv)):
            total += self.vv[i] * other.vv[i]
        return total

    def __add__(self, other):
        cc = []
        for i in range(len(self.vv)):
            cc.append(self.vv[i] + other.vv[i])
        return MyVector(cc)

    def norm(self):
        total = 0
        for i in range(len(self.vv)):
            total += self.vv[i]**2
        return total**0.5

if __name__ == "__main__":
    vec1 = MyVector([1,2,3]) # vektory mohou byt i jine dimenze nez 3!
    vec2 = MyVector([3,4,5])
    print(vec1.get_vector()) # priklad ziskani seznamu
    dot_product = vec1*vec2  # vypocet skalarniho soucinu, pretizeny operator *, vola se __mul__
    print(dot_product)       # jen kontrolni vypis
    a = MyVector([1,2,3])
    b = MyVector([2,3,4])
    c = a + b
    print(c.get_vector())