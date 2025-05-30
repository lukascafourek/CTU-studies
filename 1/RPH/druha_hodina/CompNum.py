class ComplexNumber:
    def __init__(self, re, im):
        self.re = re
        self.im = im

    def to_string(self):
        return "%.2f + %.2fi" % (self.re, self.im)

    def size(self):
        return (self.re ** 2 + self.im ** 2) ** (1/2)

    def inc_parts(self, re, im):
        self.re += re
        self.im += im

    def inc(self, other):
        self.re += other.re
        self.im += other.im

    def add(self, other):
        return ComplexNumber(self.re + other.re, self.im + other.im)

    def __str__(self):
        return self.to_string()

    def __abs__(self):
        return self.size()

    def __add__(self, other):
        return self.add(other)

if __name__ == "__main__":
    a = ComplexNumber(3,4)
    #print(a.to_string())
    #print(a.size())
    #a.inc_parts(1,2)
    #print(a.to_string())
    b = ComplexNumber(1,2)
    a.inc(b)
    #print(a.to_string())
    c = a+b
    print(c.to_string(), a.to_string(), b.to_string())
    print(a)
    print(abs(a))
    print(a+b)