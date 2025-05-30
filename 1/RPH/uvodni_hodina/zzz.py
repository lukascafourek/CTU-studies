"""
>>> seznam = [1, 2, 3, 4]
>>> seznam
[1, 2, 3, 4]

>>> for element in seznam:
...     print(element)
...
1
2
3
4
>>> seznam = 2*seznam
>>> for element in seznam:
...     print(element)
...
1
2
3
4
1
2
3
4

>>> for i in range(0, len(seznam)):
...     print(i, ",", seznam[i])
...
0 , 1
1 , 2
2 , 3
3 , 4
4 , 1
5 , 2
6 , 3
7 , 4
>>> for i, item in enumerate(seznam):
...     print(i, ",", item)
...
0 , 1
1 , 2
2 , 3
3 , 4
4 , 1
5 , 2
6 , 3
7 , 4
"""