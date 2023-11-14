#!/usr/bin/env python3
class Jar:
    def __init__(self, capacity=12):
        if capacity >= 0 and isinstance(capacity, int):
            self._capacity = capacity
        else:
            raise ValueError("Capacity has to be a positive integer.")
        self.n = 0



    def __str__(self):
        return self.n * '🍪'

    def deposit(self, n):
        self.n += n


    def withdraw(self, n):
        if self.n - n < 0:
            raise ValueError("Not enough cookies")
        else:
            self.n = max(0, self.n - n)

    @property
    def capacity(self):
        return self._capacity


    @property
    def size(self):
        return self.n

def main():
    jar = Jar(6)
    print(str(jar.capacity))

    print(str(jar))

    jar.deposit(2)

    print(str(jar))
    jar.withdraw(1)
    print(str(jar))


if __name__ == "__main__":
    main()

