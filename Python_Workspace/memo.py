"""
Automatic Dynamic Programming
"""
class memoize:
    def __init__(self, fn):
        self.fn, self.memo = fn, dict()
    def __call__(self, arg):
        if arg not in self.memo:
            self.memo[arg] = self.fn(arg)
        return self.memo[arg]

@memoize
def factorial(n):
    return reduce(lambda a,b: a*b, range(1,n+1), 1)

@memoize
def fib(n):
    a, b = 0, 1
    for i in range(n-1):
        a, b = b, a+b
    return a
