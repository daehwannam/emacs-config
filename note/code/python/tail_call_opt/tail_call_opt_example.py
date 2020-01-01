
from tail_call_opt import tco


@tco
def fib(i, current=0, next=1):
    if i == 0:
        return current
    else:
        return fib(i - 1, next, current + next)


# print(fib(10))
# print(fib(11))
# print(fib(12))
# print(fib(13))
# print(fib(10000))


# decorator should be applied to only one of the functions
@tco
def even(n):
    print('--even--')
    if n == 0:
        return True
    else:
        return odd(n - 1)


def odd(n):
    print('--odd--')
    if n == 0:
        return False
    else:
        return even(n - 1)


# print(even(10000))
print(even(10))
# print(odd(10))


@tco
def total_even(n, acc=0):
    if n == 0:
        return acc + 0
    return total_odd(n - 1, acc=acc + n)


def total_odd(n, acc=0):
    if n == 1:
        return acc + 1
    return total_even(n - 1, acc=acc + n)


# print(total_even(10))
print(total_even(10000))
