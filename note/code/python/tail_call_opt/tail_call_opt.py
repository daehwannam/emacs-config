
# modified version of TCO code
# http://code.activestate.com/recipes/474088-tail-call-optimization-decorator/


# This program shows off a python decorator(
# which implements tail call optimization. It
# does this by throwing an exception if it is 
# it's own grandparent, and catching such 
# exceptions to recall the stack.

import sys
import functools


class TailRecurseException(Exception):
    def __init__(self, args, kwargs):
        self.args = args
        self.kwargs = kwargs


def tco(func):
    return tco_depth(5)(func)


def tco_depth(max_depth=2):
    def tco(func):
        """
        This function decorates a function with tail call
        optimization. It does this by throwing an exception
        if it is it's own grandparent, and catching such
        exceptions to fake the tail call optimization.

        This function fails if the decorated
        function recurses in a non-tail context.
        """

        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            curr_f = sys._getframe()
            prev_f = curr_f.f_back
            depth = 0
            while prev_f and depth < max_depth:
                if curr_f.f_code == prev_f.f_code:
                    raise TailRecurseException(args, kwargs)
                prev_f = prev_f.f_back
                depth += 1
            else:
                while 1:
                    try:
                        return func(*args, **kwargs)
                    except TailRecurseException as e:
                        args = e.args
                        kwargs = e.kwargs
        return wrapper
    return tco
