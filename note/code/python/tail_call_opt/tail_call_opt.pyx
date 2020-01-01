
# modified version of TCO for parallel operations
# http://fiber-space.de/wordpress/2009/04/20/tail-recursion-decorator-revisited/

cdef class tco:
    cdef int CONTINUE
    cdef object func
 
    def __init__(self, func):
        self.func = func
        self.CONTINUE = id(object())
 
    def __call__(self, *args, firstcall=True, **kargs):
        if self.firstcall:
            kargs['firstcall'] = False
            try:
                while True:
                    result = self.func(*args, **kargs)
                    if all([isinstance(result, tuple),
                            len(result) > 0,
                            result[0] == self.CONTINUE]): # update arguments
                        _, args, kargs = result
                    else: # last call
                        return result
        else: # return the arguments of the tail call
            self.argskargs = args, kargs
            return self.CONTINUE, args, kargs
