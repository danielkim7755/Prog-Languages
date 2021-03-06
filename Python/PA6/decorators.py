from misc import Failure

"""Class Profiled"""
class profiled(object):
    def __init__(self,f):
        self.__count=0
        self.__f=f
        self.__name__=f.__name__
    def __call__(self,*args,**dargs):
        self.__count+=1
        return self.__f(*args,**dargs)
    def count(self):
        return self.__count
    def reset(self):
        self.__count=0

"""Class Traced"""
class traced(object):
    """This is a decorator which prints an ASCII art tree of the recursive calls and their return values"""
    __spaceCount = 0
    # This function initializes the decorator with the input function
    def __init__(self,f):
        self.__name__=f.__name__
        self.__f = f

    def __call__(self,*args,**kwargs):
        # Prepare the the number of space strings and convert arguments into string
        spaceStr = "| " * traced.__spaceCount
        argStr = ", ".join([repr(x) for x in args])
        kwargStr = ", ".join([str(x) + "=" + str(kwargs[x]) for x in kwargs])

        # Print the traced format
        print(spaceStr + ",- " + self.__name__ + "(" + argStr + kwargStr + ")")

        # Increase nesting level and call function
        traced.__spaceCount += 1

        try:
            result = self.__f(*args,**kwargs)

            # Print the results at the original level with the correct format
            traced.__spaceCount -= 1
            spaceStr = "| " * traced.__spaceCount
            print(spaceStr + "`- " + repr(result))
            return result
        except Exception as someException:
            # Adjust level in case there is an exception
            traced.__spaceCount -= 1
            raise someException

"""Memoized Class"""
class memoized(object):
    """A decorator class that checks if the called function has been called and returns the value that was returned with the given arguments"""
    def __init__(self,f):
        self.__name__=f.__name__
        self.__f = f
        self.__sofar = dict()

    def __call__(self,*args,**kwargs):
        # Change arguments into string 
        argList = [repr(x) for x in args]
        kwargsList = [str(x) + "=" + str(kwargs[x]) for x in kwargs]
        key = "".join(argList+kwargsList)

        # Check if these arguments have been used already and return it
        if key in self.__sofar:
            if isinstance(self.__sofar[key],Exception):
                raise self.__sofar[key]

        # Store the result of the function
        else:
            try:
                result = self.__f(*args,**kwargs)
                self.__sofar[key] = result
            except Exception as someException:
                self.__sofar[key] = someException
                raise someException

        return self.__sofar[key]

        
        

# run some examples.  The output from this is in decorators.out
def run_examples():
    for f,a in [(fib_t,(7,)),
                (fib_mt,(7,)),
                (fib_tm,(7,)),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp.reset,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (even_t,(6,)),
                (quicksort_t,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (change_t,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                ]:
        print "RUNNING %s(%s):" % (f.__name__,", ".join([repr(x) for x in a]))
        rv=f(*a)
        print "RETURNED %s" % repr(rv)

@traced
def fib_t(x):
    if x<=1:
        return 1
    else:
        return fib_t(x-1)+fib_t(x-2)

@traced
@memoized
def fib_mt(x):
    if x<=1:
        return 1
    else:
        return fib_mt(x-1)+fib_mt(x-2)

@memoized
@traced
def fib_tm(x):
    if x<=1:
        return 1
    else:
        return fib_tm(x-1)+fib_tm(x-2)

@profiled
@memoized
def fib_mp(x):
    if x<=1:
        return 1
    else:
        return fib_mp(x-1)+fib_mp(x-2)

@traced
def even_t(x):
    if x==0:
        return True
    else:
        return odd_t(x-1)

@traced
def odd_t(x):
    if x==0:
        return False
    else:
        return even_t(x-1)

@traced
def quicksort_t(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_t([x for x in l[1:] if x<pivot])
    right=quicksort_t([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

@traced
@memoized
def quicksort_mt(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_mt([x for x in l[1:] if x<pivot])
    right=quicksort_mt([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

class ChangeException(Exception):
    pass

@traced
def change_t(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_t(l[1:],a)
    else:
        try:
            return [l[0]]+change_t(l,a-l[0])
        except ChangeException:
            return change_t(l[1:],a)

@traced
@memoized
def change_mt(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_mt(l[1:],a)
    else:
        try:
            return [l[0]]+change_mt(l,a-l[0])
        except ChangeException:
            return change_mt(l[1:],a)


