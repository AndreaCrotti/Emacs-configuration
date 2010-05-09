# Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million.

# need a dynamic programming approach, using only one two cells array
 
def sum_fib(n):
    "sum up even fibonacci numbers until max"
    tot = 0
    tup = [1, 1]
    while True:
        tup[0], tup[1] = sum(tup), tup[0]
        if tup[0] > n:
            return tot
        if tup[0] % 2 == 0:
            tot += tup[0]
            
print sum_fib(4000000)


