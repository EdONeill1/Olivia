def f(b, n):
    if b == False:
        return n
    else:
        return f(b, n + 1)



x = 0
cond = x < 10
print(f(cond, x))