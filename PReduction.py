
f = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
n = 0
N = len(f)
r = 0

while (n < N):
    if f[n] % 2 == 1:
        r = r + f[n]
    
    if f[n] % 2 == 0:
        r = r + 0

    n = n + 1


print(r)
