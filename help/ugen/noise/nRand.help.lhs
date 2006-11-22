nrand id lo hi n

Generates a single random float value in a sum of `n' uniform
distributions from `lo' to `hi'.

n = 1 : uniform distribution - same as Rand
n = 2 : triangular distribution
n = 3 : smooth hump
as n increases, distribution converges towards gaussian

> fsinosc AR (nrand 0 IR 1200.0 4000.0 (MCE [2, 5])) 0 * line KR 0.2 0 0.01 2
