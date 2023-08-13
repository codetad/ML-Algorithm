import numpy as np
from scipy.linalg import solve_triangular


def S(z, lambd):
    return (z - lambd) * (z > lambd) + (z + lambd) * (z < -lambd) + 0 * (abs(z) - lambd < 0)


def admm(x, y, lambd=1, rho=0.1, alpha=0.5, max_iter=100, abs_tol=1e-6, rel_tol=1e-6):
    # inititate variables
    n, p = x.shape
    beta = z = u = z_new = np.repeat(0, p)

    # variables for termination check
    r = s = eps_pri = eps_dual = 0

    # Calculate matrices for beta update
    xx = np.transpose(x) @ x
    xy = np.transpose(x) @ y

    Q = xx + rho * np.identity(p)
    L = np.linalg.cholesky(Q)

    L_inv = solve_triangular(L, np.identity(p), lower=True)
    Q_inv = np.transpose(L_inv) @ L_inv

    for t in range(max_iter):
        beta = Q_inv @ (xy + rho * (z - u))
        z_new = S(beta + u, lambd / rho) * (1 / (1 + lambd * (1 - alpha)))
        u = u + (beta - z_new)

        # termination criterion
        dum = beta - z_new

        r = np.transpose(dum) @ dum
        dum2 = rho * (z_new - z)

        s = np.transpose(dum2) @ dum2

        if r <= 0.0001 and s <= 0.0001:
            break
        z = z_new

    return (beta, z)


######

import random
random.seed(1)

n = 7
p = 5
x = np.random.normal(0,1,(n,p))
e = np.random.normal(0,1,(n))


beta = np.zeros(p)
beta[0:3] = 1
#print(beta)
y = np.dot(x, beta) + e

#print(np.transpose(x)@ y)
y = np.array(y)

obj = admm(x,y,lambd=1)
print(obj)