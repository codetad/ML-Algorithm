###SVR(support vector regression)

import numpy as np
from cvxopt import matrix, solvers

def SVR(x, y, lambd = 0.01, Eps = 0.3):
    n = np.shape(x)[0]
    A = np.hstack((np.eye(n), -np.eye(n)))
    B = np.hstack((np.eye(n), np.eye(n)))
    ONE = np.ones((n,1))
    K = x @ x.T
    
    P = matrix(A.T @ K @ A / 3)
    q = matrix((y.T @ A - 2*ONE.T @ B).T)
    G = matrix(np.vstack((np.eye(2*n), -np.eye(2*n))))
    h = matrix(np.vstack((np.ones((2*n,1)),  np.zeros((2*n,1)))))
    A_ = matrix(ONE.T @ A)
    b = matrix(np.array([[0.0]]))
    
    sol = solvers.qp(P,q,G,h,A_,b)
    
    theta = np.array(sol['x']).flatten()
    sv_index = np.where((0 + 0.4e-6 < theta ) & (theta < 1 - 0.4e-7))[0]
    
    temp = np.zeros((n,1))
    sv_index_2 = np.unique(sv_index % 12).tolist()
    for i in range(n):
        temp[i] = theta[i] - theta[i+n]
        
    beta = np.sum(temp * x, axis=0) /lambd
    temp2 = 0
    for i in sv_index_2:
        temp2 = temp2 + (y[i] - x[i,:] @ beta)
    beta0 = temp2/len(sv_index_2)
    
    obj = {"beta" : beta, "beta0" : beta0, "index" : sv_index_2}
    return obj


### Simulation
np.random.seed(43)
x = np.random.poisson(4, (12,6))
y = np.array([1,1,1,1,1,1,-1,-1,-1,-1,1,1])
result = SVR(x,y)
print(result)
