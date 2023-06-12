from gurobipy import *
import pandas as pd
import numpy as np
import os
wkdi = "/Library/gurobi951/macos_universal2/examples/python/data"

hw_2 = Model("hw_2") 
m = 3; n = 10;
b = [5, 8, 4, 6, 3, 7, 6, 9, 5, 8]
p = [3, 6, 5, 4, 1, 8, 5, 12, 7, 6]
K = 15

x = np.zeros(n)
for i in range(len(x)):
    x[i] = hw_2.addVar(lb = 0, vtype = GRB.CONTINUOUS, name = "x" + str(i+1))

z = np.zeros((n, n))
for i in range(z.shape[0]):
    for j in range(z.shape[1]):
        z[i][j] = hw_2.addVar(lb = 0, vtype = GRB.BINARY, name = "z_" + str(i+1) + str(j+1))

w = np.zeros((n, m))
for i in range(w.shape[0]):
    for j in range(w.shape[1]):
        w[i][j] = hw_2.addVar(lb = 0, vtype = GRB.BINARY, name = "w_" + str(i+1) + str(j+1))

# 接下來不會寫