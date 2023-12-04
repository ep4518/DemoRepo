#!/usr/bin/env python3
from sys import argv
import numpy as np

if len(argv) != 3:
    print("Usage: python Ass3Q3.py <N>")

def binomial_tree_call(N = 2, S0 = 40, K = 40, rf = 0.04, T = 0.5, vol = 0.3):
    '''
    Returns a binomial tree option pricing model for a European call option
    
    '''
    dt = T / N
    u = np.exp(vol * np.sqrt(dt))
    d = 1 / u
    q = (np.exp(rf * dt) - d) / (u - d)

    psi = np.diag(np.ones(N+1) * q) + np.diag(np.ones(N) * (1 - q), k = 1)
    psi = psi * np.exp(-rf * dt) ### Multiply the psi matrix by e^(-r*dt)

    S = np.zeros((N+1, N+1))

    # Calculate stock prices at each node
    for i in range(N+1):
        for j in range(i+1):
            S[j, i] = S0 * (u**(i-j)) * (d**j)
    
    V = np.zeros((N+1,1))

    for i in range(N+1):
        V[i, 0] = max(S[i, N] - K, 0)

    V1 = np.linalg.matrix_power(psi, N) @ V

    price = round(V1[0][0], 5)

    return price

if len(argv) == 2:
    print(
        f"N = {argv[1]}, option price = {binomial_tree_call(int(argv[1]))}"
        )
else:
    print(
        f"N = 2, option price = {binomial_tree_call(2, 100, 100, 0.05, 1/12, 0.3)}"
        )