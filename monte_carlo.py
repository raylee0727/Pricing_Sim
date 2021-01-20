import numpy as np
import pandas as pd

sim_price = 0 # the end of simulation price
steps = 10000 # steps of monte carlo simulation
table = pd.DataFrame(
    columns=["Stock price \n at start of period",'Random sample \n for epsilon','Change in stock price \n during period'])

for step in range(steps):
    price = 100 # initial stock price
    for i in range(12):
        '''monte carlo simulation for 10 weeks'''
        epsilon = np.random.normal(0,1) # sample from normal distribution
        s = price # iterative new price
        delta_s = 0.00288*s+0.0416*s*epsilon # the eqution of geometric Brownian motion
        price = price + delta_s # initial price + delta S = new price
        print(price)
    sim_price = sim_price + price
sim_price = sim_price/steps

table = pd.DataFrame(
    columns=["Stock price \n at start of period",'Random sample \n for epsilon','Change in stock price \n during period'])


print(sim_price)