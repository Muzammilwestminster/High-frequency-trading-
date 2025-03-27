# -*- coding: utf-8 -*-
"""
Created on Tue Jan 14 19:35:08 2025

@author: yueya
"""

import numpy as np
import pandas as pd
from linearmodels.system import SUR
import statsmodels.api as sm
from scipy.stats import f


data = pd.read_csv("C:/Users/yuey/OneDrive - University of Westminster/2024-25/HFT/L5/LOB_9065_2_17.csv")
data = data[(data['time_S'] >= 28920) & (data['time_S'] <= 59400)]

data['d']=np.nan
data.loc[(data['direction'] == 'B') & ~data['Price'].isna(), 'd'] = -1
data.loc[(data['direction'] == 'S') & ~data['Price'].isna(), 'd'] = 1
data=data[(data['Type']=='M')|(data['Type']=='P')]
## define variables ##
panel=pd.DataFrame()
panel['d'] = data['d']  # Already exists
panel['delta_d'] = data['d'] - data['d'].shift(1)
panel['q'] = data['d'] * data['Volume']   #quantity can be rescaled by daily average trading volume
panel['delta_q'] = panel['q'] - panel['q'].shift(1)
panel['q_lag'] = panel['q'].shift(1)
panel['p'] = data['Price']
panel['delta_p'] = panel['p'] - panel['p'].shift(1)
panel=panel.dropna()
## regression ##
#2-way:
#first regression
model1 = sm.OLS(panel['delta_p'] ,panel.iloc[:,:2] )
results1 = model1.fit()  
coeffs1=results1.params 
#second regression
#unrestricted
model2 = sm.OLS(panel['delta_p'] ,panel.iloc[:,:4] )
results2 = model2.fit()  
coeffs2=results2.params 
#restricted
model3 = sm.OLS(panel['delta_p'] ,panel.iloc[:,[1,2]] )
results3 = model3.fit()  
coeffs3=results3.params
#compare restricted and unrestricted models to see whether the restrictions are significant
RSS_u = np.sum(results2.resid**2)
RSS_r = np.sum(results3.resid**2)
m = 2  # Number of restrictions
k = panel.iloc[:,:4].shape[1]  # Number of parameters in unrestricted model
n = len(panel['delta_p'])

F = ((RSS_r - RSS_u) / m) / (RSS_u / (n - k))

# Calculate p-value
p_value = 1 - f.cdf(F, m, n - k)  #if very small, then the restrictions are significant.

# 3-way
equations = {
    'eq1': 'q ~ q_lag',  # Equation 1: AR(1) for q_t
    'eq2': 'delta_p ~ q + q_lag + delta_d'  # Equation 2: Price impact regression
}

# Fit the SUR model
sur_model = SUR.from_formula(equations, panel)
results = sur_model.fit()
