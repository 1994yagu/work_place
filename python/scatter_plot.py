# -*- coding: utf-8 -*-
"""
Created on Fri Jul  6 13:31:48 2018

@author: yaguchi
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
#from pandas.plotting import scatter_matrix

s_dir='/Users/yaguchi/HDD2/out/NAP_area/rot/'

ifile1='nap_ic.txt'
ifile2='nap_rot.txt'

file1=open(s_dir+ifile1,'r')
ic=file1.read()

file2=open(s_dir+ifile2,'r')
rot=file2.read()

plt.scatter(ic,rot)
plt.xlabel('ice concetration')
plt.ylabel('ice concetration')

plt.show


