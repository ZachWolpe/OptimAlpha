from pandas_datareader import data
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
plt.style.use('default')
from datetime import date

# ---- ETFs ----x
available_EFTs = pd.read_excel('repo/data/EFTs_from_JSE.xlsx')
print(available_EFTs.Alpha.toList())