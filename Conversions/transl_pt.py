# -*- coding: utf-8 -*-
"""
Created on Sun Feb 25 20:22:21 2018

@author: RafaelEstatistico
"""
# https://stackoverflow.com/questions/17388213/find-the-similarity-percent-between-two-strings

#%% Importing
import os
os.chdir("C:/Users/rafal/Google Drive/LAMFO/AutomationJobs")
# os.chdir("C:/Users/b2657804/Documents/Meu Drive/LAMFO/Adicionar a automation")

import pandas as pd

cbo2002 = pd.read_csv("Conversions/CBO_Check2.csv", sep = ";", dtype = "str",
                      encoding = "latin-1")

jobzones = pd.read_excel("Data/All_Job_Zones.xls", dtype = "str", skiprows = 3,
                         names = ["Job_Zone", "code", "title"])

#%% Using Google Translator API - From Portuguese to English 
from googletrans import Translator

translator = Translator()
def tra_pt(self):
    return translator.translate(self, dest = 'en').text

from tqdm import tqdm # For check apply progress
tqdm.pandas()  # tqdm_pandas 
#cbo2002['trad'] = cbo2002.TITULO.progress_apply(tra_pt)

#%% Using Google Translator API - From Portuguese to English 
import numpy as np

def unif(x, y):
    return y if x == '' else x

a = pd.DataFrame({'TITULO': [''], 'trad_': ['0']})
b = pd.DataFrame({'TITULO': [''], 'trad_': ['0']})

cbo2002['trad'] = ""
count = sum(cbo2002.trad == '')
pbar = tqdm(total = count + 1)
while count > 0:
    try:
        for name in tqdm(cbo2002.TITULO[cbo2002.trad == '']):
            b['TITULO'] = name
            b['trad_'] = tra_pt(name)
            a = a.append(b, ignore_index = True)
    except:
        print("Continue")
        pass
    
    a = a.drop_duplicates()
    c = cbo2002.join(a.set_index("TITULO"), on = "TITULO", how = "left")
    
    c['trad'] = pd.DataFrame(np.vectorize(unif)(c.trad, c.trad_))
    
    cbo2002 = c.drop('trad_', 1)
    
    count = sum(cbo2002.trad == '')
    pbar.update(6803 - count)
    
pbar.close()

#%% Measuring name similarity
from difflib import SequenceMatcher

a = pd.DataFrame({'Job_Zone': [''], 'code': ['0'], 'title': [''], 'trad': [''],
                  'prox': ['']})

for trad in cbo2002.trad:
    print(trad)
    def similar(self):
        return SequenceMatcher(None, trad, self).ratio()
    
    jobzones['trad'] = trad
    jobzones['prox'] = jobzones.title.apply(similar)
    
    b = jobzones[(jobzones.prox == jobzones.prox.max()) & 
                (jobzones.prox.max() > .6)].head(3)
    
    a = a.append(b, ignore_index = True)


a = a.drop_duplicates()

c = cbo2002.join(a.set_index("trad"), on = "trad", how = "left")

#%% Saving
c.to_csv("Conversions/trans_pt.csv", sep = ";", index = False)
