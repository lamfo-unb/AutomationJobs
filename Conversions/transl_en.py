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

cbo2002 = pd.read_csv("Conversions/CBO_Check.csv", sep = ";", dtype = "str",
                      encoding = "latin-1")

jobzones = pd.read_excel("Data/All_Job_Zones.xls", dtype = "str", skiprows = 3,
                         names = ["Job_Zone", "code", "title"])

#%% Using Google Translator API - From English to Portuguese
from googletrans import Translator

translator = Translator()
def tra_en(self):
    return translator.translate(self, dest = 'pt').text


from tqdm import tqdm # For check apply progress
tqdm.pandas()  # tqdm_pandas 

jobzones['trad'] = jobzones.title.progress_apply(tra_en)

#%% Measuring name similarity
from difflib import SequenceMatcher

a = pd.DataFrame({'prox': ['0'], 'trad': ['']})

for trad in jobzones.trad:
    print(trad)
    def similar(self):
        return SequenceMatcher(None, trad, self).ratio()
    
    cbo2002['trad'] = trad
    cbo2002['prox'] = cbo2002.TITULO.apply(similar)
    
    b = cbo2002[(cbo2002.prox == cbo2002.prox.max()) & 
                (cbo2002.prox.max() > .6)].head(3)
    
    a = a.append(b, ignore_index = True)


a = a.drop_duplicates()

c = jobzones.join(a.set_index("trad"), on = "trad", how = "left")

#%% Saving
c.to_csv("Conversions/trans_en.csv", sep = ";", index = False)
