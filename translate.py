# -*- coding: utf-8 -*-
"""
Created on Sun Feb 25 20:22:21 2018

@author: rafal
"""
# https://stackoverflow.com/questions/17388213/find-the-similarity-percent-between-two-strings

import os
# os.chdir("C:/Users/rafal/Google Drive/LAMFO/Adicionar a automation")
os.chdir("C:/Users/b2657804/Documents/Meu Drive/LAMFO/Adicionar a automation")

import pandas as pd

cbo2002 = pd.read_csv("CBO2002 - Ocupacao.csv", sep = ";", dtype = "str",
                      encoding = "latin-1")

from googletrans import Translator

translator = Translator()
def traducao(self):
    return translator.translate(self).text

#--------------- teste com pouco ---------------
cbo = cbo2002.head()

cbo['trad'] = cbo.TITULO.progress_apply(traducao)
cbo
#-----------------------------------------------

from tqdm import tqdm, tqdm_pandas # Permite acompanhar o progresso do apply
tqdm_pandas(tqdm()) 

cbo2002['trad'] = cbo2002.TITULO.progress_apply(traducao)
#------------------------------------------------------------------------------

soc = pd.read_excel("All_Job_Zones.xls", dtype = "str", skiprows = 3,
                    names = ["Job_Zone", "code", "title"])

soc.head()

from difflib import SequenceMatcher

a = pd.DataFrame({'code': ['0'], 'title': [''], 'prox': ['0'], 'trad': ['']})

for trad in cbo2002.trad:
    print(trad)
    def similar(self):
        return SequenceMatcher(None, trad, self).ratio()
    
    soc['trad'] = trad
    soc['prox'] = soc.title.apply(similar)
    
    b = soc[(soc.prox == soc.prox.max()) & (soc.prox.max() > .5)].head(1)
    
    a = a.append(b, ignore_index = True)


a = a.drop_duplicates()

c = cbo2002.join(a.set_index("trad"), on = "trad", how = "left")

c.to_csv("translate_matching.csv", sep = ";", index = False)

