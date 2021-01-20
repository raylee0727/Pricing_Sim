import numpy as np
import requests as rq
from csv import DictReader
from bs4 import BeautifulSoup 
from datetime import date
from io import StringIO 
import urllib.request as urllib2
from urllib.request import Request, urlopen
from itertools import groupby
import pandas as pd
import csv


site= "https://www.artemis.bm/dashboard/cat-bond_ils-issuance-banks-brokers/"

column_name = []
for i in range(1,9):
    column_name.append("column-"+str(i))

row_odd = []
for i in range(3,19,2):
    row_odd.append("row-" +str(i)+ " odd")

row_even = []
for i in range(2,19,2):
    row_even.append("row-" +str(i)+ " even")



req = Request(site, headers={'User-Agent': 'XYZ/3.0'})
webpage = urlopen(req, timeout=10)
soup = BeautifulSoup(webpage, "html.parser")
name_box = soup.find("td", attrs={"class": "column-2"})
name = name_box.text
# name_list = name.split("\n")


print(name)



# # define separator keys
# def split_condition(x):
#     return x in {''}
# # define groupby object
# grouper = groupby(name_list[9:], key=split_condition)
# # convert to dictionary via enumerate
# res = dict(enumerate((list(j) for i, j in grouper if not i), 1))

# # make dataframe
# col = []
# for i in range(1,len(res),2):
#     col.append(res[i])
# numpy_array = np.array(col)
# transpose = numpy_array.T
# col = transpose.tolist()

# col_2 = []
# for i in range(2, len(res)+2, 2):
#     # print(i)
#     col_2.append(res[i])

# d = {'Issuer': col[0], 'Cedent': col[1], 'Risks / Perils covered': col[2], 
#     'Size': col[3], 'Date': col_2}
# df = pd.DataFrame(data=d)
# df.to_csv(r"C:\Users\Andy\Desktop\洪災\code\ARTEMIS_2.csv", index=False)
