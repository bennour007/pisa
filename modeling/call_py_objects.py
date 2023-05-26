import pickle
import numpy as np
import os 




file_main = "modeling/res_all"
open_file_main = open(file_main, "rb")
res_all = pickle.load(open_file_main)
open_file_main.close()
        

path = "modeling/results"
files = os.listdir(path)
full_files = []

for fi in files:
  full_files.append(path + str('/') + fi)
  


l = {}


for (file, fifo) in zip(full_files, files):
    with open(file, "rb") as f:
        l[fifo] = pickle.load(f)
        


