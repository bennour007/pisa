import pickle
import numpy as np
import os 




path = "modeling/results"
files = os.listdir(path)
full_files = []

for fi in files:
  full_files.append(path + str('/') + fi)
  
# file = "modeling/res_hun"
# open_file = open(file, "rb")
# res_hun = pickle.load(open_file)
# open_file.close()

l = {}


for (file, fifo) in zip(full_files, files):
    with open(file, "rb") as f:
        l[fifo] = pickle.load(f)
