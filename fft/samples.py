import matplotlib.pyplot as plt
import csv

import numpy as np

array = []

with open('samples.csv') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    for row in reader:
        (real, imag) = (row[0], row[1])
        array.append((float(real), float(imag)))

print(array)

# plt.title("Line graph")
# plt.xlabel("X axis")
# plt.ylabel("Y axis")
plt.plot([x for x in range(len(array))], [real for (real, _) in array], color ="red")
plt.show()

