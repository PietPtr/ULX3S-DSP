import matplotlib.pyplot as plt
import csv

import numpy as np

array = []

with open('fft.csv') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    for row in reader:
        (time, freq) = (row[0], row[1])
        array.append((float(time), float(freq)))

maxfreq = 0
maxsamp = 0
for (samp, freq) in array:
    maxfreq = max(freq, maxfreq)
    maxsamp = max(samp, maxsamp)

normalized = []
for (time, freq) in array:
    normalized.append((time, freq * 0.025))

# plt.title("Line graph")
# plt.xlabel("X axis")
# plt.ylabel("Y axis")
plt.plot([x for x in range(len(normalized))], [time for (time, _) in normalized], color ="red")
plt.plot([x for x in range(len(normalized))], [freq for (_, freq) in normalized], color='blue')

plt.show()