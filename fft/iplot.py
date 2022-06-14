import matplotlib.pyplot as plt
import csv

import numpy as np

array = []

with open('ifft.csv') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    for row in reader:
        (time, freq) = (row[1], row[0])
        array.append((float(time), float(freq)))

times = [time for (_, time) in array]
freqs = [freq for (freq, _) in array]

ax1 = plt.subplot()
l1, = ax1.plot([x for x in range(len(array))], times, color='red')
ax2 = ax1.twinx()
l2, = ax2.plot(freqs, color='blue')

plt.legend([l1, l2], ["times", "freqs"])

plt.show()