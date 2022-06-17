import matplotlib.pyplot as plt
import csv

import numpy as np
import math

array = []
SAMPLERATE = 44100 # 44100 samples in one second

with open('fft.csv') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    for row in reader:
        (time, freq) = (row[0], row[1])
        array.append((float(time), float(freq)))

N = len(array)

times = [time for (time, _) in array]
freqs = [freq for (_, freq) in array][0:(N//2)]

freqs_log = [freqs[int(math.log(i+1))] for i in range(N//2)]

fs_over_N = SAMPLERATE / N

xaxis_freq = [x * fs_over_N for x in range(N // 2)]
xaxis_time = [x / 44100 for x in range(N)]

fig, axs = plt.subplots(2)

plt.rcParams['toolbar'] = 'toolbar2'
axs[0].plot(xaxis_time, times, color='red')
axs[1].plot(xaxis_freq, freqs, color='blue')
axs[1].set_xscale('log')



largest_freq_idx = 0
for i in range(len(freqs)):
    if freqs[i] > freqs[largest_freq_idx]:
        largest_freq_idx = i

print(f"Strongest frequency: {largest_freq_idx * fs_over_N}Hz at {freqs[largest_freq_idx]}")


plt.show()