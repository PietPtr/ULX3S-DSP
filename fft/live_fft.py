"""
Given a wav file, shows how the fft changes for a given N at one sample per frame.
"""
from scipy.io import wavfile
import matplotlib.pyplot as plt
import csv

import numpy as np
import math

import time
from matplotlib import pyplot as plt
import numpy as np

N = 1024

samplerate, data_list = wavfile.read('long.wav')
data = np.array(data_list)
max_val = max(abs(min(data)), max(data))
data = data / max_val

def live_update_demo(blit = False):
    x = np.linspace(0,N/44100, num=N)
    
    fig = plt.figure()
    time_in_ax = fig.add_subplot(2, 1, 1)
    freq_in_ax = fig.add_subplot(2, 1, 2)

    line_time, = time_in_ax.plot([], lw=3)
    line_freq, = freq_in_ax.plot([], lw=3)

    time_in_ax.set_xlim(0, N/44100)
    time_in_ax.set_ylim([1.1, -1.1])

    freq_in_ax.set_xlim(0, N // 2)
    freq_in_ax.set_ylim(0, 1)

    fig.canvas.draw()   # note that the first draw comes before setting data 


    if blit:
        # cache the background
        axbackground = fig.canvas.copy_from_bbox(time_in_ax.bbox)

    plt.show(block=False)


    t_start = time.time()
    k=0.0

    for i in np.arange(0, len(data), 32):
        if i % N == 0:
            print(i)
        frame = data[i:i+N]

        line_time.set_data(x, frame)
        fft_result = np.array(list(map(abs, np.fft.fft(frame, N))))
        fft_result = fft_result / np.linalg.norm(fft_result)

        line_freq.set_data(np.arange(len(fft_result)), list(fft_result))
        freq_in_ax.set_xscale('symlog')
        
        if blit:
            # restore background
            fig.canvas.restore_region(axbackground)

            # redraw just the points
            time_in_ax.draw_artist(line_time)
            freq_in_ax.draw_artist(line_time)

            # fill in the axes rectangle
            fig.canvas.blit(time_in_ax.bbox)

            # in this post http://bastibe.de/2013-05-30-speeding-up-matplotlib.html
            # it is mentionned that blit causes strong memory leakage. 
            # however, I did not observe that.

        else:
            # redraw everything
            fig.canvas.draw()

        fig.canvas.flush_events()
        #alternatively you could use
        #plt.pause(0.000000000001) 
        # however plt.pause calls canvas.draw(), as can be read here:
        #http://bastibe.de/2013-05-30-speeding-up-matplotlib.html


live_update_demo(False)