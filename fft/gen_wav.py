from scipy.io import wavfile
import numpy as np


my_data = np.genfromtxt('samples.csv', delimiter=',')
normalized = my_data / (np.linalg.norm(my_data) * 1 / 8)

print(normalized)

wavfile.write('out.wav', 44100, normalized)