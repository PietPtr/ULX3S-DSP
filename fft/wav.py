from scipy.io import wavfile

samplerate, data = wavfile.read('sample1024.wav')

array = []
for d in data:
    array.append(d / max(data))

print(array)