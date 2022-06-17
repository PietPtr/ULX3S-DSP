from scipy.io import wavfile
import csv

samplerate, data = wavfile.read('long.wav')

array = []
m = max(data)
i = 0
for d in data:
    array.append(d / m)
    i += 1

print(array)
with open('long_sample.csv', 'w') as csv_file:
    writer = csv.writer(csv_file)
    for s in array:
        writer.writerow([s])