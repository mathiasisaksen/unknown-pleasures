
import cv2
import numpy as np
import glob
 
img_array = []
filenames = sorted(glob.glob('*.png'), key = lambda x: int(x.split(".")[0]))
first_time = True
size = (2000, 2000)
fps = 25
for i, filename in enumerate(filenames):
    print(i)
    img = cv2.imread(filename)
    img = cv2.resize(img, size)
    height, width, layers = img.shape
    if first_time:
        out = cv2.VideoWriter('video.mp4', 24, fps, size)
        first_time = False
    out.write(img)

out.release() 
 



