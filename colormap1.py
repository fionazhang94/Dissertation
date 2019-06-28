#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 01:31:38 2019

@author: yangfanzhang
"""

# https://jakevdp.github.io/blog/2014/10/16/how-bad-is-your-colormap/
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cmap
from colorspacious import cspace_convert

x = np.linspace(0, 6)
y = np.linspace(0, 3)[:, np.newaxis]
z = np.exp(-y)*np.cos(x**2)
np.cos(x**2).shape
zmax, zmin = z.max(), z.min()
z = (z - zmin)/(zmax - zmin)
jet = cmap.get_cmap('jet')
z_sRGBa = jet(z)


image_sRGB = z_sRGBa[:,:,0:3]
# Function from online source for comparing two images side by side:
def compare_images(*new):
    image_width = 2  # inches
    total_width = (1 + len(new)) * image_width
    height = image_width / image_sRGB.shape[1] * image_sRGB.shape[0]
    fig = plt.figure(figsize=(total_width, height))
    ax = fig.add_axes((0, 0, 1, 1))
    ax.imshow(np.column_stack((image_sRGB,) + new))

# Setup for colorspacious module:
cvd_space1 = {"name": "sRGB1+CVD",
             "cvd_type": "protanomaly",
             "severity": 50}
cvd_space2 = {"name": "sRGB1+CVD",
             "cvd_type": "protanomaly",
             "severity": 100}
# Compare our original plot with chosen colormap to the altered one:
image_deuteranomaly_sRGB = cspace_convert(image_sRGB, cvd_space1, "sRGB1")
image_deuteranomaly_sRGB2 = cspace_convert(image_sRGB, cvd_space2, 'sRGB1')
compare_images(np.clip(image_deuteranomaly_sRGB, 0, 1),
              np.clip(image_deuteranomaly_sRGB2,0,1))