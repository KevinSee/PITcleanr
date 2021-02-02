# Author: Kevin See
# Purpose: create a hexagon sticker as a logo for R package
# Created: 2/1/2021
# Last Modified: 2/1/2021
# Notes: Based on hexSticker package. GitHub site: https://github.com/GuangchuangYu/hexSticker

#-----------------------------------------------------------------
# load needed libraries
# library(tidyverse)
library(hexSticker)

#-----------------------------------------------------------------
# image to use
img_path = "analysis/figures/broom.png"

# create a logo sticker and save it
s = sticker(magick::image_read(img_path),
            package = "PITcleanr",
            p_size = 9,
            p_family = "sans",
            s_x = 1,
            s_y = .75,
            s_width = 1,
            s_height = 1,
            # spotlight = T,
            # h_fill = "#9e0b0f",
            h_fill = "#63065f",
            h_color = "#fec619",
            filename = "analysis/figures/PITcleanr.png")

plot(s)
