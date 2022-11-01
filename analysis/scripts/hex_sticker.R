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
# Biomark colors
# Primary
# dark blue
"#282D46"
# light blue
"#879FC2"
# black
"#000000"
# gray
"#A6A6A6"

# Secondary
# orange
"#F28031"
# green
"#A0B63A"
# dark blue
"#00677F"
# yellow
"#F3D03E"

# Tertiary
# orange
"#ECA154"
"#EFBE7D"
"#EFD19F"
# green
"#C4D600"
"#CEDC00"
"#E2E868"
# blue
"#0092BC"
"#00A9CE"
"#6AD1E3"
# yellow
"#F3D54E"
"#F3DD6D"


#-----------------------------------------------------------------
# image to use
# img_path = "analysis/figures/broom.png"
# img_path = "analysis/figures/PITtag_icon_yellow_bold.png"
img_path = "analysis/figures/PITtag_icon_orange_bold.png"
# img_path = "analysis/figures/PITtag_icon_green.png"

# nm_color = "#F28031"
nm_color = "#F3D03E"

# create a logo sticker and save it
s = sticker(magick::image_read(img_path),
            s_x = 1,
            s_y = 0.7,
            s_width = 1,
            s_height = 0.9,
            package = "PITcleanr",
            p_size = 25,
            p_family = "sans",
            # p_fontface = "bold",
            p_color = nm_color,
            # p_y = 0.6,
            h_fill = "#282D46",
            h_color = nm_color,
            filename = "analysis/figures/PITcleanr.png")

plot(s)
