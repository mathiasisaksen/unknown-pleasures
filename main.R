# Unknown Pleasure album art animation
# Made by: Mathias Isaksen

library(ggplot2) # For visualizations
library(cowplot) # For theme_nothing (removes all elements from a ggplot, except for the plot panel)
library(pals) # For color palettes
library(plotwidgets) # For darkenCol, which darkens/brightens a color

# We use the square [0, 1]^2 as our workspace/extent
ext = c(0, 1, 0, 1)

# In some versions of the album art, the curves are centered and take up roughly 43% and 53% of the width and height, respectively.
# The variables below specify the extent of the region containing the curves.
left.edge = (1-0.43)/2
right.edge = 1 - (1-0.43)/2
top.edge = 1 - (1-0.53)/2
bottom.edge = (1-0.53)/2

# The art work has 80 lines, we use the same.
num.lines = 80

# The palette gnuplot from pals is used for the fill under the curves.
# We remove the last 10 (very bright) colors and darken the palette slightly.
palette = rev(gnuplot(num.lines + 10, 0.05)[1:num.lines])
palette = darkenCol(palette, 0.1)
# Looks nice!
pal.bands(palette)

# The bump function is used to make the curves small outside a certain region.
# The parameter width controls the width of this region, while s controls how quickly the bump function
# vanishes to 0. 
bump.function = function(x, width = 0.365, s = 0.025) {
  left.limit = left.edge + width * (right.edge - left.edge)
  right.limit = right.edge - width * (right.edge - left.edge)
  if (x > left.limit & x < right.limit) {
    return(1)
  } else if (x <= left.limit) {
    return(exp(-1/s^2*(x - left.limit)^2))
  } else if (x >= right.limit) {
    return(exp(-1/s^2*(x - right.limit)^2))
  }
}

# This function is used for generating the gradual change in fill that is seen in the
# /r/generative post. i is the frame number, see code further down for setup
get.palette = function(i) {
  if (i >= 1 & i < 43) {
    # For frames 1 to 42, use a black fill
    frame.palette = rep("black", 80)
  } else if (i >= 43 & i <= 50) {
    # Go from black fill to gnuplot gradually
    frame.palette = darkenCol(palette, by = (51-i)/9)
  } else if (i >= 193 & i <= 200) {
    # Go from gnuplot to black gradually
    frame.palette = darkenCol(palette, by = (i-192)/9)
  } else {
    # The portion of the animation where the fill does not change
    frame.palette = palette
  }
  return(frame.palette)
}

# This function computes the color of the curves, which gradually go from white
# to the same color as the color used for the fill. As a result, the curves seemingly disappear.
get.line.color = function(i) {
  colors = palette
  if (i >= 93 & i <= 100) {
    brightening.factor = (101 - i)/9
  } else if (i >= 143 & i <= 150) {
    brightening.factor = (i - 142)/9
  } else if (i > 100 & i < 150) {
    brightening.factor = 0
  } else {
    colors = rep("white", num.lines)
    return(darkenCol(colors, 1-249/255))
  }
  brightening.factor = brightening.factor*249/255 # Small correction, not important
  colors = darkenCol(colors, -brightening.factor)
  return(colors)
}

# The baseline vertical position of each curve
y.coords = seq(top.edge, bottom.edge, length.out = num.lines)
set.seed(0)
# The sine curves will be on the form amplitude*sin(rate*x),
# where both the amplitude and rate are sampled from an appropriate distribution
rates = rnorm(num.lines, mean = 20, sd = 4)
amplitudes = rnorm(num.lines, mean = 0.02, sd = 0.005)

# The x-coordinate values used for drawing the curves
# The curve is drawn over the entire width of the extent, and later covered by a rectangles
x = seq(ext[1], ext[2], length.out = 100)
bump = sapply(x, bump.function)
# The bump function looks nice:
plot(x, bump, type = "l")

# The noise that is added to each curve later
noise.size = 0.03
noise = matrix(rnorm(length(x)*length(y.coords), sd = noise.size), nrow = num.lines)
# The width and height of the output image in inches
size = 7.5
# Resolution, results in a 3000 x 3000 image
dpi = 3000/size
# Number of frames to be made
num.frames = 200
# The time step between each frame, chosen to give periodic behaviour
dt = 2*pi/num.frames
for (frame in 1:num.frames) {
  t = frame*dt
  print(c(frame, t))
  # We want the curves that are further up to be obscured by those below.
  # This can be done by creating a polygon consisting of the curve and a
  # a rectangular portion that extends below it.
  # polygon.df will contain the necessary data for each curve, and the column group is
  # used to differentiate between different curves.
  polygon.df = data.frame()
  for (i in 1:num.lines) {
    # The sine is squared to ensure that it is positive
    curve = y.coords[i] + (0.1+bump)*amplitudes[i]*(sin(rates[i]*x+t)^2 + noise[i, ])
    temp.df = data.frame(x = c(x, ext[2], ext[1]), y = c(curve, ext[3]-1, ext[3]-1), group = i)
    polygon.df = rbind(polygon.df, temp.df)
  }
  frame.fill.palette = get.palette(frame)
  frame.curve.color = get.line.color(frame)
  wave.plot = ggplot()+
    # Draw polygons containing curves
    geom_polygon(data = polygon.df, aes(x = x, y = y, group = group, fill = factor(group), color = factor(group)))+
    # Add rectangles to obscure the non-central portion
    geom_rect(aes(xmin = ext[1], xmax = left.edge, ymin = ext[3], ymax = ext[4]), color = "black", fill = "black")+
    geom_rect(aes(xmin = right.edge, xmax = ext[2], ymin = ext[3], ymax = ext[4]), color = "black", fill = "black")+
    # Without the next rectangle, a thin white line occurs in the bottom of the plot. Temporary fix
    geom_rect(aes(xmin = ext[1], xmax = ext[2], ymin = -1, ymax = 0.2), color = "black", fill = "black")+
    scale_fill_manual(values = frame.fill.palette)+
    scale_color_manual(values = frame.curve.color)+
    # Limit plot region to where the curves are. Change this if you want the whole extent
    coord_fixed(xlim = c(0.2, 0.8), ylim = c(0.2, 0.8), expand = 0)+
    theme_nothing()+
    # Make background black
    theme(panel.background = element_rect(fill = "black", color = NA))
  
  ggsave(sprintf("%i.png", frame), wave.plot, width = size, height = size, dpi = dpi)
}
