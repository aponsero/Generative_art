library(dplyr)
library(tidyverse)

#code from https://fronkonstin.com/2019/03/27/drrrawing-with-purrr/

# drawing a pentagon
polygon <- function(n) {
  tibble(
    x    = accumulate(1:(n-1), ~.x+cos(.y*2*pi/n), .init = 0),
    y    = accumulate(1:(n-1), ~.x+sin(.y*2*pi/n), .init = 0),
    xend = accumulate(2:n,     ~.x+cos(.y*2*pi/n), .init = cos(2*pi/n)),
    yend = accumulate(2:n,     ~.x+sin(.y*2*pi/n), .init = sin(2*pi/n)))
}

ggplot(polygon(6))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

ggplot(polygon(7))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

ggplot(polygon(8))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

ggplot(polygon(9))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

# adding connectors
edges <- 5
niter <- 4
polygon(edges) -> df1
accumulate(.f = function(old, y) {
  if (y%%2!=0) mid_points(old) else con_points(old)
},
1:niter,.init=df1) %>% bind_rows() -> df

ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

# changing angle of the connectors
mid_points <- function(d, p, a) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + a,
    x=p*x+(1-p)*xend,
    y=p*y+(1-p)*yend,
    xend=x+0.2*cos(angle),
    yend=y+0.2*sin(angle)) %>% 
    select(x, y, xend, yend)
}
edges <- 7
niter <- 18
polygon(edges) -> df1
accumulate(.f = function(old, y) {
  if (y%%2!=0) mid_points(old, 0.3, pi/5) else con_points(old)
},
1:niter,
.init=df1) %>% 
  bind_rows() -> df
ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

# increase angle
mid_points <- function(d, p, a, i, FUN = function(x) x) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + a,
    radius=FUN(i),
    x=p*x+(1-p)*xend,
    y=p*y+(1-p)*yend,
    xend=x+radius*cos(angle),
    yend=y+radius*sin(angle)) %>% 
    select(x, y, xend, yend)
}

edges <- 7
niter <- 18
polygon(edges) -> df1
accumulate(.f = function(old, y) {
  if (y%%2!=0) mid_points(old, 0.3, pi/5, y) else con_points(old)
},
1:niter,
.init=df1) %>% 
  bind_rows() -> df
ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

#more iterations on the last one
edges <- 7
niter <- 250
step <- 2
polygon(edges) -> df1
accumulate(.f = function(old, y) {
  if (y%%step!=0) mid_points(old, 0.3, pi/5, y) else con_points(old)
},
1:niter,
.init=df1) %>% 
  bind_rows() -> df
ggplot(df)+
  geom_curve(aes(x=x, y=y, xend=xend, yend=yend),
             curvature = 0,
             color="black",
             alpha=0.1)+
  coord_equal()+
  theme(legend.position  = "none",
        panel.background = element_rect(fill="white"),
        plot.background  = element_rect(fill="white"),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())

###################COMPLETE CODE##################

# This function creates the segments of the original polygon
polygon <- function(n) {
  tibble(
    x    = accumulate(1:(n-1), ~.x+cos(.y*2*pi/n), .init = 0),
    y    = accumulate(1:(n-1), ~.x+sin(.y*2*pi/n), .init = 0),
    xend = accumulate(2:n,     ~.x+cos(.y*2*pi/n), .init = cos(2*pi/n)),
    yend = accumulate(2:n,     ~.x+sin(.y*2*pi/n), .init = sin(2*pi/n)))
}

# This function creates segments from some mid-point of the edges
mid_points <- function(d, p, a, i, FUN = ratio_f) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + a,
    radius=FUN(i),
    x=p*x+(1-p)*xend,
    y=p*y+(1-p)*yend,
    xend=x+radius*cos(angle),
    yend=y+radius*sin(angle)) %>% 
    select(x, y, xend, yend)
}

# This function connect the ending points of mid-segments
con_points <- function(d) {
  d %>% mutate(
    x=xend,
    y=yend,
    xend=lead(x, default=first(x)),
    yend=lead(y, default=first(y))) %>% 
    select(x, y, xend, yend)
}

edges <- 6   # Number of edges of the original polygon
niter <- 500 # Number of iterations
pond <- 0.1  # Weight to calculate the point on the middle of each edge
step  <- 20  # No of times to draw mid-segments before connect ending points
alph  <- 0.7 # transparency of curves in geom_curve
angle <- 0.4 # angle of mid-segment with the edge
curv <- 0   # Curvature of curves
line_color <- "deeppink" # Color of curves in geom_curve
back_color <- "mediumpurple4" # Background of the ggplot
ratio_f <- function(x) {1/x} # To calculate the longitude of mid-segments

# Generation on the fly of the dataset
accumulate(.f = function(old, y) {
  if (y%%step!=0) mid_points(old, pond, angle, y) else con_points(old)
}, 1:niter,
.init=polygon(edges)) %>% bind_rows() -> df

# Plot
ggplot(df)+
  geom_curve(aes(x=x, y=y, xend=xend, yend=yend),
             curvature = curv,
             color=line_color,
             alpha=alph,
             size=0.4)+
  coord_equal()+
  theme(legend.position  = "none",
        panel.background = element_rect(fill=back_color),
        plot.background  = element_rect(fill=back_color),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())


out_file="plot_purple2.png"
ggsave(out_file, width = 10, height = 10, units = "cm")
