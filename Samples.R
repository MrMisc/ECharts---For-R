

library(echarts4r)
library(readxl)
require(devtools)
library(data.table)
library(purrr)
library(geonames)
library(stringr)
library(plotly)

# Gamma<-data.frame(y = rgamma(1000,4.5,rate = 0.5))
# 
# 
# Gamma |>
#   e_charts() |>
#   e_histogram(y, name = "histogram") |>
#   e_density(y, areaStyle = list(opacity = .4), smooth = TRUE, name = "density", y_index = 1) |>
#   e_tooltip(trigger = "axis") |>
#   e_x_axis(name = "Days",nameLocation = 'center',nameGap = 20)|>
#   e_theme("infographic")|>
#   e_title("Colonization time in days", "Adjusted time to colonize")
# 
# min(Gamma$y)



###  Movement 1
#Brownian motion
# Initialize empty lists for x, y, z coordinates
x_coords <- c()
y_coords <- c()
z_coords <- c()
type<-c()

# Define the spacing
spacing <- 300
max_val <- 600

# Generate the points
for (x in seq(0, max_val, spacing)) {
  for (y in seq(0, max_val, spacing)) {
    for (z in seq(0, max_val, spacing)) {
      x_coords <- c(x_coords, x)
      y_coords <- c(y_coords, y)
      z_coords <- c(z_coords, z)
      type<-c(type,rnorm(1,50,10))
    }
  }
}

len<-length(x_coords)
# Create a data frame with x, y, z coordinates
brown <- data.frame(x = x_coords, y = y_coords, z = z_coords,time = rep(1,length(x_coords)), type = type)
# brown<-data.frame(x = x,y=y,z=z,time = time)

for (i in 0:40){
  addto<-subset(brown,time == i+1)
  print(nrow(addto))
  toadd<-data.frame(x = addto$x+rnorm(len,0,20),y = addto$y+rnorm(len,0,20),z = addto$z+rnorm(len,0,20),time = rep(i+2,len), type = addto$type)
  print(rnorm(len,0,2))
  brown<-rbind(brown,toadd)
}

library(echarts4r)

my_scale <- function(x) scales::rescale(x, to = c(-10,80))

brown = subset(brown, time !=1)
d1<-brown|>group_by(time)|>e_charts(x,timeline = TRUE)|>
  e_scatter_3d(y,z, time, type)|>
  # e_visual_map(type,inRange = list(symbolSize = c(40,20)),scale = my_scale,dimension = 4)|>
  # e_visual_map(type,inRange = list(symbol = "diamond",colorLightness = c(0.6,0.9), colorHue = c(0,50),colorSaturation = c(120,200), dimension = 3))|>
  e_visual_map(type,inRange = list(colorLightness = c(0.5,0.8),symbolSize = c(100,10), colorHue = c(100,260),colorSaturation = c(80,200)),dimension = 4,bottom = 300, scale = my_scale, show=FALSE)|>
  e_tooltip()|>
  e_theme("vintage")|>
  # e_x_axis_3d(min = -50,max = 200,interval = 10)|>
  # e_y_axis_3d(min = 0,max = 200,interval = 10)|>
  # e_z_axis_3d(min = 0,max = 200,interval = 10)|>
  e_grid_3d(boxWidth = 100,boxHeight = 100,boxDepth = 100)|>
  e_timeline_opts(autoPlay = TRUE, show = FALSE)|>
  e_animation(duration = 11000, delay = 0, duration.update = 10000,easing.update = "sinusoidalInOut", delay.update = 0.0)|>
  e_legend(show = TRUE)





###  Movement 2
#Brownian motion
# Initialize empty lists for x, y, z coordinates
x_coords <- c()
y_coords <- c()
z_coords <- c()
type<-c()

# Define the spacing
spacing <- 600
max_val <- 600

# Generate the points
for (x in seq(0, max_val, spacing)) {
  for (y in seq(0, max_val, spacing)) {
    for (z in seq(0, max_val, spacing)) {
      x_coords <- c(x_coords, x)
      y_coords <- c(y_coords, y)
      z_coords <- c(z_coords, z)
      type<-c(type,rnorm(1,50,10))
    }
  }
}

len<-length(x_coords)
# Create a data frame with x, y, z coordinates
brown2 <- data.frame(x = x_coords, y = y_coords, z = z_coords,time = rep(1,length(x_coords)), type = type)
# brown<-data.frame(x = x,y=y,z=z,time = time)

for (i in 0:40){
  addto<-subset(brown2,time == i+1)
  print(nrow(addto))
  toadd<-data.frame(x = addto$x+rnorm(len,0,0.5),y = addto$y+rnorm(len,0,0.5),z = addto$z+rnorm(len,0,0.5),time = rep(i+2,len), type = addto$type)
  print(rnorm(len,0,2))
  brown2<-rbind(brown2,toadd)
}

my_scale <- function(x) scales::rescale(x, to = c(-10,80))

brown2 = subset(brown2, time !=1)
d2<-brown2|>group_by(time)|>e_charts(x,timeline = TRUE)|>
  e_scatter_3d(y,z, time, type)|>
  # e_visual_map(type,inRange = list(symbolSize = c(40,20)),scale = my_scale,dimension = 4)|>
  # e_visual_map(type,inRange = list(symbol = "diamond",colorLightness = c(0.6,0.9), colorHue = c(0,50),colorSaturation = c(120,200), dimension = 3))|>
  e_visual_map(type,inRange = list(colorLightness = c(0.5,0.8),symbolSize = c(100,10), colorHue = c(10,170),colorSaturation = c(80,200)),dimension = 4,bottom = 300, scale = my_scale, show=FALSE)|>
  e_tooltip()|>
  e_theme("vintage")|>
  # e_x_axis_3d(min = -50,max = 200,interval = 10)|>
  # e_y_axis_3d(min = 0,max = 200,interval = 10)|>
  # e_z_axis_3d(min = 0,max = 200,interval = 10)|>
  e_grid_3d(boxWidth = 100,boxHeight = 100,boxDepth = 100)|>
  e_timeline_opts(autoPlay = TRUE, show = TRUE)|>
  e_animation(duration = 11000, delay = 0, duration.update = 10000,easing.update = "sinusoidalInOut", delay.update = 0.0)|>
  e_legend(show = TRUE)

d2





cb <- "() => {
  let x = 0;
  setInterval(() => {
    x++
    chart.setOption(opts[x % 2], true);
  }, 3000);
}"

e_morph(d1, d2, callback = cb)







#Improved movement




# Brownian motion data ------------------------
x_coords <- y_coords <- z_coords <- type <- c()
# Define the spacing
spacing <- 50
max_val <- 400
# Generate the points
for (x in seq(0, max_val, spacing)) {
  for (y in seq(0, max_val, spacing)) {
    for (z in seq(0, max_val, spacing)) {
      x_coords <- c(x_coords, x)
      y_coords <- c(y_coords, y)
      z_coords <- c(z_coords, z)
      type<-c(type,rnorm(1,50,10))
    }
  }
}
len<-length(x_coords)
# Create a data frame with x, y, z coordinates
brown <- data.frame(x= x_coords, y= y_coords, z= z_coords, time= rep(1,length(x_coords)), type= type)

for (i in 0:19){
  addto <- subset(brown, time==i+1)
  #print(nrow(addto))
  toadd<-data.frame(x = addto$x+rnorm(len,0,20),y = addto$y+rnorm(len,0,20),z = addto$z+rnorm(len,0,20),time = rep(i+2,len), type = addto$type)
  brown<-rbind(brown,toadd)
}
brown = subset(brown, time !=1) |> dplyr::mutate(tscaled= scales::rescale(type, to = c(-10,80))) 
cax <- list(max= 'dataMax', min= 'dataMin')
#  cax above is made with a ECharts "shortcut", but I think this works better:
lax <- \(aa) { list(max= round(max(brown[aa])), min= round(min(brown[aa]))) }

library(echarty)
brown |> group_by(time) |>
  ec.init(load='3D', 
          tooltip= list(show=T), options= list(title=NULL), # remove preset title
          # xAxis3D= cax, yAxis3D= cax, zAxis3D= cax,
          xAxis3D= lax('x'), yAxis3D= lax('y'), zAxis3D= lax('z'),
          timeline= list(autoPlay=TRUE, playInterval=555, orient='vertical', 
                         left=NULL, right=0, top=20, bottom=20, width=55, height=NULL),
          tl.series= list(type='scatter3D', animationDurationUpdate=500, animationEasingUpdate="sinusoidalIn"),
          visualMap = list(dimension=6, # 6==tscaled 
                           inRange = list(colorLightness = c(0.5,0.8), colorHue = c(100,260),colorSaturation = c(120,200)),
                           bottom = 300, show=FALSE),
  )|>ec.theme("thing",code = jsonfile)




















#




# Create a data frame with a single point in the middle
data <- data.frame(
  x = 5,
  y = 5,
  z = 5,
  time = 1,
  type = 50 # You can adjust the type value as needed
)

my_scale <- function(x) scales::rescale(x, to = c(-10, 80))

e1<-data |>
  e_charts(x) |>
  e_scatter_3d(y,z, time, type,    universalTransition = TRUE,
               animationDurationUpdate = 1000L) |>
  e_visual_map(
    type,
    inRange = list(
      colorLightness = c(0.5, 0.8),
      symbolSize = 0.1,
      colorSaturation = c(80, 200)
    ),
    dimension = 4,
    bottom = 300,
    show = FALSE
  ) |>
  e_tooltip() |>
  e_theme("vintage") |>
  e_x_axis_3d(min = 0, max = 1000, interval = 25) |>
  e_y_axis_3d(min = 0, max = 100, interval = 25) |>
  e_z_axis_3d(min = 0, max = 100, interval = 4) |>
  e_grid_3d(boxWidth = 1500, boxHeight = 150, boxDepth = 150) |>
  e_legend(show = TRUE)

e1







# Create a data frame with a single point in the middle
data <- data.frame(
  x = runif(5, 0, 10),
  y = runif(5, 0, 10),
  z = runif(5, 0, 10),
  time = rep(1, 5),
  type = rep(100, 5)
)
my_scale <- function(x) scales::rescale(x, to = c(-10, 80))




e2<-data |>
  e_charts(x) |>
  e_scatter_3d(y,z, time, type,universalTransition = TRUE,animationDurationUpdate = 1000L) |>
  e_visual_map(
    type,
    inRange = list(
      colorLightness = c(0.5, 0.8),
      symbolSize = 30,
      colorSaturation = c(80, 200)
    ),
    dimension = 4,
    bottom = 300,
    show = FALSE
  ) |>
  e_tooltip() |>
  e_theme("vintage") |>
  e_x_axis_3d(min = 0, max = 10, interval = 10) |>
  e_y_axis_3d(min = 0, max = 10, interval = 10) |>
  e_z_axis_3d(min = 0, max = 10, interval = 10) |>
  e_grid_3d(boxWidth = 100, boxHeight = 100, boxDepth = 100) |>
  e_legend(show = TRUE)




cb <- "() => {
  let x = 0;
  setInterval(() => {
    x++
    chart.setOption(opts[x % 2], true);
  }, 3000);
}"

e_morph(e1, e2, callback = cb)














plot_ly(brown, x = ~x, y = ~y, z = ~z, mode = "markers", type = "scatter3d", frame = ~time,text = ~paste(
  '<br>Time:', time, 'hours ','<br> Infection Event Type:',type),
  marker = list(
    color = "#15798C",
    size = factor,
    opacity = 0.9,
    colorscale = 'Inferno'
  ))%>%
  animation_opts(frame = 1000,transition = 300,mode = "afterall",
                 easing = "linear-in", redraw = TRUE
  )






#Liquid

df <- data.frame(val = c(0.9, 0.5, 0.4))

df |>
  e_charts() |>
  e_liquid(val) |>
  e_theme("dark")

