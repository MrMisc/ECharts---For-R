library(echarts4r)
library(echarts4r.assets)
library(dplyr)
library(pandoc)

df <- data.frame(x = runif(100,1,20),
                 y = runif(100,10,25),
                 z = rnorm(100,100,50),
                 Time = runif(100,5,500),
                 label = factor(rep("Host<->Host",100)))

df_toadd<-data.frame(x = runif(100,80,100),
                     y = runif(100,10,25),
                     z = rnorm(100,100,50),
                     Time = runif(100,5,500),
                     label = factor(rep("Egg->Host",100)))
df<-rbind(df,df_toadd)

add_icon <- function(symbols){
  sapply(symbols, ea_icons)
}

df<-df %>% mutate(
  symbol_str = sample(c("bookmark","user"),n(),replace = TRUE),
  symbol = add_icon(symbol_str)
)




fig<-df |> group_by(label,symbol) |> e_charts(x) |> 
  e_scatter_3d(y,z,Time,label,symbol_str,scale = e_scale)|>
  e_tooltip() |>
  e_visual_map(Time,inRange = list(symbolSize = c(5, 30)),dimension = 3,bottom = 300) |>
  e_add_unnested("symbol",symbol) |>
  e_x_axis_3d(min = 0,max = 200,interval = 100)|>
  e_y_axis_3d(min = 0,max = 200,interval = 25)|>
  e_z_axis_3d(min = 0,max = 300)|>
  e_grid_3d(boxWidth = 1000,boxHeight = 1000,boxDepth = 20)|>
  e_legend(show = TRUE, type = "scroll") |>
  e_title("Infection Plot", "CIC Model | by Irshad Ul Ala")|>
  e_theme_custom("MyEChartsTheme.json")

# ea_icons_search("")

fig

htmlwidgets::saveWidget(fig, paste("animation","0",".html",sep = "_"), selfcontained = TRUE)





df|> e_charts(x) |>
  e_scatter_3d(y,z,color = label,smooth  = FALSE,legend = TRUE, name="Sample 3D Plot")|>
  e_visual_map() |>
  e_grid(index = c(0, 1)) |>
  e_tooltip()


library(dplyr)
library(echarts4r)
# install.packages("devtools")
# remotes::install_github("JohnCoene/echarts4r.assets")
library(echarts4r.assets)
# serialise ea_icons
add_icon <- function(symbols){
  sapply(symbols, ea_icons)
}
library(dplyr)
mtcars %>% 
  mutate(
    symbol = sample(c("trash", "user"), n(), replace = TRUE), # random icon
    symbol = add_icon(symbol)
  ) %>% 
  e_charts(mpg) %>% 
  e_scatter(
    wt, 
    qsec
  ) %>%  
  e_add_unnested("symbol", symbol) %>%
  e_legend(icons = ea_icons("trash"))|>
  e_theme("chalk")






quakes |>
  e_charts(long) |>
  e_geo(
    roam = TRUE,
    boundingCoords = list(
      c(185, -10),
      c(125, -60)
    )
  ) |>
  e_scatter(lat, mag, coord_system = "geo") |>
  e_visual_map(min = 2.5, max = 8.5)|>
  e_tooltip()









v <- LETTERS[1:10]
matrix <- data.frame(
  x = sample(v, 300, replace = TRUE), 
  y = sample(v, 300, replace = TRUE), 
  z = rnorm(300, 10, 1),
  color = rnorm(300, 10, 1),
  size = rnorm(300, 10, 1),
  stringsAsFactors = FALSE
) %>% 
  dplyr::group_by(x, y) %>% 
  dplyr::summarise(
    z = sum(z),
    color = sum(color),
    size = sum(size)
  ) %>% 
  dplyr::ungroup() 

matrix %>% 
  e_charts(x) %>% 
  e_scatter_3d(y, z, color, size) %>% 
  e_visual_map(
    z, # scale to z
    inRange = list(symbolSize = c(1, 30)), # scale size
    dimension = 3 # third dimension 0 = x, y = 1, z = 2, size = 3
  ) %>% 
  e_visual_map(
    z, # scale to z
    inRange = list(color = c('#bf444c', '#d88273', '#f6efa6')), # scale colors
    dimension = 4, # third dimension 0 = x, y = 1, z = 2, size = 3, color = 4
    bottom = 300 # padding to avoid visual maps overlap
  ) %>% e_tooltip()


path <- paste0(
  "path://M11.344,5.71c0-0.73,0.074-1.122,1.199-1.122",
  "h1.502V1.871h-2.404c-2.886,0-3.903,1.36-3.903,3.646",
  "v1.765h-1.8V10h1.8v8.128h3.601V10h2.403l0.32-2.718h",
  "-2.724L11.344,5.71z"
)
e <- cars |>
  e_charts(speed) |>
  e_scatter(dist, symbol_size = 5)
e |>
  e_legend(
    icons = list(path)
  )|>
  e_datazoom(x_index = 0, type = "slider") |> 
  e_datazoom(y_index = 0, type = "slider") |>
  e_tooltip()
