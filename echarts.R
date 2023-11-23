library(tidyr)
library(echarts4r)
# library(echarts4r.assets)
library(dplyr)
library(pandoc)
library(rjson)
library(echarty)

N<-300
df <- data.frame(x = runif(N,1,20),
                 y = runif(N,10,25),
                 z = rnorm(N,100,50),
                 Time = runif(N,5,500),
                 label = sample(c("interaction1", "interaction2", "interaction3", "interaction4", "interaction5"), N, replace = TRUE),
                 zone = sample(c("zone0", "zone1", "zone3"), N, replace = TRUE))

df_toadd<-data.frame(x = runif(N,80,100),
                     y = runif(N,10,25),
                     z = rnorm(N,100,50),
                     Time = runif(N,5,500),
                     label = sample(c("interaction1", "interaction2", "interaction3", "interaction4", "interaction5"), N, replace = TRUE),
                     zone = sample(c("zone0", "zone1", "zone3"), N, replace = TRUE))
df<-rbind(df,df_toadd)
df$mylabel<-as.numeric(substr(df$label,12,12))
my_scale <- function(x) scales::rescale(x, to = c(min(df$Time),max(df$Time)))


df|>group_by(label)|>e_charts(x)|>
  e_scatter_3d(y,z,Time)|>
  e_visual_map(Time,inRange = list(symbol = "diamond",symbolSize = c(25,5)),scale = my_scale)|>
  e_tooltip()|>
  e_theme("westeros")|>
  e_legend(show = TRUE)


##Timeline


df|>group_by(zone)|>e_charts(x,timeline = TRUE)|>
  e_scatter_3d(y,z,Time,mylabel,label)|>
  e_visual_map(Time,inRange = list(symbol = "diamond",symbolSize = c(35,5)),scale = my_scale,dimension = 3)|>
  e_visual_map(mylabel,inRange = list(colorLightness = c(0.5,0.8), colorHue = c(180,260),colorSaturation = c(120,200)),dimension = 4,bottom = 300)|>
  e_tooltip()|>
  e_theme("westeros")|>
  e_legend(show = TRUE)







df|> group_by(zone)|>ec.init(
  tl.series = list(
    encode = list(x = 'x',y = 'y',symbolSize = ec.clmn(2,scale = 30)),
    groupBy = 'label'
  )
)|>ec.theme("dark-mushroom")








dat <- data.frame(
  x3 = runif(16),
  x4 = runif(16),
  x5 = abs(runif(16)),
  x1 = rep(2020:2023, each = 4),
  x2 = rep(c("A", "A", "B", "B"), 4)
) 


p <- dat |> group_by(x1) |> ec.init(
  tl.series= list(encode= list(x= 'x3', y= 'x5'), 
                  symbolSize= ec.clmn(2, scale=30), 
                  groupBy= 'x2') 
)




p



add_icon <- function(symbols){
  sapply(symbols, ea_icons)
}

df<-df %>% mutate(
  symbol_str = sample(c("bookmark","user"),n(),replace = TRUE),
  symbol = add_icon(symbol_str)
)



df |> group_by(label,symbol) |> e_charts(x) |> 
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
  e_theme("vintage")

# ea_icons_search("")

fig






htmlwidgets::saveWidget(fig, paste("animation","0",".html",sep = "_"), selfcontained = TRUE)





df|> e_charts(x) |>
  e_scatter_3d(y,z,Time,label,smooth  = FALSE,legend = TRUE, name="Sample 3D Plot")|>
  e_visual_map(Time,type = "continuous",inRange = list(symbol = "diamond",symbolSize = c(45,8)),scale = my_scale) |>
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















library(tidyr)
library(echarts4r)
library(echarts4r.assets)
library(dplyr)
# setwd("E:/Outbreak_CIC_Model/")
# extra<-read.csv("extra.csv")
setwd("E:/Outbreak_CIC_Model")
S_<-read.csv("full.csv")
# Rearranging the data
S_rearranged <- S_  %>%
  group_by(interaction,time,zone) %>%
  summarise(count = n()) %>%
  ungroup() %>% 
  mutate(dates = time) %>%
  select( -time) %>%
  rename(groups = interaction, values = count)
data<-S_rearranged
# # Creating the river plot
# data<-subset(S_rearranged, groups != "marker" && groups != "Host 0[*]")
# 
# data|>group_by(groups)|>
#   e_charts(dates) |>
#   e_line(values, stack = "grp2") |>
#   e_tooltip() |>
#   e_theme("vintage")|>
#   e_datazoom()
# 



# Convert 'dates' column to numeric (if it's not already numeric)
data$dates <- as.numeric(data$dates)

# Create a template dataframe with all unique combinations of groups and dates
all_combinations <- expand.grid(
  groups = unique(data$groups),
  dates = unique(data$dates)
)

# Merge the template with the existing data
filled_data <- merge(all_combinations, data, by = c("groups", "dates"), all = TRUE)

# Replace missing values with 0
filled_data[is.na(filled_data$values), "values"] <- 0

# e_theme(
#   e,
#   name = c("auritus", "azul", "bee-inspired", "blue", "caravan", "carp", "chalk", "cool",
#            "dark-blue", "dark-bold", "dark-digerati", "dark-fresh-cut", "dark-mushroom", "dark",
#            "eduardo", "essos", "forest", "fresh-cut", "fruit", "gray", "green", "halloween",
#            "helianthus", "infographic", "inspired", "jazz", "london", "macarons", "macarons2",
#            "mint", "purple-passion", "red-velvet", "red", "roma", "royal", "sakura", "shine",
#            "tech-blue", "vintage", "walden", "wef", "weforum", "westeros", "wonderland")
# )



# Sort the combined data by 'groups' and 'dates'
filled_data$zone<-replace_na(filled_data$zone,-1)
filled_data <- filled_data[order(filled_data$groups, filled_data$dates), ]
filled_data|>group_by(groups)|>
  e_charts(dates) |>
  e_area(values,zone,
         emphasis = list(
           focus = "self"
         )) |> 
  e_y_axis(min = 0)|>
  e_tooltip()  |>
  e_theme("westeros")|>
  e_datazoom(
    type = "slider",
    toolbox = TRUE,
    bottom = 10
  )|>
  e_legend(right = 5,top = 80,selector = "inverse",show=TRUE,icon = 'circle',emphasis = list(selectorLabel = list(offset = list(10,0))), align = 'right',type = "scroll",width = 10,orient = "vertical")|>
  e_legend_unselect("marker")|>
  e_legend_unselect("Host 0[*]")|>
  e_title(paste("Infection Occurrences over Time by Type"), "CIC Model | by Irshad Ul Ala")


subset(data,zone == 2)|>e_charts(dates)|>
  e_river(values)











#Stream

filled_data|>group_by(groups)|>
  e_charts(dates) |>
  e_river(values) |> 
  e_tooltip()  |>
  e_theme("westeros")|>
  e_datazoom(
    type = "slider",
    toolbox = TRUE,
    bottom = 10
  )|>
  e_legend(right = 5,top = 80,selector = "inverse",show=TRUE,icon = 'circle',emphasis = list(selectorLabel = list(offset = list(10,0))), align = 'right',type = "scroll",width = 10,orient = "vertical")|>
  e_legend_unselect("marker")|>
  e_legend_unselect("Host 0[*]")|>
  e_title(paste("Infection Occurrences over Time by Type"), "CIC Model | by Irshad Ul Ala")





#Timeline

filled_data|>group_by(zone)|>
  e_charts(dates,timeline = TRUE) |>
  e_area(values,
         emphasis = list(
           focus = "self"
         )) |> 
  e_y_axis(min = 0)|>
  e_tooltip()  |>
  e_theme("westeros")|>
  e_legend(right = 5,top = 80,selector = "inverse",show=TRUE,icon = 'circle',emphasis = list(selectorLabel = list(offset = list(10,0))), align = 'right',type = "scroll",width = 10,orient = "vertical")|>
  e_legend_unselect("marker")|>
  e_legend_unselect("Host 0[*]")|>
  e_title(paste("Infection Occurrences over Time by Type"), "CIC Model | by Irshad Ul Ala")






summed_data <- filled_data %>%
  group_by(groups, dates) %>%
  summarise(total_values = sum(values)) %>%
  ungroup() %>%
  group_by(groups) %>%
  summarise(values = sum(total_values))


subset(summed_data,groups!="marker")|> e_charts(groups)|>e_pie(values, roseType = "radius")|>
  e_legend(show = FALSE)


##COMBINE
filled_data|>group_by(groups,zone)|>
  e_charts(dates) |>
  e_area(values,zone,
         emphasis = list(
           focus = "self"
         ),name = "InfectionTime") |> 
  e_data(summed_data,groups)|>
  e_pie(values, roseType = "radius",name = "InfectionTotal")|>
  e_grid(right = 40, top = 100, width = "30%") |>
  e_tooltip()  |>
  e_theme("westeros")|>
  e_title("Infection Occurrences over Time by Type", "CIC Model | by Irshad Ul Ala")











quakes |> 
  e_charts(long) |> 
  e_geo(
    boundingCoords = list(
      c(190, -10),
      c(180, -40)
    )
  ) |> 
  e_scatter(lat, mag, stations, coord_system = "geo", name = "mag", rm_y = FALSE, rm_x = FALSE) |> # do not remove axis
  e_data(quakes, depth) |> # use e_data to add data and/or change value on x axis
  e_scatter(depth, mag, stations,  name = "mag & depth") |>  # plot scatter
  e_grid(right = 40, top = 100, width = "30%") |> # adjust grid to avoid overlap
  e_y_axis(name = "depth", min = 3.5) |> # add y axis name
  e_x_axis(name = "magnitude") |> # add x axis name
  e_legend(FALSE) |>  # hide legend
  e_title("Built-in crosstalk", "Use the brush") |> # title
  e_theme("westeros") |> # add a theme
  e_brush() |> # add the brush
  e_tooltip() # Add tooltips


