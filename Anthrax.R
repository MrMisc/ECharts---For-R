


#Anthrax
library(echarts4r)
library(readxl)
require(devtools)
#install_github("ropensci/geonames")
setwd("E:/")
anth<-read_excel("Anthrax.xlsx")

library(data.table)
library(purrr)
library(geonames)
library(stringr)
options(geonamesUsername = "LGFUAD")

#Manually simplify some names 

# anth$Country <- str_replace(anth$Country, unique(anth$Country)[30], "China")
library(dplyr)
anth <- anth %>%
  mutate(Country = ifelse(Country == "China (People's Rep. of)", "China", Country)) %>% 
  mutate(Country = ifelse(Country == "South Sudan (Rep. of)", "South Sudan", Country)) %>% 
  mutate(Country = ifelse(Country == "Congo (Dem. Rep. of the)", "Congo", Country)) %>% 
  mutate(Country = ifelse(Country == "Korea (Rep. of)", "Korea", Country)) 
unique(anth$Country)

places <- unique(anth$Country)
get_coords <- function(q) {
  q<-
    res <- GNsearch(q=q)  
  out <- data.frame(Country = q,name = res$toponymName, lat = res$lat, lon = res$lng)
  return(out[1,])  
}

GNresult <- places %>% 
  map(get_coords) %>% 
  rbindlist()
print(GNresult)






finalOut<-data.frame(Country = GNresult$Country.name,database_name = GNresult$name,lat = GNresult$lat,lon = GNresult$lon)
write.csv(finalOut, "linkednamestolatlong.csv",row.names = FALSE)
anth<-merge(anth,finalOut,by = "Country",all.x = TRUE)
write.csv(anth, "Anthrax_Merged.csv",row.names = FALSE)

data<-anth %>% group_by(Year,Country) %>% 
  summarise(n = n()) %>% 
  ungroup()

data<-merge(data,finalOut,by = "Country",all.x = TRUE)

library(echarts4r)
my_scale <- function(x) scales::rescale(x, to = c(min(data$n),max(data$n)))



#remotes::install_github('JohnCoene/echarts4r.assets')
library(echarts4r.assets)
###GLOBE

globe_year<-data |> group_by(Year)|>
  e_charts(lon,timeline = TRUE) |> 
  e_globe(
    environment = ea_asset("starfield"),
    base_texture = ea_asset("world topo"), 
    height_texture = ea_asset("world topo"),
    displacementScale = 0.04
  ) |> 
  e_bar_3d(lat, n,Country, coord_system = "globe") |> 
  e_tooltip()|>
  e_visual_map(n,inRange = list(opacity = c(0.6,1)),show = FALSE, dimension = 2)|>
  e_theme("infographic")|>
  e_animation(easing = 'quarticIn',easing.update = 'quarticIn')|>
  e_title("ALL Anthrax Reports by Year", "WAHIS Public Quantitative data from WHO | SFA")



##By semester instead of year


data<-anth %>% group_by(Semester,Country) %>% 
  summarise(n = n()) %>% 
  ungroup()

data<-merge(data,finalOut,by = "Country",all.x = TRUE)


data |> group_by(Semester)|>
  e_charts(lon,timeline = TRUE) |> 
  e_globe(
    environment = ea_asset("starfield"),
    base_texture = ea_asset("world topo"), 
    height_texture = ea_asset("world topo"),
    displacementScale = 0.04
  ) |> 
  e_bar_3d(lat, n,Country, coord_system = "globe") |> 
  e_timeline_opts(left = 10,right = 10)|>
  e_tooltip()|>
  e_visual_map(n,inRange = list(opacity = c(0.6,1)),show = FALSE, dimension = 2)|>
  e_theme("infographic")|>
  e_animation(easing = 'quarticIn',easing.update = 'quarticIn')|>
  e_title("ALL Anthrax Reports by Semester", "WAHIS Public Quantitative data from WHO | SFA")




##MAP

typeof(anth$Deaths)
data<-anth %>% group_by(Year,Country) %>% 
  summarise(n = n(),Deaths = sum(as.numeric(Deaths)),Cases = sum(as.numeric(Cases))) %>% 
  ungroup()

data<-replace(data, is.na(data), 0)
data$lethality<-data$Deaths/data$Cases
data<-replace(data, is.na(data), 0)


data<-merge(data,finalOut,by = "Country",all.x = TRUE)
data |> group_by(Year)|>
  e_charts(lon,timeline = TRUE) |> 
  e_geo() |> 
  e_scatter(lat,n,lethality, coord_system = "geo", name = "reports",rm_y = FALSE, rm_x = FALSE)|>
  e_visual_map(n,inRange = list(symbol = "diamond",symbolSize = c(8,40)),scale = my_scale,dimension = 2)|>
  e_visual_map(lethality,inRange = list(colorLightness = c(0.5,0.2), colorHue = c(50,10),colorSaturation = c(200,100)),scale = e_scale,dimension = 3,bottom = 300)|>
  e_tooltip()|>
  e_theme("essos")|>
  e_animation(easing.update = 'quarticOut',easing = 'quarticOut')




#River Plot
data|>group_by(Country)|>e_charts(Year)|>
  e_river(n)|>
  e_legend(right = 5,top = 30,bottom = 30,selector = "inverse",show=TRUE,icon = 'circle',emphasis = list(selectorLabel = list(offset = list(10,0))), align = 'right',type = "scroll",width = 10,orient = "vertical")|>
  e_theme("chalk")|>
  e_data





df|> group_by(zone)|>ec.init(
  tl.series = list(
    encode = list(x = 'x',y = 'y',symbolSize = ec.clmn(2,scale = 30)),
    groupBy = 'label'
  )
)|>ec.theme("dark-mushroom")

