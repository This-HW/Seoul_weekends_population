library(ggplot2)
library(geojsonio)
library(broom)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(ggmap)

rm(list=ls())

# geojson 파일 불러오기
spdf <- geojson_read("dong.geojson",  what = "sp")

# 서울만 추려내기
spdf@data$seoul = substr( spdf@data$adm_nm, 1, 2)
seoul = spdf[ spdf@data$seoul == "서울" , ]

seoul@data$location_cd <- substr(seoul@data$adm_cd2,1,8)

# str(seoul@data)
# View(seoul@data)

# 'fortify' the data to get a dataframe format required by ggplot2
spdf_fortified <- tidy(seoul)

# 인구데이터 불러와서 합치기
final <- read.csv("final_data.csv")
final_test <- subset(final,date_ID==20170107)

names(final_test$location_cd) <- "id"
m <- merge(korea,final_test,by=id)

korea <- fortify(seoul, region="location_cd")

date_ls <- final$date_ID
date_ls <- unique(date_ls)

sort_date[100] <- list(final %>% filter(date_ID==date_ls[100]))
# sort_date[[1]]$final_pop

## 삽입중
# View(kormap3@data)

mymap <- final
str(final)


# 기존소스
# mypalette <- colorNumeric(palette ='RdYlBu' , domain = final$final_pop)
mypopup <- paste0(final$dong,'<br> 유동인구: ',sort_date[[100]]$final_pop)


# 수정중 임시소스스 -> 나중에 반복문 안으로
sort_date[[100]]$final_pop <- round(sort_date[[100]]$final_pop,-2)

# pop_max <- max(sort_date[[1]]$final_pop)
# pop_min <- min(sort_date[[1]]$final_pop)

# buylrd <- c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026")
# buylrd1 <- c("#313695", "#E0F3F8", "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#F46D43", "#D73027", "#D73027", "#D73027", "#A50026", "#A50026", "#A50026")
buylrd2 <- c("#0061FF", "#0000FF", "#FFFFBF","#FFFFBF", "#FDAE61", "#FDAE61", "#F46D43", "#F46D43", "#F45543", "#D73027",  "#D73027", "#D72C27", "#D72C27", "#C71818", "#C70000","#C70000", "#A50026", "#B8002B", "#980026", "#980026","#980026", "#8F0026", "#8F0026")
mypalette <- colorNumeric(palette = buylrd2 , domain = final$no_pop)
# mypalette <- colorNumeric(palette = buylrd , domain = sort_date[[1]]$final_pop)
no_pop <- scale(sort_date[[100]]$)
map <- NULL
map <- leaflet(seoul) %>% 
  addTiles() %>% 
  setView(lat=37.565 ,lng=127 , zoom=11) %>%
  addPolygons(stroke =FALSE,
              smoothFactor = 0.2,
              fillOpacity = 1,
              # popup=mypopup,
              # color = ~mypalette(sort_date[[i]]$final_pop)) %>%
              # color = ~mypalette(sort_date[[1]]$final_pop)) %>%
              color = ~mypalette(scale(no_pop))) %>%
  # addLegend( value = ~sort_date[[1]]$final_pop,
  addLegend( value = ~sort_date[[100]],
             pal = mypalette,
             title = '유동인구',
             opacity = 1)
map
# ?ggsave
# sort_date[1] <- list(final %>% filter(date_ID==date_ls[1]))
## 삽입중

i <- 1

for (date in date_ls){

  sort_date[i] <- list(final %>% filter(date_ID==date_ls[i]))
  sort_date[[i]]$final_pop <- round(sort_date[[i]]$final_pop,-2)
  
  # map <- NULL
  # map <- leaflet(seoul) %>% 
  #   addTiles() %>% 
  #   setView(lat=37.559957 ,lng=126.975302 , zoom=10) %>%
  #   addPolygons(stroke =FALSE,
  #               smoothFactor = 0.2,
  #               fillOpacity = 1,
  #               # popup=mypopup,
  #               # color = ~mypalette(sort_date[[i]]$final_pop)) %>%
  #               color = ~mypalette(sort_date[[i]]$final_pop)) %>%
  #   addLegend( value = ~sort_date[[i]]$final_pop,
  #              pal = mypalette,
  #              title = '유동인구',
  #              opacity = 1)
  # map

  i=i+1
  print(i)
  # Sys.sleep(5)

  }

# 
# 
# map7 <- NULL
# map7<-leaflet(seoul) %>% 
#   addTiles() %>% 
#   setView(lat=37.559957 ,lng=126.975302 , zoom=10.5) %>%
#   addPolygons(stroke =FALSE,
#               smoothFactor = .2,
#               fillOpacity = .9,
#               popup=mypopup,
#               color = ~mypalette(mymap$final_pop)) %>%
#   addLegend( value = ~final$final_pop,
#              pal = mypalette,
#              title = '유동인구',
#              opacity = 1)
# map7
