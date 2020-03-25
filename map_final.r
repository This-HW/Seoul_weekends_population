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
spdf_fortified <- tidy(seoul)

# 인구데이터 불러와서 합치기
final <- read.csv("final_data.csv")
korea <- fortify(seoul, region="location_cd")

date_ls <- final$date_ID
date_ls <- unique(date_ls)
# date_ls

buylrd2 <- c("#0061FF", "#0000FF", "#FFFFBF","#FFFFBF", "#FDAE61", "#FDAE61", "#F46D43", "#F46D43", "#F45543", "#D73027",  "#D73027", "#D72C27", "#D72C27", "#C71818", "#C70000","#C70000", "#A50026", "#B8002B", "#980026", "#980026","#980026", "#8F0026", "#8F0026")
sort_date <- list(final %>% filter(date_ID==date_ls[1]))
i <- 1
for (date in date_ls){
  
  sort_date[i] <- list(final %>% filter( date_ID == date_ls[i]))
  sort_date[[i]]$final_pop <- round(sort_date[[i]]$final_pop,-2)
  no_pop <- scale(sort_date[[i]]$final_pop)
  mypalette <- colorNumeric(palette = buylrd2 , domain = sort_date[[i]]$final_pop)
  # mypopup <- paste0(final$dong,'<br> 유동인구: ',sort_date[[i]]$final_pop)  # 지도 로딩하는 데 시간이 너무 오래 걸려서 필요시에 사용
  
  map <- NULL
  
  ####################개별 지도 띄울려면 아래 두개만 실행행
  map <- leaflet(seoul) %>% 
    addTiles() %>% 
    setView(lat=37.565 ,lng=127 , zoom=11) %>%
    addPolygons(stroke =FALSE,
                smoothFactor = 0.2,
                fillOpacity = 1,
                # popup=mypopup,  # 지도 로딩하는 데 시간이 너무 오래 걸려서 필요시에 사용
                color = ~mypalette(sort_date[[i]]$final_pop)) %>%
    addLegend( value = ~sort_date[[i]]$final_pop,
               pal = mypalette,
               title = '유동인구',
               opacity = 1)
  map
  #################### 개별지도는 여기까지 실행
  
  i=i+1
  print(i)
  Sys.sleep(3)
  
}