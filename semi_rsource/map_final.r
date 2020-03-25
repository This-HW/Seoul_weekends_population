library(ggplot2)
library(geojsonio)
library(broom)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(ggmap)
library(mapview)

setwd("C:\\Users\\student\\Desktop\\semi")

rm(list=ls())

# geojson 파일 불러오기
spdf <- geojson_read("dong.geojson",  what = "sp")

# 서울만 추려내기
spdf@data$seoul = substr( spdf@data$adm_nm, 1, 2)
seoul = spdf[ spdf@data$seoul == "서울" , ]

seoul@data$location_cd <- substr(seoul@data$adm_cd2,1,8)
tem <- seoul@data[order( seoul@data$OBJECTID),]

# 인구데이터 불러와서 합치기
final <- read.csv("final_data.csv")

final$final_pop <- round(final$final_pop, 0)
min(final$final_pop)
max(final$final_pop)

date_ls <- final$date_ID
date_ls <- unique(date_ls)
date_ls <- date_ls[-2]
final_sum <- NULL

# 지도 데이터 순서에 맞춰서 데이터 변환
for (date in date_ls){
   final_sub <- filter(final, date_ID==date)
   final_sub$location_cd <- as.character(final_sub$location_cd)
   tem$location_cd <- as.character(tem$location_cd)
   final_total <- left_join(tem, final_sub,  by="location_cd")
   final_total <- final_total[,c(6,8:10)]
   final_sum <- bind_rows(final_sum, final_total)
}

final <- final_sum


buylrd2 <- c("#0061FF", "#0000FF", "#FFFFBF","#FFFFBF", "#FDAE61", "#FDAE61", "#F46D43", "#F46D43", "#F45543", "#D73027",  "#D73027", "#D72C27", "#D72C27", "#C71818", "#C70000","#C70000", "#A50026", "#B8002B", "#980026", "#980026","#980026", "#8F0026", "#8F0026")
sort_date <- list(final %>% filter(date_ID==date_ls[1]))
i <- 2
for (date in date_ls){

   sort_date[i] <- list(final %>% filter( date_ID == date_ls[i]))
   sort_date[[i]]$final_pop <- round(sort_date[[i]]$final_pop,-2)
   no_pop <- scale(sort_date[[i]]$final_pop)
   mypalette <- colorNumeric(palette = buylrd2 , domain = sort_date[[i]]$final_pop)
   mypopup <- paste0(final$dong,'<br> 유동인구: ',sort_date[[i]]$final_pop)  # 지도 로딩하는 데 시간이 너무 오래 걸려서 필요시에 사용

   map <- NULL

   ####################개별 지도 띄울려면 아래 두개만 실행행
   map <- leaflet(seoul) %>%
     addTiles() %>%
     setView(lat=37.565 ,lng=127 , zoom=11) %>%
     addPolygons(stroke =FALSE,
                 smoothFactor = 0.2,
                 fillOpacity = 1,
                 popup=mypopup,  # 지도 로딩하는 데 시간이 너무 오래 걸려서 필요시에 사용
                 color = ~mypalette(sort_date[[i]]$final_pop)) %>%
     addLegend( value = ~sort_date[[i]]$final_pop,
                pal = mypalette,
                title = '유동인구',
                opacity = 1)
   x=date
   # mapshot(map, file =paste0("photo/",x,".png"))
   map
   #################### 개별지도는 여기까지 실행
   print(i)
   i=i+1
 }

stat_data <- final_sum[,c(3,4)]
head(stat_data)
stat_data$dong <- as.factor(stat_data$dong)
table(stat_data)

# library(sf)
# library(ggplot2)
# library(ggmap)
# library(raster)
# library(rgeos)
# library(maptools)
# library(maps)
# library(rgdal)
# # library(broom)
# # library(leaflet)
# # library(RColorBrewer)
# # library(dplyr)
# #
# # # setwd("C:\\Users\\student\\Desktop\\semi")
# #
# # SHP파일 변환
# # read in the shapefile as SpatialPolygonsDataFrame, but don't fortify it yet
# hsa <- readOGR(dsn = "EMD_201905/TL_SCCO_EMD.shp")
# 
# # check if there are invalid geometries & correct that
# rgeos::gIsValid(hsa) #returns FALSE
# hsa <- rgeos::gBuffer(hsa, byid = TRUE, width = 0)
# rgeos::gIsValid(hsa) #returns TRUE
# 
# # fortify shouldn't encounter problems now.
# hsa <- hsa %>% fortify(region = "EMD_CD")
# hsa <- hsa[,c(1,2,6)]
# #
# # 법정동을 행정동으로 전환
# # Sys.setlocale('LC_ALL','C')
# Sys.setlocale('LC_ALL','korean')
# code <- read.csv("code.csv",header=T)
# 
# colnames(code) <- c("city", "dong", "dong_law", "location_cd", "id")
# code <- filter(code, city=="서울특별시")
# 
# code <- as.data.frame(code)
# code$id <- as.character(code$id)
# 
# test <- left_join(hsa,code, by="id")
# test <- filter(test, city=="서울특별시")
# test <- test[,c(1,2,5,7)]
# 
# #
# # 인구데이터 불러와서 합치기
# final <- read.csv("final_data.csv")
# final <- final[,-1]
# final$location_cd <- as.character(final$location_cd)
# test$location_cd <- as.character(test$location_cd)
# 
# final_test <- left_join(final, test,  by="location_cd")
# final_sub1 <- filter(final_test,date_ID=="20170107")
# 
# # 시각화하기
# # fillPalette <- c("#0061FF", "#0000FF", "#FFFFBF","#FFFFBF", "#FDAE61", "#FDAE61", "#F46D43", "#F46D43", "#F45543", "#D73027",  "#D73027", "#D72C27", "#D72C27", "#C71818", "#C70000","#C70000", "#A50026", "#B8002B", "#980026", "#980026","#980026", "#8F0026", "#8F0026")
# # plot <- ggplot(data=final_sub1,aes(x=long, y=lat)) + geom_polygon( aes(x=long, y=lat, group=location_cd),fill=abs(final_sub1$final_pop))
# # plot +theme_bw() + labs(title = "서울시 동별 유동인구") +
# #     theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face="bold", size=18,hjust=0.5))+
# #   labs(x="경도",y="위도", fill='day')
