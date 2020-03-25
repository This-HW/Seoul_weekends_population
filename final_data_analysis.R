
library(ggplot2)

rm(list=ls())

final <- read.csv("final_data.csv")
final$date_ID
final$final_pop <- round(final$final_pop,-2) 
date_ls <- final$date_ID
date_ls <- unique(date_ls)
local_ls <- final$dong
local_ls <- unique(local_ls)

sort_date <- list(final %>% filter(date_ID==date_ls[1]))
i <- 1

for (date in date_ls){
  
  sort_date[i] <- list(final %>% filter( date_ID == date_ls[i]))
  sort_date[[i]]$final_pop <- round(sort_date[[i]]$final_pop,-2) 
  
  i=i+1
  print(i)
}

dong_list <- c("서교동", "여의동", "종로1.2.3.4가동", "삼성1동", "명동", "잠실6동", "반포4동", "잠실3동", "한강로동", "역삼1동")
str(dong_list)

sort_dong <- list(final %>% filter(dong == dong_list[1] ))
i <- 1
dong_list
for (dname in dong_list){
  sort_dong[i] <- list(final %>% filter(dong == dname) %>% arrange(date_ID))
  i =i+1
}


i <- 1
for(dname in dong_list){
  
  title <- paste0(sort_dong[[i]]$dong[1], " 유동인구 추이")
  sort_dong[[i]]$date_ID <- as.factor(sort_dong[[i]]$date_ID)
  sort_dong[[i]]$date_ID <- as.character(sort_dong[[i]]$date_ID)
  sort_dong[[i]]$date_ID <- as.Date(sort_dong[[i]]$date_ID, format="%Y%m%d")
  
  ggplot(sort_dong[[i]], aes(x=date_ID, y=final_pop, group=1)) +
    geom_line() + 
    ggtitle(title) + 
    theme(plot.title=element_text(size=20))
  
  ggsave(paste0("./Graph/",title,".png"))
  i=i+1
}


title <- paste0(sort_dong[[1]]$dong[1], " 유동인구 추이")
sort_dong[[1]]
sort_dong[[1]]$date_ID <- as.character(sort_dong[[1]]$date_ID)
sort_dong[[1]]$date_ID <- as.Date(sort_dong[[1]]$date_ID, format="%Y%m%d")
sort_dong[[1]]$date_ID


sort_dong[[1]]$final_pop
ggplot(sort_dong[[1]], aes(x=date_ID, y=final_pop, group=1)) +
  geom_line() + 
  ggtitle(title) + 
  theme(plot.title=element_text(size=20))

