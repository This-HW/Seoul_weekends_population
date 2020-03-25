library(readr)
library(readxl)
library(dplyr)

#주말 날짜 벡터 생성하기
#시작일 지정
start_date <- as.Date("2017-01-01")
#종료일 지정
end_date <- as.Date("2019-09-30")

#일단위로 일련의 날짜 생성하기
df <-data.frame(seq(as.Date(start_date), as.Date(end_date), by=1))
names(df) <-"date_yr"
df$weekday <- weekdays(as.Date(df$date_yr))

# 주말만 추출하기
date_data <- df[(df$weekday == '토요일') | (df$weekday == "일요일"),]
date_data <- date_data[,1]
week_date <- NULL

# 주말 날짜 벡터 만들기 
for (i in 1:length(date_data)){
  tem <- paste0(substr(date_data[i],1,4),substr(date_data[i],6,7),substr(date_data[i],9,10))
  week_date <- append(week_date, tem)
}


# 서울 내 데이터 생성하기
final_data <- NULL

# 동이름과 코드 매치하는 데이터프레임 생성 
maping_data <- read_xlsx("C:/Users/student/Desktop/semi/data/maping_data.xlsx")
maping_data <- maping_data[,c(2,5)]
names(maping_data) <- c("location_cd","dong")
maping_data <- as.data.frame(maping_data)

# 동별 17시의 생활인구 구하기
for (i in 1:length(week_date)){
  data <- read_csv(paste0("C:/Users/student/Desktop/semi/data/LOCAL_PEOPLE_",20190930,".csv"))
  data <- as.data.frame(data)
  data <- data[,c(1:3,5)]
  names(data) <- c("date_ID","time","location_cd","live_pop")
  data$date_ID <- as.character(data$date_ID)
  data$location_cd <- as.character(data$location_cd)
  data <- filter(data, time=="17")
  data <- left_join(data,maping_data, by="location_cd")
  
  data_test1 <- aggregate(live_pop~time+location_cd,data,sum)
  
  data_test2 <- inner_join(data_test1,maping_data, by="location_cd")
  data_test2$date_ID <- week_date[i]
  data_test2 <- data_test2[,c(5,1,2,3,4)]
  
  final_data <- bind_rows(final_data, data_test2)
}

head(final_data)
tail(final_data)

write.csv(final_data,"seoul_dong_livepop.csv")

# 서울 외 지역 생활인구 구하기
final_metrodata <- NULL

for (i in 1:length(week_date)){
  metro <- read_csv(paste0("C:/Users/student/Desktop/semi/data/METRO_PEOPLE_",week_date[i],".csv"))
  metro <- as.data.frame(metro)
  metro <- metro[,c(1:3,6)]
  names(metro) <- c("date_ID","time","location_cd","live_pop")
  metro$date_ID <- as.character(metro$date_ID)
  metro$location_cd <- as.character(metro$location_cd)
  metro$time <- as.character(metro$time)
  metro <- filter(metro, time=="17")
  
  metro1 <- aggregate(live_pop~time+location_cd,metro,sum)
  
  metro2 <- inner_join(metro1,maping_data, by="location_cd")
  metro2$date_ID <- week_date[i]
  metro2 <- metro2[,c(5,1,2,3,4)]
  
  final_metrodata  <- bind_rows(final_metrodata , metro2)
}

# 결측치 처리
missing_data <- c("20181125","17", "11170580",669,"효창동")
names(missing_data) <- c("date_ID", "time", "location_cd", "live_pop", "dong")
final_metrodata <- rbind(final_metrodata[1:83990,],missing_data,final_metrodata[83991:121687,])

write.csv(final_metrodata,"metro_dong_livepop.csv")


# 총 생활인구 구하기
Sys.setlocale("LC_ALL","Korean") # 언어 다시 한글로
final_data <- read.csv("C:/Users/student/Desktop/semi/data/seoul_dong_livepop.csv")
final_metrodata <- read.csv("C:/Users/student/Desktop/semi/data/metro_dong_livepop.csv")
total <- bind_rows(final_data, final_metrodata)
total <- total[-1]
total <- aggregate(live_pop~date_ID+time+location_cd+dong,total,sum)
total <- total[,c(1:3,5,4)]

write.csv(total,"total_livepop.csv")

# 총 생활인구 중 거주인구를 제하여 총 유동인구 구하기
# 거주인구는 일,화,목 오전 3시를 기준으로 평균 값을 구함
total <- read.csv("total_livepop.csv")
live <- read.csv("seoul_livepop_mean.csv")
total <- total[,-1]
live <- live[,c(-1,-5)]
live$location_cd <- as.character(live$location_cd)
live <- left_join(live,maping_data, by="location_cd")
names(live) <- c("date_ID", "time", "location_cd", "live_pop", "dong")

# 17년 1월 1일이 일요일이므로 해당 데이터 삭제 
total$date_ID <- as.character(total$date_ID)
tem <- NULL
for (i in 2:length(week_date)){
  total_test <- subset(total,date_ID==week_date[i])
  tem <-  bind_rows(tem , total_test)
}
total <- tem
names(total) <- c("date_ID", "time", "location_cd", "live_pop", "dong")

# 주말 자료와의 비교를 위해 live 데이터를 두번 반복한다.(정렬을 위해 date값은 임의로 조정한다.)
live_two <-live
live_two$date_ID <- live_two$date_ID+1
live <-  bind_rows(live , live_two)

# 두 데이터프레임을 정렬한다.
live_two <- arrange(live_two,date_ID, location_cd, dong)
total <- arrange(total,date_ID, location_cd, dong)

total$final_pop <- total$live_pop - live$live_pop
sum(total$final_pop)/286

final <- total[,c(1,3,5,6)]
write.csv(final,"final_data.csv")
