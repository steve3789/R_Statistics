library(XML)
library(stringr)
library(rvest)
library(dplyr)
library(reshape2)
library(rJava)
library(tm)
library(KoNLP)
library(wordcloud2)
library(xlsx)
library(readxl)
library(ggplot2)
library(gridExtra)
useSejongDic()

# 크롤링 준비하기
url_base <- 'https://movie.naver.com'
start_url <- '/movie/bi/mi/point.nhn?code=18847#tab'
url <- paste0(url_base, start_url)
html <- read_html(url)
if_url <- html %>%
  html_node('iframe.ifr') %>%
  html_attr('src')

ifr_url <- paste0(url_base, if_url) 

html2 <- read_html(ifr_url)

# 총 데이터 건수로부터 총 페이지수 구하기
html2 %>%
  html_node('div.score_total') %>%
  html_nodes('em') -> ems
pages <- ems[2] %>% html_text()
pages <- gsub(",", "", pages)
total_page <- ceiling(as.numeric(pages)/10)
page <- '&page='

# 임시 데이터 배열 생성
r <- c()
s <- c()
t <- c()

# 타이타닉 리뷰 및 평점 크롤링
for(i in 1:total_page){
  url3 <- paste0(ifr_url, page, i)
  html3 <- read_html(url3)

  temp_reple <- html3 %>%
    html_nodes('.score_reple') %>%
    html_nodes('p') %>%
    html_text()

  temp_score <- html3 %>%
    html_nodes('.star_score') %>%
    html_nodes('em') %>%
    html_text()
  
  temp_time <- html3 %>%
    html_nodes('.score_reple') %>%
    html_nodes('em') %>%
    html_text()
  leng <- seq(2, length(temp_time), 3)

  r <- c(r, temp_reple)
  s <- c(s, temp_score[3:length(temp_score)])
  t <- c(t, temp_time[leng])
}

# 크롤링한 데이터를 데이터프라임으로 제작
reple <- data.frame(text = r, score = s, time = t)

# 파일 쓰기
write.xlsx(reple, file="D:/workspace/R_Statistics/MovieInfo_Titanic.xlsx", 
           sheetName="info", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

# 파일 읽기
MovieInfo_Titanic <- read_excel(path = "D:/workspace/R_Statistics/MovieInfo_Titanic.xlsx")

# 리뷰 데이터만 추출
reple_wordcloud <- MovieInfo_Titanic$text
# 이상치가 포함된 데이터 제거
reple_wordcloud <- reple_wordcloud[-20017]

# 데이터 필터링
reple_Noun <- sapply(reple_wordcloud, extractNoun, USE.NAMES = F)
reple_unlist <- unlist(reple_Noun)
reple_replace<- str_replace_all(reple_unlist, "[^[:alpha:]]","")
reple_filter1<- Filter(function(x) {nchar(x) <= 10}, reple_replace)
reple_filter2<- Filter(function(x) {nchar(x) >= 2}, reple_filter1)
head(sort(table(reple_filter2),decreasing = T),100)

# 데이터 출력
wordcloudRepleTxt <-as.data.frame(sort(table(reple_filter2), decreasing = T))
wordcloudRepleTxt2 <- wordcloudRepleTxt %>% filter(wordcloudRepleTxt$reple_filter2 != '영화')
head(wordcloudRepleTxt2)

# 과도한 단어 비율에 대한 정제
for( i in 1:5){
  wordcloudRepleTxt2[i,2] <- (i * -200) + 2200
}

# wordcloud2 출력력
wordcloud2(wordcloudRepleTxt2, maxRotation = -pi/36,  minSize = 2, rotateRatio = 1,  color = "random-light", backgroundColor = "grey")

# 데이터 시간별 컬럼 추가
score_time_info <- MovieInfo_Titanic %>% select(-text)
temp_time <- as.data.frame(t(data.frame(str_split(score_time_info$time, pattern = ' '))))
score_time_info <- score_time_info %>% 
  mutate(year = substr(temp_time$V1,1,4), 
         month = substr(temp_time$V1,6,7), 
         day = substr(temp_time$V1,9,10),
         day = substr(temp_time$V1,9,10),
         hour = substr(temp_time$V2,1,2),
         ) %>% 
  select(-time)
# factor -> numeric
score_time_info$score <- as.numeric(score_time_info$score)
score_time_info$year <- as.numeric(score_time_info$year)
score_time_info$month <- as.numeric(score_time_info$month)
score_time_info$day <- as.numeric(score_time_info$day)
score_time_info$hour <- as.numeric(score_time_info$hour)

# 각 시간별 평균 평점 계산
score_year_data <- score_time_info %>% group_by(year) %>% summarise(meanScore = mean(score))
score_month_data <- score_time_info %>% group_by(month) %>% summarise(meanScore = mean(score))
score_day_data <- score_time_info %>% group_by(day) %>% summarise(meanScore = mean(score))
score_hour_data <- score_time_info %>% group_by(hour) %>% summarise(meanScore = mean(score))

# 그래프 그리기 - year
year_plot <- ggplot(data = score_year_data, mapping = aes(x = year, y = meanScore, color = meanScore, group = 1)) + geom_line(size =1) +geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2003,2019,2)) + scale_y_continuous(limits = c(7,10)) +
  ggtitle('년도별 평점 변동 그래프') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        axis.text.x = element_text(angle = 90)); year_plot 

# 그래프 그리기 - month
month_plot <- ggplot(data = score_month_data, mapping = aes(x = month, y = meanScore, color = meanScore, group = 1)) + geom_line(size =1) +geom_point(size = 2) +
  scale_x_continuous(breaks = c(1:12)) + scale_y_continuous(limits = c(7,10)) +
  ggtitle('월별 평점 변동 그래프') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        theme(axis.text.y = element_text(angle = 90)))
month_plot

# 그래프 그리기 - day
day_plot <- ggplot(data = score_day_data, mapping = aes(x = day, y = meanScore, color = meanScore, group = 1)) + geom_line(size =1) +geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1,31,5)) + scale_y_continuous(limits = c(6.5,10)) +
  ggtitle('일별 평점 변동 그래프') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"))

# 그래프 그리기 - hour
hour_plot <- ggplot(data = score_hour_data, mapping = aes(x = hour, y = meanScore, color = meanScore, group = 1)) + geom_line(size =1) +geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1,31,5)) + scale_y_continuous(limits = c(8, 10)) +
  ggtitle('시간별 평점 변동 그래프') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"))

# 모든 그래프 출력
grid.arrange(year_plot, month_plot, nrow = 2)
grid.arrange(day_plot, hour_plot, nrow = 2)

# 그래프의 이상치에 대한 분석
# 데이터 시간별 컬럼 추가
review_score_time_info <- MovieInfo_Titanic
# 분석이 안되는 이상치 제거
review_score_time_info <- review_score_time_info[-20017,,]
temp_time <- as.data.frame(t(data.frame(str_split(review_score_time_info$time, pattern = ' '))))
review_score_time_info <- review_score_time_info %>% 
  mutate(year = substr(temp_time$V1,1,4), 
         month = substr(temp_time$V1,6,7), 
         day = substr(temp_time$V1,9,10),
         day = substr(temp_time$V1,9,10),
         hour = substr(temp_time$V2,1,2),
  ) %>% 
  select(-time)
# factor -> numeric
review_score_time_info$score <- as.numeric(review_score_time_info$score)
review_score_time_info$year <- as.numeric(review_score_time_info$year)
review_score_time_info$month <- as.numeric(review_score_time_info$month)
review_score_time_info$day <- as.numeric(review_score_time_info$day)
review_score_time_info$hour <- as.numeric(review_score_time_info$hour)

# 평점 3점 이하에 대한 분석
review_score <- review_score_time_info %>% filter(score <= 3)
# 이상치 필터링
review_score_summary <- review_score %>% 
  group_by(year, month, day) %>% 
  summarise(meanScore = mean(score), count = n()) %>% 
  arrange(desc(count)) %>% head(10)

# 이상치가 포함된 날짜 확인
review_score_summary

# 해당 날짜의 리뷰 분석
review_time <- review_score_time_info %>% filter(year == 2013 & month == 3 & day %in% c(19:25))

reple_Noun <- review_time$text
reple_Noun <- sapply(reple_Noun, extractNoun, USE.NAMES = F)
reple_unlist <- unlist(reple_Noun)
reple_replace<- str_replace_all(reple_unlist, "[^[:alpha:]]","")
reple_filter1<- Filter(function(x) {nchar(x) <= 10}, reple_replace)
reple_filter2<- Filter(function(x) {nchar(x) >= 2}, reple_filter1)

wordcloudRepleTxt <-as.data.frame(sort(table(reple_filter2), decreasing = T))

wordcloudRepleTxt2 <- wordcloudRepleTxt %>% filter(wordcloudRepleTxt$reple_filter2 != '영화') %>% head(10)

ggplot(data = wordcloudRepleTxt2, mapping = aes(x = Freq, y = reorder(reple_filter2, Freq))) +
  geom_segment(aes(yend = reple_filter2, color = Freq,), xend = 0, size = 3) + geom_point(size = 7, aes(color = Freq)) + theme_minimal() +
  theme(axis.text = element_text(face = 'bold', size = 15, angle = 15)) +
  ggtitle('2019년 3월 19일 ~ 25일의 단어 빈도 그래프') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))
        
        