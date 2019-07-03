library(XML)
library(stringr)
library(rvest)
library(dplyr)

url_base <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=163788&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page="
r <- c()
s <- c()
num <- html %>%
  html_nodes('div.score_total') %>%
  html_nodes('em') %>%
  html_text()


for(i in 1:100){
  url <- paste0(url_base,i)
  html <- read_html(url_base)
  
  temp_reple <- html %>%
    html_nodes('.score_reple') %>%
    html_nodes('p') %>%
    html_text()
  
  temp_score <- html %>%
    html_nodes('.star_score') %>%
    html_nodes('em') %>%
    html_text()
  
  r <- c(r, temp_reple)
  s <- c(s, temp_score[3:12])
}
reple <- data.frame(text = r, score = s)
head(reple)

