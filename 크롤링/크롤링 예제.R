# 아카데미 도서 크롤링

library(dplyr)
library(stringr)
library(rvest)
library(rJava)
library(openxlsx)

p <- c()
t <- c()
w <- c()

trim <- function(x) gsub('^\\s+|\\s+$',"",x)

base_url <- 'http://www.hanbit.co.kr/academy/books/category_list.html?'
page <- 'page='
pageNum <- c(6, 4, 3, 3, 1, 2, 2, 2 ,1 , 1, 1)
category <- '&cate_cd='
number <- '00'
cateNum <- c(4007,4008, 4003:4006, 5005,5001:5004)

p <- c()
t <- c()
w <- c()
s <- c()

for(x in cateNum){
  for( y in pageNum){
    url <- paste0(base_url,page,y,category,number,x)
    html <- read_html(url)
    book_list <- html_node(html, '.sub_book_list_area')
    lis <- html_nodes(book_list, 'li')
    lis <- html %>% 
      html_node('.sub_book_list_area') %>% 
      html_nodes('li')
    for (li in lis){
      price <- html_node(li, '.price') %>% html_text()
      price <- gsub('\\\\', "", price)
      p <- c(p, price)
      title <- html_node(li, '.book_tit') %>% html_text()
      t <- c(t, title)
      writer <- html_node(li, '.book_writer') %>% html_text()
      w <- c(w, writer)
      s <- c(s, x)
    }
  }
}

books <- data.frame(title = t, writer = w, price = p, category = s)
book <- createWorkbook('book')

sheetsName <- c('컴퓨터공학', '정보통신_전기_전자', '수학_과학_공학', '프로그래밍_웹', '그래픽_디자인', 
                'OA_활용', '전기기본서', '전기기사', '전기산업기사', '전기공사기사', 
                '전기공사산업기사')

for( i in sheetsName){
  addWorksheet(book, i)
}

for( i in 1:11){
  book_tmp <- books %>% filter(category == cateNum[i]) %>% select(-category)
  writeDataTable(book, sheet = i, book_tmp)
}

saveWorkbook(book, file = 'd:/book2.xlsx',overwrite = T)
