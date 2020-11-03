#HCYT789 2020/11/03

rm(list = ls())
gc()
library(stats)
library(rvest)
library(dplyr)
Date <- format(Sys.time(), "%Y-%m-%d")
MainTable <- NULL
#down price table
#元大
url <- "http://jdata.yuanta.com.tw/Z/ZC/ZCW/ZCWG/ZCWG_2317_1.djhtm"

#元富
url <-
  "http://newjust.masterlink.com.tw//z/zc/zcw/zcwg/zcwg_2888_1.djhtm"

#玉山
#https://sjmain.esunsec.com.tw/Z/ZC/ZCW/ZCWG/ZCWG_2884_1.djhtm

HtmlData <- read_html(url)
stockPriceData <- HtmlData %>%
  html_nodes(xpath = "//*[@id='SysJustIFRAMEDIV']") %>%
  html_text()

#Find Price Data
OrigPriceString <-
  grep("GetBcdData*.*');", stockPriceData, value = T)
OrigPriceString <- regexpr("GetBcdData*.*');", stockPriceData)
StartIndex <- OrigPriceString[1]
EndIndex <- attr(OrigPriceString, "match.length")
ConvertComplate <-
  substr(stockPriceData, StartIndex, StartIndex + EndIndex)

#Show Result
cat(ConvertComplate)

###清洗字串
ConvertComplate <- gsub("GetBcdData\\('", "", ConvertComplate)
ConvertComplate <- gsub("\\);\r", "", ConvertComplate)
ConvertComplate <- gsub("'", "", ConvertComplate)
cat(ConvertComplate)

#分割價錢及成交量為兩個陣列 
SplitString <- strsplit(ConvertComplate, " ")

Volume <- unlist(strsplit(SplitString[[1]][2], split = ","))
Price <- unlist(strsplit(SplitString[[1]][1], split = ","))

#合併兩個陣列轉換成Tibbe格式
result = cbind(Volume, Price)
result <- result %>% as_tibble() %>% mutate(
  code = 2317,
  Date = Date,
  Volume = as.numeric(Volume),
  Price = as.numeric(Price)
)

hist(result$Volume)
#合併主表

if (is.null(MainTable)) {
  MainTable <- result
} else{
  MainTable <- MainTable %>% rbind(result)
}



