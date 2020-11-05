#
#   CYTSS Basic (Choose Your Trading Signal System Basic)            
#                                                      
#   Copyright (C) 2020 ~ Now  Chung-YI, H.             
#                                                      
#   Ver: 0.789 Alpha for MACD                                       
#                                                      
#   Last Update Time: 2020-11-05                          
#                                                      
#   WebSite: https://www.hcytlog.com                      
#
#   This project using TTR、Tidyverse、quantmod、data.table、tidyquant package
##################################################################################
#------------------------初始化------------------------
rm(list = ls())
gc()
library(data.table)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(TTR)

#change Folder
setwd("E:/SourceCode/R/StockR/")
#setting FileName
FileName <- "E:/SourceCode/R/StockR/Data/StockData.csv"
#Loading File
Data <-read.table(file = FileName,header = T,stringsAsFactors = F,sep = ",")

#------------------------程式開始------------------------
StockData <- Data
colnames(StockData) <-
  c("code", "name", "date", "open", "high", "low", "close", "volume")
#股票最低天數
QTY <- 60
#股票最低成交量
LIMIT_VOLUME <- 0

#------------------------5日均量要9成要大於500張------------------------
vol_require <- LIMIT_VOLUME
require_ratio <- 0.9
code_Big_Vol <- StockData %>%
  group_by(code) %>%
  filter(n() >= QTY) %>%
  mutate(volMA5 = SMA(volume, 5),
         is_volbig500 = ifelse(volMA5 > vol_require, 1, 0)) %>%
  group_by(code) %>%
  summarise(volbig500_Ratio = mean(is_volbig500, na.rm = T)) %>%
  filter(volbig500_Ratio >= require_ratio) %>% pull(code)
StockData <- StockData %>%
  filter(code %in% code_Big_Vol)
#------------------------調整日期格式------------------------
DateConvert <- function(date) {
  date <- as.Date(as.character(date), "%Y%m%d")
  return(date)
}
StockData <- StockData %>% mutate(date = DateConvert(date))
#轉換格式確定是數字型態
StockData <- StockData %>%
  arrange(code, date) %>% group_by(code) %>%
  filter(n() >= QTY) %>%
  mutate(
    open = as.numeric(open, na.rm = TRUE),
    high = as.numeric(high, na.rm = TRUE),
    low = as.numeric(low, na.rm = TRUE),
    close = as.numeric(close, na.rm = TRUE)
  ) %>% na.omit()

#------------------------建立技術分析指標及基礎欄位------------------------
StockData <- StockData %>%
  arrange(code, date) %>% group_by(code) %>%
  filter(n() >= QTY) %>%
  mutate(
    UP = ifelse(close > open, 1, 0),
    LogRet=log(close/lag(close)),
    leadOpen1=lead(open,1),   #隔日開盤價
    Buy_date=lead(date,1),    #買進日期
    MA5=SMA(close,n=5),       #5日均線
    MA10=SMA(close,n=10),     #10日均線
    MA20=SMA(close,n=20),     #20日均線
    MA40=SMA(close,n=40),     #40日均線     
    MA60=SMA(close,n=60),     #60日均線  
  )

StockData <-
  StockData %>% group_by(code) %>% arrange(code, date) %>%
  tq_mutate(
    select = close,
    mutate_fun = MACD,
    nFast = 12,
    nSlow = 26,
    nSig  = 9,
    percent = F
  ) %>% rename("DIFF" = "macd", "MACD" = "signal")

StockData <- StockData %>% mutate(
  OSC = 2*(DIFF-MACD),
  LagMacd=lag(MACD,1),
  Slope_MACD8=(lag(MACD,8)-MACD)/8,
  Slope_MACD4=(lag(MACD,4)-MACD)/4,
  F_MACD=ifelse(MACD>LagMacd,1,0)
  
  )

#########################開始回測###########################


tradeDetailTable<-NULL
tradeTargetTable<-NULL
MaintradeDetailTable<-NULL
outSiteTable<-NULL


# 整理進場位置
tradeTargetTable <- StockData %>%
  filter(
    DIFF>MACD,
    OSC>0,
    lag(OSC,1)<0,
    lag(OSC,2)<lag(OSC,1),
    lag(OSC,3)<lag(OSC,2),
  ) %>%  
  select(code, signalDate=date,inDate=Buy_date, inPrice=leadOpen1)%>%arrange(signalDate)


# 整理出場位置
outSiteTable <- StockData %>%
  mutate(
    outSite1=ifelse(DIFF<MACD & OSC < 0 & lag(OSC,1)>OSC & lag(OSC,3)>lag(OSC,2) ,1,0)
  ) %>%  
  filter( outSite1 == 1 ) %>%
  select(code, outDate=date, outPrice=close)%>%
  group_by()

# 整理交易明細表

for(ix in 1:nrow(tradeTargetTable)){
  
  inDate <- tradeTargetTable$inDate[ix]   # 進場日期
  stockCode <- tradeTargetTable$code[ix]  # 股票代碼
  outData <- outSiteTable %>%             # 該支股票代碼資料進場日之後的最近出場日
    filter(code==stockCode, outDate>inDate) %>%
    filter(row_number()==1) %>%
    select(outDate, outPrice)
  if(nrow(outData)>0){
    MaintradeDetailTable <- bind_rows(MaintradeDetailTable, bind_cols(tradeTargetTable[ix,], outData))
  }
}


tradeDetailTable <- MaintradeDetailTable %>% mutate(
  Year=format(as.Date(outDate, format="%d/%m/%Y"),"%Y")
)

ComplateData<-tradeDetailTable
# tradeDetailTable<-ComplateData 
tradeDetailTable<-ComplateData
#write.csv(tradeDetailTable,"E:/Qsync/000.論文撰寫/New0427/Final/RSI_XG.csv", row.names = FALSE)
tradeDetailTableOrg<-ComplateData


  
  tradeDetailTable<-tradeDetailTableOrg
  tradeDetailTable<-tradeDetailTable 
  # 計算考慮交易成本後之報酬率
  buyCostR <- 0.001425
  sellCostR <- 0.004425
  tradeDetailTable$ret <- (tradeDetailTable$outPrice*(1-sellCostR))/(tradeDetailTable$inPrice*(1+buyCostR))-1
  # 計算持有期間日數
  # 日期轉換函數
  DateConvert <- function(x){
    return(as.Date(paste(substring(x,1,4),substring(x,5,6),substring(x,7,8),sep="-",format="%Y%m%d")))
  }
  tradeDetailTable$holdDays <- as.numeric(tradeDetailTable$outDate-tradeDetailTable$inDate)

  # 計算相關績效指標
  # 平均報酬率
  meanRet <- mean(tradeDetailTable$ret)
  # 報酬率標準差
  sdRet <- sd(tradeDetailTable$ret)
  # 交易次數
  tradeNums <- nrow(tradeDetailTable)
  # 勝率
  winRatio <- sum(as.numeric(tradeDetailTable$ret>0))/tradeNums
  # 最大報酬率
  maxRet <- max(tradeDetailTable$ret)
  # 最小報酬率
  minRet <- min(tradeDetailTable$ret)
  # 平均持有期間日數
  avgHoldDays <- mean(tradeDetailTable$holdDays)
  
  # win times
  win <- sum(tradeDetailTable$ret>0)
  
  # lose times
  lose <- sum(tradeDetailTable$ret<0)
  
  # 繪製績效分配圖
  hist(tradeDetailTable$ret, xlab="報酬率", ylab="次數", main=paste0("2015~2019年XGBoost預測MACD交易策略報酬率分配圖(大於90%)"), col="deepskyblue1")
  
  which(tradeDetailTable$ret>0.3)
  
  #輸出結果
  cat(paste0("*********年回測績效*********\n",
             #"回測期間: ",backtestStartDate, " 至 ",backtestEndDate,"\n",
             "平均報酬率: ",round(meanRet*100,2)," %\n",
             "最大報酬率: ",round(maxRet*100,2)," %\n",
             "最小報酬率: ",round(minRet*100,2)," %\n",
             "交易次數: ",tradeNums," 次\n",
             "勝率: ",round(winRatio*100,2)," %\n",
             "報酬率標準差: ",round(sdRet*100,2)," %\n",
             "平均持有日數: ",round(avgHoldDays,2),"天\n",
             "正報酬次數: ",win,"次\n",
             "負報酬次數: ",lose,"次\n"))
 

  # 繪圖函數
  PlotGraph <- function(plotSample){
    
    #  print(plotSample)
    # 繪製交易的股票代碼
    plotCode <- tradeDetailTable$code[plotSample]
    inDate <- tradeDetailTable$inDate[plotSample]
    outDate <- tradeDetailTable$outDate[plotSample]
    
    # 整理該股票的股價資料
    stockData <- StockData[which(StockData$code==plotCode),] %>%
      rename(tradeVolume=volume)
    stockData <- stockData[,c("date","open","high","low","close","tradeVolume","MA5","MA10","MA20","MA60")] # 取出繪圖所需資料(開高收低成交量)
    
    # 繪圖起始日
    matchSite <- which(stockData$date==inDate)-50
    plotStartDate <- stockData$date[ifelse(matchSite<1, 1, matchSite)]  # 此處用ifelse避免資料超出邊界
    
    # 繪圖結束日
    matchSite <- which(stockData$date==outDate)+30
    plotEndDate <- stockData$date[ifelse(matchSite>nrow(stockData), nrow(stockData), matchSite)] # 此處用ifelse避免資料超出邊界
    
    # 要繪製的股價資料範圍
    plotData <- stockData[which((stockData$date>=plotStartDate)&(stockData$date<=plotEndDate)),]
    
    # 加入進場位置資訊
    plotData$inSite <- rep(NA, nrow(plotData))
    plotData$inSite[which(plotData$date==inDate)] <- plotData$close[which(plotData$date==inDate)]
    
    # 加入出場位置資訊
    plotData$outSite <- rep(NA, nrow(plotData))
    plotData$outSite[which(plotData$date==outDate)] <- plotData$close[which(plotData$date==outDate)]
    
    # 將plotData資料由data.frame格式轉為xts，符合繪圖資料格式要求
    # plotData <- xts(plotData[,-1], order.by= as.Date(ISOdate(year=substring(plotData$date,1,4),
    #                                                          month=substring(plotData$date,5,6),
    #                                                          day=substring(plotData$date,7,8)),format="%Y%m%d"))
    #
    plotData <- xts(plotData[,-1], order.by= plotData$date)
    # 繪製技術分析圖形
    # 設定K棒顏色
    myTheme <- chart_theme()
    myTheme$col$dn.col <- c("chartreuse3")  # 跌K棒顏色
    myTheme$col$up.col <- c("firebrick3")   # 漲K棒顏色
    # 繪製主圖形
    chart_Series(plotData[,1:5], name=paste0(plotCode," 技術分析圖"), theme=myTheme)
    # 加入成交量
    add_Vo()
    # 加入5日移動平均線
    add_TA(plotData$MA5, on=1, type="l", col="blue", lwd=1.5)
    # 加入10日移動平均線
    add_TA(plotData$MA10, on=1, type="l", col="Black", lwd=1.5)
    # 加入20日移動平均線
    add_TA(plotData$MA20, on=1, type="l", col="orange", lwd=1.5)
    add_MACD()
    # 標註進場位置
    add_TA(plotData$inSite, on=1, type="p", col="red", pch=2, cex=5, lwd=3)
    # 標註出場位置
    add_TA(plotData$outSite, on=1, type="p", col="green", pch=6, cex=5, lwd=3)
  # 繪製交易技術分析圖形 plotSample為交易明細表內的列數
  }
  
  #最壞前三名
  top3<-tradeDetailTable %>% arrange(holdDays)%>% head(3)
  for (ix in 1:nrow(top3)) {
    TopIndex<- which(tradeDetailTable$code == top3[ix,1]$code &  tradeDetailTable$ret ==  top3[ix,8]$ret)
    PlotGraph(plotSample=TopIndex) %>% print()
  }
  
  
  
  # 最好前三名
  Tail3<-tradeDetailTable %>% arrange(ret) %>% tail(3)
  for (ix in 1:nrow(Tail3)) {
    TailIndex<- which(tradeDetailTable$code == Tail3[ix,1]$code &  tradeDetailTable$ret ==  Tail3[ix,8]$ret)
    PlotGraph(plotSample=TailIndex) %>% print()
  }

  


