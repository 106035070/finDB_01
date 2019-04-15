rm(list=ls())

ifelse(!require(quantmod), install.packages('quantmod'), library(quantmod))
tw20_adj_close<-read.csv("2018Q4_20.csv")


ifelse(!require(readr), install.packages('readr'), library(readr))
tw20.txt<-read.table("tw20.txt", header = T)


ifelse(!require(reshape2), install.packages('reshape2'), library(reshape2))
colnames(tw20.txt)<-c("id","", "date", "price")
tw20.xts = dcast(tw20.txt, date~id)


tw20.xts$date<-as.Date(as.character(tw20.xts$date), "%Y%m%d")
library(xts)
tw20.xts.1<-xts(tw20.xts[,-1], order.by = tw20.xts$date)

library(quantmod)
tw20.mon.ret <- to.monthly(tw20.xts.1, indexAt = "lastof", OHLC=FALSE)
head(tw20.mon.ret)

library(PerformanceAnalytics)
library(magrittr)
tw20.day.ret <-Return.calculate(tw20.xts.1, method = "log")
  na.omit()
head(tw20.day.ret)



con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

load.packages('quantmod')

tw50.txt<-readRDS("~/gittest/gittest01/0415/tw50.txt")
head(etf4.all)
str(etf4.all)
etf4.all.1<-etf4.all[complete.cases(etf4.all),]
head(etf4.all.1)
tail(etf4.all.1)
# 1101
data1<-new.env()
data1$prices<-etf4.all.1$`0050`
prices<-data1$prices

sma50<-SMA(prices, 50)
head(sma50, 51)
# buy and hold for 0050
bt.prep(data1, align='keep.all')
names(data1)
data1$weight
data1$execution.price = data1$prices = etf4.all.1$`0050`
data1$weight[] = 1
buy.hold.0050 <- bt.run.share(data1, clean.signal=F, trade.summary = TRUE)
buy.hold.0050 <-bt.run(data1)
# sma 200 for 0050
prices<-data1$prices
sma200<-SMA(prices, 200)
head(sma200, 201)
data1$weight[] <- iif(prices >= sma200, 1, 0)
sma200.0050 <- bt.run(data1, trade.summary=T)   
# sma 50 for 0050
sma50<-SMA(prices, 50)
head(sma50, 51)
data1$weight[] <- iif(prices >= sma50, 1, 0)
sma50.0050 <- bt.run(data1, trade.summary=T)
# sma 50 for 005, short allowed
data1$weight[] <- iif(prices >= sma50, 1, -1)
sma50.0050.short <- bt.run(data1, trade.summary=T)
# summary of investment
models<-list("SMA50"= sma50.0050, 
             "SMA200"= sma200.0050, 
             "SMA50_short" = sma50.0050.short, 
             "BH 0050" = buy.hold.0050)
strategy.performance.snapshoot(models, T)
strategy.performance.snapshoot(models, control=list(comparison=T), sort.performance=T)
plotbt.strategy.sidebyside(models, return.table=T)
# You can plot in ggplot2
library(ggplot2)
all.0050<-merge.xts(sma50.0050$equity, 
                    sma50.0050.short$equity, 
                    sma200.0050$equity, 
                    buy.hold.0050$equity)
colnames(all.0050)<-c("sma50", "sma50 short", "sma200", "BH")
head(all.0050)
all.0050.long<-fortify(all.0050, melt=T)
head(all.0050.long)
#
title = "Cumulative returns of 0050s"
p = ggplot(all.0050.long, aes(x = Index, y = Value)) +
  geom_line(aes(linetype = Series, color = Series)) +
  #geom_point(aes(shape = Series))+
  xlab("year") + ylab("cumulative returns")+
  ggtitle(title)
p
