setwd("/Library/WebServer/CGI-Executables/Adaptive-Transform")

sink(file("log.txt", open="wt"), type="message")
options(scipen=12)

library(IBrokers)
library(twitteR)
library(ROAuth)

load("Credentials.RData")
registerTwitterOAuth(Cred)

sink("memory_dump.txt")
cat("**************************************************** \n")
cat("Loading libraries... \n")

library(stats4)
library(class)
library(e1071) 
library(tseries)
library(TTR)
library(fTrading)
library(RCurl)
library(chron)
library(timeDate)
library(mailR)
library(quantmod)

cat("[Libraries loaded successfully] \n")
cat("Loading engine via linked directory... \n")

source("eWrapper.data.Last.R")
source("pi_engine.R")

cat("[Engine loaded successfully] \n")

cat("Attempting to connect to IBGateway via API protocol... \n")
if(isConnection()){
  tws <- ibgConnect(clientId=1)
} else {
  while(!isConnection())
  {
    cat("Network connection interupted... \n")  	
    Sys.sleep(30)
    
    if(isConnection())
    {
      Sys.sleep(30)
      tws <- ibgConnect(clientId=1)
      cat("[Network connection re-established] \n")	
      break
    }		
  }
}
cat("[System online] \n")

fx.list <- rbind(c("EUR","USD"), c("USD","JPY"), c("GBP","USD"))
fx.lock <- list(TRUE, TRUE, TRUE)
ccy <- lapply(1:nrow(fx.list), function(i){ContractDetails(fx.list[i,1],fx.list[i,2],"forex", tws)})
cct <- lapply(1:nrow(fx.list), function(i){ContractDetails(fx.list[i,1],fx.list[i,2],"currency", tws)})
img.dir <- "/Library/WebServer/Documents/PriceTape.png" 
margin <- 0.023
oca <- 0; init <- FALSE; filled <- FALSE; t.tree <- as.matrix(seq(0,23,1))

while(TRUE)
{
  start.time <- Sys.time()
  
  if(format(Sys.time(), "%A") == "Friday" && format(Sys.time(), "%H:%M") >= "14:00")
  {
    cat("Market closure for weekend/holiday(s). \n")
    break	
  }
  
  t.node <- which(as.numeric(format(Sys.time(), "%H")) - t.tree == 0, arr.ind=FALSE)
  t.next <- paste(as.character(ifelse(t.node < 23, t.node+1, 0)),":00", sep="")

  while(TRUE)
  {
    tried <- try({
      mkt <- lapply(1:nrow(fx.list), function(i){reqMktData(tws, ccy[[i]], eventWrapper = eWrapper.data.Last(1), CALLBACK = snapShot)})
      if(identical(format(Sys.time(), "%H:%M"), t.next) || !(init)) {
        sec <- lapply(1:nrow(fx.list), function(i){
          temp <- OHLC(reqHistoricalData(tws, ccy[[i]], barSize='1 hour', duration='2 W', whatToShow="MIDPOINT"))
          temp[nrow(temp),2:4] <- c(NA, NA, NA)
          return(temp)
        })
      }
      init <- TRUE
    }, silent = TRUE)    
    if(inherits(tried, "try-error")) {
      cat("Corrupted connection. Applying patch... \n")		
      Sys.sleep(1)
    } else {
      if(length(sec) > 0) {
        cat("[Connection okay] \n")
        break
      } else {
        cat("Corrupted connection. Applying patch... \n")
        twsDisconnect(tws)
        if(isConnection()){
          tws <- ibgConnect(clientId=1)
        } else {
          while(!isConnection())
          {
            cat("Network connection interupted... \n")		
            Sys.sleep(1)
            
            if(isConnection())
            {
              Sys.sleep(1)
              tws <- ibgConnect(clientId=1)
              cat("[Network connection re-established] \n")	
              break
            }		
          }
        }
      }
    }
  }
  
  t.band <- transform(sec, 14, sm.par = 120, basis = 0.0001, na.rm = TRUE)
  init.cap <- as.numeric(reqAccountUpdates(tws)[[1]]$NetLiquidation[1]); lot.size <- (init.cap*margin)^2
  
  fx.lock <- lapply(1:nrow(fx.list), function(i) {
    if(tail(t.band[[i]]$gamma, 1) > mkt[[i]]$AskPrice && tail(t.band[[i]]$omega, 1) < mkt[[i]]$BidPrice) FALSE else TRUE
  })
  
  for(i in 1:nrow(fx.list))
  {
    if(!fx.lock[[i]])
    {
      if(mkt[[i]]$AskPrice > tail(t.band[[i]]$gamma, 1))
      {
        oca <- format(Sys.time(), "%Y%m%d %X"); filled <- TRUE;
        placeOrder(tws, cct[[i]], Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="MKT"))  
        
        cat("BUY SIGNAL @ ",as.numeric(twsPortfolioValue(reqAccountUpdates(tws))$averageCost)," on ",format(Sys.time(), "%Y %a %b %d %X"),"\n")
        cat("********************************************************************* \n")	
        
        placeOrder(tws, cct[[i]], Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="STP", auxPrice=tail(t.band[[i]]$gamma, 1), tif="GTC", ocaGroup=oca))
        
        #         send.mail(from = "pashazavari@gmail.com",
        #                   to = "6044416599@pcs.rogers.com",
        #                   subject = "FOREXBot Trade Activity Notification",
        #                   body = paste("BUY SIGNAL @ ",as.numeric(twsPortfolioValue(reqAccountUpdates(tws))$averageCost)," on ",format(Sys.time(), "%Y %a %b %d %X"),"@ http://24.87.162.133/Forex_bot.html"),
        #                   smtp = list(host.name = "smtp.gmail.com", port = 587, user.name = "pashazavari", passwd = "vancouverthedawg", ssl = TRUE),
        #                   authenticate = TRUE,
        #                   send = TRUE)
       
        init.cap <- as.real(reqAccountUpdates(tws)[[1]]$NetLiquidation[1])
        
        cat("AVAILABLE FUNDS: $", as.numeric(init.cap),"\n")
        cat("********************************************************************* \n")	        
      }   

      if(mkt[[i]]$BidPrice < tail(t.band[[i]]$omgea, 1))
      {
        oca <- format(Sys.time(), "%Y%m%d %X"); filled <- TRUE; 
        placeOrder(tws, cct[[i]], Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="MKT"))  
        
        cat("SELL SIGNAL @ ",as.numeric(twsPortfolioValue(reqAccountUpdates(tws))$averageCost)," on ",format(Sys.time(), "%Y %a %b %d %X"),"\n")
        cat("********************************************************************* \n")  
        
        placeOrder(tws, cct[[i]], Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="STP", auxPrice=tail(t.band[[i]]$omega, 1), tif="GTC", ocaGroup=oca))
        
        #         send.mail(from = "pashazavari@gmail.com",
        #                   to = "6044416599@pcs.rogers.com",
        #                   subject = "FOREXBot Trade Activity Notification",
        #                   body = paste("BUY SIGNAL @ ",as.numeric(twsPortfolioValue(reqAccountUpdates(tws))$averageCost)," on ",format(Sys.time(), "%Y %a %b %d %X"),"@ http://24.87.162.133/Forex_bot.html"),
        #                   smtp = list(host.name = "smtp.gmail.com", port = 587, user.name = "pashazavari", passwd = "vancouverthedawg", ssl = TRUE),
        #                   authenticate = TRUE,
        #                   send = TRUE)
        
        init.cap <- as.real(reqAccountUpdates(tws)[[1]]$NetLiquidation[1])
        
        cat("AVAILABLE FUNDS: $", as.numeric(init.cap),"\n")
        cat("********************************************************************* \n")	        
      } 
    }
  }

  png(filename=img.dir, width=750, height=500)
  par(mfrow=c(3,1), mar=c(2.1, 4.1, 2.1, 1.1), xpd=TRUE, bg="#444444",cex.lab = 0.8,cex.main = 0.9, cex.axis=0.6, col.axis="#CCCCCC", col.lab="#CCCCCC", col.main="#CCCCCC", fg="#CCCCCC", adj=1)
  for(i in 1:nrow(fx.list))
  {
    temp <- t.band[[i]]
    temp[nrow(temp),1] <- (mkt[[i]]$AskPrice+mkt[[i]]$BidPrice)/2
    matplot(cbind(temp$omega, temp$gamma, Op(temp), Hi(temp), Lo(temp)), lty= 1, lwd=1.5, col=heat.colors(n = 5), type="l", main=paste(toString(fx.list[i,])," [",format(Sys.time(), "%Y %a %b %d %X"),"]", sep=""), xlab=NA, ylab=NA, ylim=c(min(temp$omega,Lo(temp), na.rm = TRUE),max(temp$gamma,Hi(temp), na.rm = TRUE)))
    title(ylab="Spot Rate", xlab=NA, adj=0.5)
    lnames <- c(expression(~ omega), expression(~ gamma),'Open', 'High', 'Low') 
    legend("topleft", lnames, inset=c(0,-0.3), col=heat.colors(n = 5), lty = 1, lwd=2, horiz=TRUE, text.width=(3), cex=0.9, pt.cex=10, bty="n") 
    par(xpd=FALSE)
    grid(lty = 3, col="#AAAAAA")
    abline(v=nrow(temp)-1, lty=3, lwd=3, col="#AAAAAA")
    if(fx.lock[[i]]) mtext("LOCKED", side=4, col="red", line=0.2, cex=0.8)
  }
  
  if(t.next == "00:00" || init)
  { 
    write.table(init.cap, "eq.curve.csv", sep=",", append=TRUE, row.names=FALSE, col.names=FALSE, fileEncoding="UTF-8")
    eq.curve <- as.matrix(read.csv("eq.curve.csv", header=FALSE)) 
    
    png(filename=img.dir, width=750, height=500)
    matplot(eq.curve, lty= 1, lwd=1.5, col=heat.colors(n = 1), type="l", main=paste("Portfolio size as of ",format(Sys.time(), "%Y %a %b %d %X"), sep=""), xlab=NA, ylab=NA, ylim=c(min(temp$omega,Lo(temp), na.rm = TRUE),max(temp$gamma,Hi(temp), na.rm = TRUE)))
    title(ylab="$CAD", xlab=NA, adj=0.5)
    lnames <- c('Equity Curve') 
    legend("topleft", lnames, inset=c(0,-0.3), col=heat.colors(n = 1), lty = 1, lwd=2, horiz=TRUE, text.width=(3), cex=0.9, pt.cex=10, bty="n") 
    par(xpd=FALSE)
    grid(lty = 3, col="#AAAAAA")
    dev.off()   
  }
 
  end.time <- Sys.time()
  Sys.sleep(30-(end.time-start.time))
}

cat("**************************************************** \n")
cat("Application shutdown initiated... \n")
twsDisconnect(tws)
cat("Shutdown successfull! \n")
cat("**************************************************** \n")

sink()
quit(save="no")