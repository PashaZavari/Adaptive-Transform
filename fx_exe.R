if(!file.exists("sessionData.RData"))
{
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
  library(quantmod)
  
  cat("[Libraries loaded successfully] \n")
  cat("Loading engine via linked directory... \n")
  
  source("eWrapper.data.Last.R")
  source("pi_engine.R")
  
  cat("[Engine loaded successfully] \n")
  
  fx.list <- rbind(c("EUR","USD"), c("USD","JPY"), c("USD","CHF"))
  fx.lock <- list(TRUE, TRUE, TRUE)
  
  img.dir <- "/Library/WebServer/Documents/PriceTape.png"; img.subdir <- "/Library/WebServer/Documents/PortSnap.png"; port.dir <- "/Library/WebServer/Documents/PortSnap.csv";
  
  margin <- 0.023
  oca <- 0; init <- FALSE; filled <- rep(0,nrow(fx.list)); cross <- rep(FALSE,nrow(fx.list)); t.tree <- as.matrix(seq(0,23,1))
}

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
  init.cap <- as.numeric(reqAccountUpdates(tws)[[1]]$NetLiquidation[1]); lot.size <- (init.cap*margin)^2
  
  if(t.next == "00:00" || !init)
  { 
    write.table(init.cap, "eq.curve.csv", sep=",", append=TRUE, row.names=FALSE, col.names=FALSE, fileEncoding="UTF-8")
    eq.curve <- as.matrix(read.csv("eq.curve.csv", header=FALSE)) 
    
    png(filename=img.subdir, width=750, height=500)
    par(mfrow=c(1,1), mar=c(2.1, 4.1, 2.1, 1.1), xpd=TRUE, bg="#444444",cex.lab = 0.8,cex.main = 0.9, cex.axis=0.6, col.axis="#CCCCCC", col.lab="#CCCCCC", col.main="#CCCCCC", fg="#CCCCCC", adj=1)
    matplot(eq.curve, lty= 1, lwd=1.5, col=heat.colors(n = 1), type="l", main=paste("Portfolio size as of ",format(Sys.time(), "%Y %a %b %d %X"), sep=""), xlab=NA, ylab=NA)
    title(ylab="$CAD", xlab=NA, adj=0.5)
    lnames <- c('Equity Curve') 
    legend("topleft", lnames, inset=c(0,-0.3), col=heat.colors(n = 1), lty = 1, lwd=2, horiz=TRUE, text.width=(3), cex=0.9, pt.cex=10, bty="n") 
    par(xpd=FALSE)
    grid(lty = 3, col="#AAAAAA")
    dev.off()   
  }
  
  while(TRUE)
  {
    tried <- try({
      ccy <- lapply(1:nrow(fx.list), function(i){ContractDetails(fx.list[i,1],fx.list[i,2],"forex", tws)})
      mkt <- reqMktData(tws, ccy, eventWrapper = eWrapper.data.Last(3), CALLBACK = snapShot)
      mkt <- lapply(1:nrow(fx.list), function(i){
        mkt[i,]
      })
      if(identical(format(Sys.time(), "%H:%M"), t.next) || !init || format(Sys.time(), "%H:%M") == "14:15") {
        ccy <- lapply(1:nrow(fx.list), function(i){ContractDetails(fx.list[i,1],fx.list[i,2],"forex", tws)})
        sec <- lapply(1:nrow(fx.list), function(j){
          temp <- OHLC(reqHistoricalData(tws, ccy[[j]], barSize='1 hour', duration='2 W', whatToShow="MIDPOINT"))
          temp[nrow(temp),2:4] <- c(NA, NA, NA)
          return(temp)
        })
      }
      init <- TRUE
    }, silent = TRUE)    
    if(inherits(tried, "try-error")) {
      cat("Corrupted connection. Applying patch...",format(Sys.time(), "%Y %a %b %d %X"),"\n")		
      Sys.sleep(1)
    } else {
      if(length(sec) > 0 && length(mkt) > 0) {
        #cat("[Connection okay] \n")
        break
      } else {
        cat("Corrupted connection. Applying patch...",format(Sys.time(), "%Y %a %b %d %X"),"\n")  	
        twsDisconnect(tws)
        if(isConnection()){
          tws <- ibgConnect(clientId=1)
        } else {
          while(!isConnection())
          {
            cat("Network connection interupted...",format(Sys.time(), "%Y %a %b %d %X"),"\n")		
            Sys.sleep(1)
            
            if(isConnection())
            {
              Sys.sleep(1)
              tws <- ibgConnect(clientId=1)
              cat("[Network connection re-established: ",format(Sys.time(), "%Y %a %b %d %X"),"] \n")	
              break
            }		
          }
        }
      }
    }
  }
  
  t.band <- transform(sec, 14, 14, sm.par = 120, basis = 0.0001, na.rm = TRUE)
  
  fx.lock <- lapply(1:nrow(fx.list), function(i) {
    if(fx.lock[[i]]) if(tail(t.band[[i]]$gamma, 1) > mkt[[i]]$AskPrice && tail(t.band[[i]]$omega, 1) < mkt[[i]]$BidPrice) FALSE else TRUE else FALSE
  })
  
  for(i in 1:nrow(fx.list))
  {
    if(!fx.lock[[i]])
    {
      if(filled[i]) 
      {
        #         index <- which(twsPortfolioValue(reqAccountUpdates(tws))$local == paste(cct[[i]]$symbol,".",cct[[i]]$currency, sep=""))
        #         if(twsPortfolioValue(reqAccountUpdates(tws))$position[index] == 0) 
        #         {
        #           filled[i] <- 0
        #           cross[i] <- FALSE
        #         } 
        
        if(filled[i] > 0 && !cross[i] && mkt[[i]]$AskPrice > tail(t.band[[i]]$gamma, 1)+(2*tail(t.band[[i]]$g_sigma, 1))) cross[i] <- TRUE
        if(filled[i] < 0 && !cross[i] && mkt[[i]]$BidPrice < tail(t.band[[i]]$omega, 1)-(2*tail(t.band[[i]]$o_sigma, 1))) cross[i] <- TRUE
        
        if(filled[i] > 0 && cross && mkt[[i]]$AskPrice <= tail(t.band[[i]]$gamma, 1)) 
        {
          placeOrder(tws, cct[[i]], Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="LMT", lmtPrice=mkt[[i]]$AskPrice, tif="GTC", ocaGroup=oca))
          filled[i] <- 0
          cross[i] <- FALSE
        }
        
        if(filled[i] < 0 && cross && mkt[[i]]$BidPrice >= tail(t.band[[i]]$omega, 1)) placeOrder(tws, cct[[i]], Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="LMT", lmtPrice=mkt[[i]]$BidPrice, tif="GTC", ocaGroup=oca))
{
          filled[i] <- 0
          cross[i] <- FALSE
        }

if(filled[i] > 0 && !cross && mkt[[i]]$AskPrice < tail(t.band[[i]]$gamma, 1)-(2*tail(t.band[[i]]$g_sigma, 1))) 
{
  placeOrder(tws, cct[[i]], Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="LMT", lmtPrice=mkt[[i]]$AskPrice, tif="GTC", ocaGroup=oca))
  filled[i] <- 0
  cross[i] <- FALSE
}

if(filled[i] < 0 && !cross && mkt[[i]]$BidPrice > tail(t.band[[i]]$omega, 1)+(2*tail(t.band[[i]]$o_sigma, 1))) 
{
  placeOrder(tws, cct[[i]], Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="LMT", lmtPrice=mkt[[i]]$BidPrice, tif="GTC", ocaGroup=oca))
  filled[i] <- 0
  cross[i] <- FALSE
}

      }

if(mkt[[i]]$AskPrice > tail(t.band[[i]]$gamma, 1) && !filled[i])
{
  oca <- format(Sys.time(), "%Y%m%d %X"); filled[i] <- mkt[[i]]$AskPrice
  cct <- lapply(1:nrow(fx.list), function(i){ContractDetails(fx.list[i,1],fx.list[i,2],"currency", tws)})
  
  placeOrder(tws, cct[[i]], Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="LMT", lmtPrice=filled[i]))  
  
  index <- which(twsPortfolioValue(reqAccountUpdates(tws))$local == paste(cct[[i]]$symbol,".",cct[[i]]$currency, sep=""))     
}   

if(mkt[[i]]$BidPrice < tail(t.band[[i]]$omega, 1) && !filled[i])
{
  oca <- format(Sys.time(), "%Y%m%d %X"); filled[i] <- -mkt[[i]]$BidPrice 
  cct <- lapply(1:nrow(fx.list), function(i){ContractDetails(fx.list[i,1],fx.list[i,2],"currency", tws)})
  
  placeOrder(tws, cct[[i]], Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="LMT", lmtPrice=abs(filled[i])))  
  
  index <- which(twsPortfolioValue(reqAccountUpdates(tws))$local == paste(cct[[i]]$symbol,".",cct[[i]]$currency, sep=""))
} 
    }
  }

portfolio <- twsPortfolioValue(reqAccountUpdates(tws))
if(!is.null(portfolio)){write.table(portfolio, port.dir, sep=",", append=FALSE, row.names=FALSE, col.names=TRUE, fileEncoding="UTF-8")}	

png(filename=img.dir, width=750, height=996)
par(mfrow=c(3,1))
for(i in 1:nrow(fx.list))
{
  tr <- t.band[[i]]
  tr[nrow(tr),1] <- (mkt[[i]]$AskPrice+mkt[[i]]$BidPrice)/2
  par(mfrow=c(1,1))
  myTheme <- chart_theme()
  myTheme$col$up.col <- "#97F55A"
  plot(chart_Series(OHLC(tr), type ="candlesticks", on=1, theme=myTheme, name=paste(fx.list[i,1],"-",fx.list[i,2], sep=""), 
                    TA='add_TA(tr$gamma,col="#6495ED", on=1, lwd=1);
                    add_TA((2*tr$g_sigma)+tr$gamma,col="pink", on=1, lwd=1);
                    add_TA(tr$gamma-(2*tr$g_sigma),col="pink", on=1, lwd=1);
                    add_TA(tr$omega, col="#FF4040", on=1,lwd=1.5);
                    add_TA((2*tr$o_sigma)+tr$omega,col="pink", on=1, lwd=1);
                    add_TA(tr$omega-(2*tr$o_sigma),col="pink", on=1, lwd=1);
                    if(filled[i] > 0) add_TA(cbind(tr, rep(filled[i], nrow(tr)))[,ncol(tr)+1], on=1, col="#97F55A55", lwd=5, lty=3);
                    if(filled[i] < 0) add_TA(cbind(tr, rep(abs(filled[i]), nrow(tr)))[,ncol(tr)+1], on=1, col="#FF404055", lwd=5, lty=3);
                    add_TA((tr$gamma+tr$omega)/2, on=1,col="#A2CD5A", lwd=1, lty=3);'))
  if(fx.lock[[i]]) mtext("LOCKED", side=4, col="red", line=0.2, cex=0.8)
}
dev.off() 

end.time <- Sys.time()
diff.time <- 5-(end.time-start.time)
Sys.sleep(ifelse(diff.time > 0, diff.time, 0))
}

cat("**************************************************** \n")
cat("Application shutdown initiated... \n")
twsDisconnect(tws)
save.image("sessionData.RData")
cat("Shutdown successfull! \n")
cat("**************************************************** \n")

#sink()
quit(save="no")