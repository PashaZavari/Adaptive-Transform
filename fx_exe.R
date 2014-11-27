setwd("/Library/WebServer/CGI-Executables")

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
ccy <- lapply(1:nrow(fx.list), function(i){ContractDetails(fx.list[i,1],fx.list[i,2],"forex", tws)})
cct <- lapply(1:nrow(fx.list), function(i){ContractDetails(fx.list[i,1],fx.list[i,2],"currency", tws)})
init.cap <- as.real(reqAccountUpdates(tws)[[1]]$NetLiquidation[1]); oca <- 0; img.dir <- "/Library/WebServer/Documents/PriceTape.png"; filled <- FALSE; #double.slap <- FALSE


while(TRUE)
{
  if(format(Sys.time(), "%A") == "Friday" && format(Sys.time(), "%H:%M") >= "14:00")
  {
    cat("Market closure for weekend/holiday(s). \n")
    break	
  }
  
  #if(!filled){Sys.sleep(5)} else{
  #Sys.sleep(4)
  #filled <- FALSE
  #}
  
  while(TRUE)
  {
    tried <- try(sec <- lapply(i:nrow(fx.list), function(i){Op(reqHistoricalData(tws, ccy[[i]], barSize='1 hour', duration='2 W', whatToShow="MIDPOINT"))}), silent = TRUE)
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
  
  if(format(index(tail(sec, n=1)), "%Y-%m-%d %H:%M") == format(Sys.time(), "%Y-%m-%d %H:%M"))
  {
    stack <- nrow(sec); n <- 50; p <- 10
    
    df <- matrix(data=unlist(lapply(2:1, function(x){
      sub <-(sec[(stack-n):stack-x-1,])  
      
      sigma <- cov(cbind(Op(sub), Cl(sub), Hi(sub), Lo(sub)))
      mu <- rbind(mean(Op(sub)), mean(Cl(sub)), mean(Hi(sub)), mean(Lo(sub)))  
      
      ((2^(4/2))/(2^4))*det(denman.beavers(sigma)$sqrt.inv)*exp((-sqrt(2)/(pi/2))*t(rbind(Op(sub)[n+1], Cl(sub)[n+1], Hi(sub)[n+1], Lo(sub)[n+1])-mu)%*%denman.beavers(sigma)$sqrt.inv)  
      
    })), ncol=4, byrow=TRUE)
    
    sig.level <- ifelse(abs(diff(log(df[,3]))) > 0.65, 1,0)
    fcst <- median(pred.range(as.matrix(Lo(sec)[(stack-p-1):(stack-1)]), p+1))
    sig.primary <-ifelse(fcst > head(tail(as.matrix(Lo(sec)), 2), 1), 0, 1)
    
    open <- tail(Op(sec), n=1)
    
    lot.size <- as.real(reqAccountUpdates(tws)[[1]]$NetLiquidation[1])*20
    
    if(sig.primary*sig.level == 1)
    {
      #if(double.slap)
      #{
      #double.slap <- FALSE
      #}
      
      #else
      #{
      oca <- format(Sys.time(), "%Y%m%d %X"); filled <- TRUE; stp <- open - 0.3; lmt <- open + 0.3
      placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="MKT"))	
      
      cat("BUY SIGNAL @ ",open," on ",format(Sys.time(), "%Y %a %b %d %X"),"\n")
      cat("********************************************************************* \n")		
      
      if(format(index(tail(sec, n=1)), "%H:%M:%S") == "14:15:00")
      {
        placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="LMT", lmtPrice=round(as.real(lmt), digits=3), tif="GTD", goodTillDate=format(Sys.time()+2695, "%Y%m%d %X"), ocaGroup=oca))
        
        #placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="STP", auxPrice=round(as.real(stp), digits=3), tif="GTD", goodTillDate=format(Sys.time()+2695, "%Y%m%d %X"), ocaGroup=oca))
        
        placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="MKT", tif="GAT", goodAfterTime=format(Sys.time()+2695, "%Y%m%d %X"), ocaGroup=oca))				
      }
      
      else
      {
        placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="LMT", lmtPrice=round(as.real(lmt), digits=3), tif="GTD", goodTillDate=format(Sys.time()+3595, "%Y%m%d %X"), ocaGroup=oca))
        
        #placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="STP", auxPrice=round(as.real(stp), digits=3), tif="GTD", goodTillDate=format(Sys.time()+3595, "%Y%m%d %X"), ocaGroup=oca))
        
        placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="MKT", tif="GAT", goodAfterTime=format(Sys.time()+3595, "%Y%m%d %X"), ocaGroup=oca))					
      }		
      
      send.mail(from = "pashazavari@gmail.com",
                to = "pashazavari@gmail.com",
                subject = "FOREXBot Trade Activity Notification",
                body = paste("BUY SIGNAL @ ",open," on ",format(Sys.time(), "%Y %a %b %d %X"),"@ http://24.87.162.133/Forex_bot.html"),
                smtp = list(host.name = "smtp.gmail.com", port = 587, user.name = "pashazavari", passwd = "vancouverthedawg", ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
      
      if(format(index(tail(sec, n=1)), "%H:%M:%S") == "14:15:00"){Sys.sleep(2695)} else{Sys.sleep(3595)}
      
      Sys.sleep(5)
      init.cap <- as.real(reqAccountUpdates(tws)[[1]]$NetLiquidation[1])
      
      #double.slap <- TRUE
      
      cat("AVAILABLE FUNDS: $", as.real(init.cap),"\n")
      cat("********************************************************************* \n")	        
      #}
    }
    
    if(sig.primary*sig.level == -1)
    {
      #if(double.slap)
      #{
      #double.slap <- FALSE
      #}
      
      #else
      #{
      oca <- format(Sys.time(), "%Y%m%d %X"); filled <- TRUE; stp <- open + 0.3; lmt <- open - 0.3
      placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="SELL", totalQuantity=as.integer(lot.size),  orderType="MKT"))	
      
      cat("SELL SIGNAL @ ",open," on ",format(Sys.time(), "%Y %a %b %d %X"),"\n")
      cat("********************************************************************* \n")		
      
      if(format(index(tail(sec, n=1)), "%H:%M:%S") == "14:15:00")
      {
        placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="LMT", lmtPrice=round(as.real(lmt), digits=3), tif="GTD", goodTillDate=format(Sys.time()+2695, "%Y%m%d %X"), ocaGroup=oca))
        
        #placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="STP", auxPrice=round(as.real(stp), digits=3), tif="GTD", goodTillDate=format(Sys.time()+2695, "%Y%m%d %X"), ocaGroup=oca))
        
        placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="MKT", tif="GAT", goodAfterTime=format(Sys.time()+2695, "%Y%m%d %X"), ocaGroup=oca))				
      }
      
      else
      {
        placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="LMT", lmtPrice=round(as.real(lmt), digits=3), tif="GTD", goodTillDate=format(Sys.time()+3595, "%Y%m%d %X"), ocaGroup=oca))
        
        #placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="STP", auxPrice=round(as.real(stp), digits=3), tif="GTD", goodTillDate=format(Sys.time()+3595, "%Y%m%d %X"), ocaGroup=oca))
        
        placeOrder(tws, cct, Order=twsOrder(reqIds(tws), action="BUY", totalQuantity=as.integer(lot.size),  orderType="MKT", tif="GAT", goodAfterTime=format(Sys.time()+3595, "%Y%m%d %X"), ocaGroup=oca))					
      }		
      
      send.mail(from = "pashazavari@gmail.com",
                to = "pashazavari@gmail.com",
                subject = "FOREXBot Trade Activity Notification",
                body = paste("SELL SIGNAL @ ",open," on ",format(Sys.time(), "%Y %a %b %d %X"),"@ http://24.87.162.133/Forex_bot.html"),
                smtp = list(host.name = "smtp.gmail.com", port = 587, user.name = "pashazavari", passwd = "vancouverthedawg", ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
      
      if(format(index(tail(sec, n=1)), "%H:%M:%S") == "14:15:00"){Sys.sleep(2695)} else{Sys.sleep(3595)}
      
      Sys.sleep(5)
      init.cap <- as.real(reqAccountUpdates(tws)[[1]]$NetLiquidation[1])
      
      #double.slap <- TRUE
      
      cat("AVAILABLE FUNDS: $", as.real(init.cap),"\n")
      cat("********************************************************************* \n")	       
      #}
    }
    
    write.table(init.cap, "eq.curve.csv", sep=",", append=TRUE, row.names=FALSE, col.names=FALSE, fileEncoding="UTF-8")
    eq.curve <- as.matrix(read.csv("eq.curve.csv", header=FALSE)) 
    
    png(filename=img.dir, width=750, height=500)
    par(bg="#EEEEEE")
    plot(eq.curve, type="l", pch=22, axes=FALSE, ann=FALSE, col="#FF5900", lwd=2)
    title(main=format(Sys.time(), "%Y %a %b %d %X"), font.main=1, col="black")
    box(col="white", lwd=5)
    axis(side=2, las=1, cex.axis=0.9, cex.main=0.9, lwd=0.9, col="white")
    mtext("USD.JPY@IDEALPRO - Hourly Profits", side=1, line=3, col="black", cex=1)
    grid(col="white", lty=3, lwd=2)
    dev.off()	
    
    if(format(index(tail(sec, n=1)), "%H:%M:%S") == "13:00:00" && !filled)
    {
      cat("[Position flat: waiting for next opening tick] \n")
      Sys.sleep(4500 - update.time(format(index(tail(sec, n=1)), "%Y-%m-%d %H:%M:%S")))
    } 
    
    if(format(index(tail(sec, n=1)), "%H:%M:%S") == "14:15:00" && !filled)
    {
      cat("[Position flat: waiting for next opening tick] \n")
      Sys.sleep(2700 - update.time(format(index(tail(sec, n=1)), "%Y-%m-%d %H:%M:%S")))
    }
    
    if(format(index(tail(sec, n=1)), "%H:%M:%S") != "13:00:00" && format(index(tail(sec, n=1)), "%H:%M:%S") != "14:15:00" && !filled)
    {
      cat("[Position flat: waiting for next opening tick] \n")
      Sys.sleep(3600 - update.time(format(index(tail(sec, n=1)), "%Y-%m-%d %H:%M:%S")))
    }
  }
  
  else
  {
    oca <- format(Sys.time(), "%Y%m%d %X")
    reset.pointer <-  3600 - update.time(format(index(tail(sec, n=1)), "%Y-%m-%d %H:%M:%S"))
    
    if(format(index(tail(sec, n=1)), "%H:%M:%S") == "13:00:00")
    {
      reset.pointer <-  4500 - update.time(format(index(tail(sec, n=1)), "%Y-%m-%d %H:%M:%S"))
    } 
    
    if(format(index(tail(sec, n=1)), "%H:%M:%S") == "14:15:00")
    {
      reset.pointer <-  2700 - update.time(format(index(tail(sec, n=1)), "%Y-%m-%d %H:%M:%S"))
    }
    
    if(format(index(tail(sec, n=1)), "%H:%M:%S") != "13:00:00" && format(index(tail(sec, n=1)), "%H:%M:%S") != "14:15:00")
    {
      reset.pointer <- 3600 - update.time(format(index(tail(sec, n=1)), "%Y-%m-%d %H:%M:%S"))
    }
    
    cat("[Pointer misalignment detected] \n")
    
    cat("Estimated correction phase: ",reset.pointer/60," minute(s)... \n")
    
    if(format(index(tail(sec, n=1)), "%H:%M:%S") == "13:00:00")
    {
      Sys.sleep(4500 - update.time(format(index(tail(sec, n=1)), "%Y-%m-%d %H:%M:%S")))
    } 
    
    if(format(index(tail(sec, n=1)), "%H:%M:%S") == "14:15:00")
    {
      Sys.sleep(2700 - update.time(format(index(tail(sec, n=1)), "%Y-%m-%d %H:%M:%S")))
    }
    
    if(format(index(tail(sec, n=1)), "%H:%M:%S") != "13:00:00" && format(index(tail(sec, n=1)), "%H:%M:%S") != "14:15:00")
    {
      Sys.sleep(3600 - update.time(format(index(tail(sec, n=1)), "%Y-%m-%d %H:%M:%S")))
    }
  }	
}

cat("**************************************************** \n")
cat("Application shutdown initiated... \n")
twsDisconnect(tws)
cat("Shutdown successfull! \n")
cat("**************************************************** \n")

sink()
quit(save="no")