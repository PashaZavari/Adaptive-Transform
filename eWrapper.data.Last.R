isConnection <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE) 
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage[-grep("127.0.0.1", ipmessage)]))
}

ContractDetails <- function(a, b, type, con){	
	if(isConnection() && isConnected(con)) {
		if(type == "forex"){return(reqContractDetails(tws, twsCurrency(a,b))[[1]]$contract)}
		else if(type == "stock"){return(lapply(a, twsSTK))}
		else if(type == "currency"){return(twsCurrency(a,b))}
		else if(type == "equity"){return(twsEquity(a))}
	} else{	
		while(isConnection() == FALSE || isConnected(con) == FALSE)
		{	
			cat("Network connection interupted... \n")		
			Sys.sleep(30)
		}
		
		Sys.sleep(30)
		cat("[Network connection re-established] \n")	
		if(type == "forex"){return(reqContractDetails(tws, twsCurrency(a,b))[[1]]$contract)}
		else if(type == "stock"){return(lapply(a, twsSTK))}
		else if(type == "currency"){return(twsCurrency(a,b))}
		else if(type == "equity"){return(twsEquity(a))}
	}
}

eWrapper.data.Mkt <- function(n) {
  eW <- eWrapper(NULL)  # use basic template
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_,3),nc=3),0),
                                      .Dimnames=list(NULL,c("Last","Open","LastSize")))),n))

  eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) 
  {
    tickType = msg[3]
    msg <- as.numeric(msg)
    id <- msg[2]
    data <- eW$get.Data("data") 
    attr(data[[id]],"index") <- as.numeric(Sys.time())
    nr.data <- NROW(data[[id]])
    if(tickType == .twsTickType$LAST) {
      data[[id]][nr.data,1] <- msg[4]
    }
    else if(tickType == .twsTickType$OPEN) { 
      data[[id]][nr.data,2] <- msg[4] 
    }
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  eW$tickSize  <- function(curMsg, msg, timestamp, file, ...) 
  { 
    data <- eW$get.Data("data")
    tickType = msg[3]
    msg <- as.numeric(msg)
    id <- as.numeric(msg[2])
    attr(data[[id]],"index") <- as.numeric(Sys.time())
    nr.data <- NROW(data[[id]])
    if(tickType == .twsTickType$LAST_SIZE) {
      data[[id]][nr.data,3] <- msg[4]
    } 
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  return(eW)
}


eWrapper.data.RealTimeBars <- function(n) {
  eW <- eWrapper(NULL)  # use basic template
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_,3),nc=3),0),
                                      .Dimnames=list(NULL,c("High","Low","Close")))),n))

  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...) 
  {
    msg <- as.numeric(msg)
    id <- msg[2]
    file <- file[[id]]
    data <- eW$get.Data("data") 
    attr(data[[id]],"index") <- as.numeric(msg[3])
    nr.data <- NROW(data[[id]])
    cat(paste(msg[4], msg[5], msg[6], sep = ","), "\n", file = "fx.bids.txt", append = TRUE)
	data[[id]][nr.data,2] <- msg[4]
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  return(eW)
}


eWrapper.data.Last<- function (n) 
{		
    eW <- eWrapper(NULL)
    eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 
        4), nc = 4), 0), .Dimnames = list(NULL, c("BidSize", 
        "BidPrice", "AskPrice", "AskSize")))), n))
    eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) {
        tickType = msg[3]
        msg <- as.numeric(msg)
        id <- msg[2]
        data <- eW$get.Data("data")
        attr(data[[id]], "index") <- as.numeric(Sys.time())
        nr.data <- NROW(data[[id]])
        if (tickType == .twsTickType$BID) {
            data[[id]][nr.data, 1:2] <- msg[5:4]
        }
        else if (tickType == .twsTickType$ASK) {
            data[[id]][nr.data, 3:4] <- msg[4:5]
        }
        eW$assign.Data("data", data)
        c(curMsg, msg)
    }
    eW$tickSize <- function(curMsg, msg, timestamp, file, ...) {
        data <- eW$get.Data("data")
        tickType = msg[3]
        msg <- as.numeric(msg)
        id <- as.numeric(msg[2])
        attr(data[[id]], "index") <- as.numeric(Sys.time())
        nr.data <- NROW(data[[id]])
        if (tickType == .twsTickType$BID_SIZE) {
            data[[id]][nr.data, 1] <- msg[4]
        }
        else if (tickType == .twsTickType$ASK_SIZE) {
            data[[id]][nr.data, 4] <- msg[4]
        }
        eW$assign.Data("data", data)
        c(curMsg, msg)
    }
    return(eW)
}

snapShot <- function (twsCon, eWrapper, timestamp, file, playback = 1, ...)
{
   if (missing(eWrapper))
       eWrapper <- eWrapper()
   names(eWrapper$.Data$data) <- eWrapper$.Data$symbols
   con <- twsCon[[1]]
   if (inherits(twsCon, "twsPlayback")) {
       sys.time <- NULL
       while (TRUE) {
           if (!is.null(timestamp)) {
               last.time <- sys.time
               sys.time <- as.POSIXct(strptime(paste(readBin(con,
                 character(), 2), collapse = " "), timestamp))
               if (!is.null(last.time)) {
                 Sys.sleep((sys.time - last.time) * playback)
               }
               curMsg <- .Internal(readBin(con, "character",
                 1L, NA_integer_, TRUE, FALSE))
               if (length(curMsg) < 1)
                 next
               processMsg(curMsg, con, eWrapper, format(sys.time,
                 timestamp), file, ...)
           }
           else {
               curMsg <- readBin(con, character(), 1)
               if (length(curMsg) < 1)
                 next
               processMsg(curMsg, con, eWrapper, timestamp,
                 file, ...)
               if (curMsg == .twsIncomingMSG$REAL_TIME_BARS)
                 Sys.sleep(5 * playback)
           }
       }
   }
   else {
       while (TRUE) {
           socketSelect(list(con), FALSE, NULL)
           curMsg <- .Internal(readBin(con, "character", 1L,
               NA_integer_, TRUE, FALSE))
           if (!is.null(timestamp)) {
               processMsg(curMsg, con, eWrapper, format(Sys.time(),
                 timestamp), file, ...)
           }
           else {
               processMsg(curMsg, con, eWrapper, timestamp,
                 file, ...)
           }
           if (!any(sapply(eWrapper$.Data$data, is.na)))
               return(do.call(rbind, lapply(eWrapper$.Data$data,
                 as.data.frame)))
       }
   }
}

function (orderId, action = "BUY", totalQuantity = "10", orderType = "LMT", 
          lmtPrice = "0.0", auxPrice = "0.0", tif = "", outsideRTH = "0", 
          openClose = "O", origin = .twsOrderID$CUSTOMER, ocaGroup = "", 
          account = "", orderRef = "", transmit = TRUE, parentId = "0", 
          blockOrder = "0", sweepToFill = "0", displaySize = "0", triggerMethod = "0", 
          hidden = "0", discretionaryAmt = "0.0", goodAfterTime = "", 
          goodTillDate = "", faGroup = "", faMethod = "", faPercentage = "", 
          faProfile = "", shortSaleSlot = "0", designatedLocation = .twsOrderID$EMPTY_STR, 
          ocaType = "0", rule80A = "", settlingFirm = "", clearingAccount = "", 
          clearingIntent = "", allOrNone = "0", minQty = "", percentOffset = "", 
          eTradeOnly = "0", firmQuoteOnly = "0", nbboPriceCap = "", 
          auctionStrategy = "0", startingPrice = "", stockRefPrice = "", 
          delta = "", stockRangeLower = "", stockRangeUpper = "", overridePercentageConstraints = "0", 
          volatility = "", volatilityType = "", deltaNeutralOrderType = "", 
          deltaNeutralAuxPrice = "", continuousUpdate = "0", referencePriceType = "", 
          trailStopPrice = "", basisPoints = "", basisPointsType = "", 
          scaleInitLevelSize = "", scaleSubsLevelSize = "", scalePriceIncrement = "", 
          notHeld = FALSE, algoStrategy = "", algoParams = NULL, whatIf = FALSE, 
          clientId = "", permId = "") 
{
  if (missing(orderId)) 
    orderId <- ""
  structure(list(orderId = orderId, clientId = clientId, permId = permId, 
                 action = action, totalQuantity = as.character(as.numeric(totalQuantity)), 
                 orderType = orderType, lmtPrice = as.character(lmtPrice), 
                 auxPrice = as.character(auxPrice), tif = tif, ocaGroup = ocaGroup, 
                 ocaType = ocaType, orderRef = orderRef, transmit = as.character(as.integer(transmit)), 
                 parentId = parentId, blockOrder = blockOrder, sweepToFill = sweepToFill, 
                 displaySize = displaySize, triggerMethod = triggerMethod, 
                 outsideRTH = outsideRTH, hidden = hidden, goodAfterTime = goodAfterTime, 
                 goodTillDate = goodTillDate, overridePercentageConstraints = overridePercentageConstraints, 
                 rule80A = rule80A, allOrNone = allOrNone, minQty = minQty, 
                 percentOffset = percentOffset, trailStopPrice = trailStopPrice, 
                 faGroup = faGroup, faProfile = faProfile, faMethod = faMethod, 
                 faPercentage = faPercentage, openClose = openClose, origin = origin, 
                 shortSaleSlot = shortSaleSlot, designatedLocation = designatedLocation, 
                 discretionaryAmt = discretionaryAmt, eTradeOnly = eTradeOnly, 
                 firmQuoteOnly = firmQuoteOnly, nbboPriceCap = nbboPriceCap, 
                 auctionStrategy = auctionStrategy, startingPrice = startingPrice, 
                 stockRefPrice = stockRefPrice, delta = delta, stockRangeLower = stockRangeLower, 
                 stockRangeUpper = stockRangeUpper, volatility = volatility, 
                 volatilityType = volatilityType, continuousUpdate = continuousUpdate, 
                 referencePriceType = referencePriceType, deltaNeutralOrderType = deltaNeutralOrderType, 
                 deltaNeutralAuxPrice = deltaNeutralAuxPrice, basisPoints = basisPoints, 
                 basisPointsType = basisPointsType, scaleInitLevelSize = scaleInitLevelSize, 
                 scaleSubsLevelSize = scaleSubsLevelSize, scalePriceIncrement = scalePriceIncrement, 
                 account = account, settlingFirm = settlingFirm, clearingAccount = clearingAccount, 
                 clearingIntent = clearingIntent, algoStrategy = algoStrategy, 
                 algoParams = algoParams, whatIf = as.character(as.integer(whatIf)), 
                 notHeld = as.character(as.integer(notHeld))), class = "twsOrder")
  
  Sys.sleep(1)
  
  cat(action," SIGNAL for ",toString(fx.list[i,])," @ ",as.numeric(filled[i])," on ",format(Sys.time(), "%Y %a %b %d %X"),"\n")
  cat("********************************************************************* \n")  
  
  #init.cap <- as.real(reqAccountUpdates(tws)[[1]]$NetLiquidation[1])
  
  cat("AVAILABLE FUNDS: $", as.numeric(init.cap),"\n")
  cat("********************************************************************* \n")  
}
