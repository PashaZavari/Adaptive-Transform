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