fitarima <- function (X, d, LOG, n, s, size, f.prd)
{	
	if(LOG)
	{		
		dClose <- diff(log(abs(X)+1), lag=d)
	}	
	
	else
	{
		dClose <- diff(X, lag=d)	
	}
	
	P <- dClose
	
	i <- 1; j <- 1
	
	aicA <- matrix(0,n,n)
	bicA <- matrix(0,n,n)
	
	while(j <= n)
	{	
		fClose <- arima(dClose, order = c(i,s,j))
		bicA[j,i] <- ((-2)*logLik(fClose)) + ((i+j)*log(length(fClose)))
		
		j <- j+1
		
		if(i < n && j > n)	
		{
			i <- i+1
			j <- 1	
		}
		
	}
  
	imbic <- which(bicA==min(bicA), arr.in=TRUE)
		
	fClose <- arima(dClose, order = c(imbic[1,1],s,imbic[1,2]))
	pClose <- predict(fClose, n.ahead=f.prd)
	
	return(pClose$pred[f.prd] + X[size-d])
}

ft <- function(X, r=2*pi, itr=1)
{
  r <- round(1/r, digits=3)
  N <- length(X)
  seq_N <- r*pi*seq(1:N)
  
  Tr <- as.matrix(cbind(matrix(1, N), matrix(cos(seq_N), N), matrix(sin(seq_N), N)))  
  
  stmFt <- solve(t(Tr) %*% Tr) %*% t(Tr) %*% X
  
  Tr <- (1 + exp(-(as.matrix(cbind(matrix(1, N), matrix(cos(seq_N), N), matrix(sin(seq_N), N))))))
  
  hess <- 2*(t(Tr) %*% Tr)
  
  for(i in 1:itr)
  {
    grad <- 2*((t(Tr) %*% Tr)%*%stmFt - t(t(X)%*%Tr))
    if(all(eigen(hess)$values > 0)) stmFt <- stmFt - (solve(hess)%*%grad) else break;
  }

  #return(mean(X) + stmFt[2]*cos(pi*N) + stmFt[3]*sin(pi*N))
  return(stmFt[2]*cos(pi*N) + stmFt[3]*sin(pi*N)) 
}

cmLogic <- function(X, cntrs=2, itr=10, mass=2)
{
	set.seed(1)

	x <- as.matrix(cbind(index(as.integer(X)),as.vector(X)))
	xrows <- nrow(x)
	
	centers <- x[sample(1:xrows, cntrs), , drop = FALSE]
	ncenters <- nrow(centers)
	sum <- 0; k_sum <- 0; eps <- 0.01; membership <- matrix(0, xrows, ncenters)
	
	for(eps in 1:itr)
	{  
	  for(i in 1:xrows)
	  {
	    for(j in 1:ncenters)
	    {
	      for(k in 1:ncenters)
	      {
	        sum <- sum + (dist(x[i,]-centers[j,])/dist(x[i,]-centers[k,]))^(2/(mass-1))
	      }
        
	      membership[i,j] <- 1/sum
	    }
	    sum <- 0
	  }
	  
	  membership[is.nan(membership)] <- 0
	  centers <- matrix(0, ncenters, 2)
	  for(j in 1:ncenters)
	  {
	    centers[j,] <- cbind(sum((x*membership[,j])[,1]),sum((x*membership[,j])[,2]))/sum(membership[,j])
	  }
    
	  if(max(abs(membership - k_sum)) > eps || itr == 1){k_sum <- membership} else break
	}
  
	return(mean(centers[which(membership[xrows,]==min(membership[xrows,]), arr.ind=TRUE),2]))
}

hw <- function(X, d=1, LOG=1, s=1)
{
  inputSize <- length(X)
  
	if(LOG)
	{		
		dClose <- diff(log(abs(X)+1), lag=d)
	}	
	
	else
	{
		dClose <- diff(X, lag=d)	
	}
	
	#dClose <- ts(dClose[,1],start=1,freq=7)	
	#hwfClose <- HoltWinters(dClose)	
	
	alpha <- 0.3
	beta <- 0.1
	gamma <- 0.1
	
	L_s <- sum(X)/inputSize
	B_s <- ((sum(X[1:(inputSize/2)])/(inputSize/2)) - (sum(X[((inputSize/2)+1):(inputSize)])/(inputSize/2)))/(inputSize/2)
	S_k <- (X[inputSize-s] - (((s-1)*B_s)/2))/L_s
	
	L_t <- (alpha*(X[inputSize]/S_k)) + ((1-alpha)*(L_s - B_s))
	B_t <- (beta*(L_t - L_s)) + ((1-beta)*B_s)
	S_t <- (gamma*(X[inputSize]/L_t)) + ((1-gamma)*S_k) 
	
	phwClose <- (L_t + B_t)*S_t
	
	return(phwClose)
}

nearest <- function (X, n, k) 
{ 
    N <- nrow(X) 
    inds <- c(n); i <- 0 
    
    while (i < k) 
    { 
        j <- as.integer(knn1(X [-inds, ], X[n, ], 1:(N-length(inds)))) 
        inds <- c(inds, setdiff(1:N, inds)[j]) 
        i <- i+1 
    } 
    
    return(inds[-1]) 
}

pnn <- function (Y, X)
{
	i <- 0; aClose <- 0; k <- 0
	k <- length(Y)
	
	while(i < k)
	{
		aClose <- aClose + X[Y[i+1]]
		i <- i+1
	}
	
	aClose <- aClose * (1/(k-1))
	return(aClose)
}

denman.beavers <- function(mat,maxit=50) 
{
  stopifnot(nrow(mat) == ncol(mat))
  niter <- 0
  y <- mat
  z <- diag(rep(1,nrow(mat)))
  
  for (niter in 1:maxit) 
  {
    y.temp <- 0.5*(y+solve(z))
    z <- 0.5*(z+solve(y))
    y <- y.temp
  }
  
  return(list(sqrt=y,sqrt.inv=z))
}

rollFun <- function(x, n, FUN, ...)
{ 
  x <- as.vector(x)
  
  start <- 1; end <- length(x)-n+1; m <- x[start:end]
  
  for (i in 2:n) {
    start <- start + 1
    end <- end + 1
    m <- cbind(m, x[start:end])
  }
  
  ans <- apply(m, MARGIN = 1, FUN = FUN, ...)
  
  return(ans)
}

rollVar <- function(x, n = 9, method = NULL, trim = TRUE, unbiased = TRUE, na.rm = FALSE)
{  
  x <- as.vector(x)
  
  if (na.rm) x <- as.vector(na.omit(x))
  
  roll <- rollFun(x = x, n = n, FUN = method)
  
  if (!unbiased) roll <- (roll * (n-1))/n
  if (!trim) roll <- c(rep(NA, (n-1)), roll)
  
  return(roll)
}

lagpad <- function(x, k) {
  c(rep(NA, k), x)[1 : length(x)] 
}

ahead <- function(x, k) {
  c(as.matrix(x[-(1:k)]), rep(NA, k))
}

delchars <- function(str,n,lead=TRUE)
{ 
	dots <- paste(rep(".",n),collapse="") 
	pat <- if(lead)paste("^",dots,sep="") else paste(dots,"$",sep="") 
	sub(pat,"",str) 
} 

count.dec <- function(x) {min(which( x*10^(0:20)==floor(x*10^(0:20)) )) - 1} 

pred.range <- function(X="close", inputSize, arima_n=1, arima_s=1, nn_k=52, ft_itr=1, cm_cntrs=2, cm_itr=100, cm_mass=2, lag=1, log=1, p=1)
{
	p.mat <- matrix(0,5)

	p.mat[1] <- pnn(nearest(X, n=inputSize, k=nn_k), X)
	p.mat[2] <- hw(X, inputSize, d=lag, LOG=log, s=5)
	p.mat[3] <- fitarima(X, d=lag, LOG=log, n=arima_n, s=arima_s, size=inputSize, f.prd=p)
	p.mat[4] <- cmLogic(X, cntrs=cm_cntrs, itr=cm_itr, mass=cm_mass)
	p.mat[5] <- ft(X, r=2*pi, itr=ft_itr)

	return(p.mat)
}