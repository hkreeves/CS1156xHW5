##
## Logistic Regression using Stochastic Gradient Descent
## EdX - CaltechX CS1156x Learning From Data
##
## HW 5
##
## Author: Kai He
##

## Need getLine(), getLineData(), getClass(), PlotSim(), eval() from PLA.R code
source("F:/Google Drive/Study Note/edX-Machine Learning/hw/CS1156xHW1/PLA.R")

## tolerance and learning rate
tol <- 0.01
eta <- 0.01

## logistic regression using SGD
logReg <- function(trainX, trainY)
{
	trainX <- cbind(1, trainX)

	## initialize weight
	new.w <- c(0, 0, 0)

	for(iter in 1:5000)
	{
		old.epoch.w <- new.w
		permutation <- sample(length(trainY))
		for(pt in permutation)
		{
			old.w <- new.w
			## compute gradient at the selected point, then update w
			grad <- -trainY[pt]*trainX[pt,]/(1+exp(trainY[pt]*
					(trainX[pt,] %*% old.w)))
			new.w <- old.w - eta*grad
		}

		# compute error function (cross entropy/negative log likelihood)
		err <- mean(log(1+exp(-trainY*(trainX%*%old.w))))
		# print(err)

		## check exit condition
		if(sqrt(sum((new.w-old.epoch.w)^2)) < tol)
		{
			print(iter)
			return(new.w)
		}
	}
	# if not meet tolerance requirement, terminate
	print(iter)
	return(new.w)
}	

## cross entropy evaluation
evalCE <- function(g, f, N=1000)
{
	X <- matrix(runif(2*N, -1, 1), N, 2)
	Y <- getClass(X, f)
	X <- cbind(1, X)	
	mean(log(1+exp(-Y*(X %*% g))))
}	 

## set up simulation.
testLogReg <- function(N, is.in=T, plot=F){
	# initialize data and f line
	sampleX <- matrix(runif(2*N, -1, 1), N, 2)
	f <- getLine()
	sampleY <- getClass(sampleX, f)

	# get logistic regression result, g
	g <- logReg(sampleX, sampleY)
	
	# predict
	# predY <- getClass(sampleX, g)

	if(is.in){
		# evaluate in-sample error
		error <- mean(log(1+exp(-sampleY*(cbind(1,sampleX) %*% g))))
	}
	else{
		# evaluate out-of-sample error using Monte-Carlo method
		error <- evalCE(g, f)
	}
	
	if(plot){
		plotSim(sampleX, sampleY, g)
		print(rbind(g,f))
	}
	return(error)
}

iterLogReg <- function(N, is.in=T, iter){
	errorSet <- vector()
	for( i in 1:iter ){
		error <- testLogReg(N, is.in)
		errorSet <- c(error, errorSet)
	}
	
	mean(errorSet)
}
	
