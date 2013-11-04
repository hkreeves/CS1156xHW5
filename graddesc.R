errfunc <- function(u, v)
{
	(u*exp(v) - 2*v*exp(-u))^2
}

rate <- 0.1
u <- 1
v <- 1
tol <- 1e-14
new.err <- errfunc(u, v)

## gradient descent 
for(i in 1:500)
{
	old.err <- new.err
	derv1 <- 2*(u*exp(v)-2*v*exp(-u))*(exp(v)+2*v*exp(-u))
	derv2 <- 2*(u*exp(v)-2*v*exp(-u))*(u*exp(v)-2*exp(-u))

	## update parameters
	u <- u - rate*derv1
	v <- v - rate*derv2
	
	## evaluate new error function
	new.err <- errfunc(u, v)
	print(c(old.err, new.err))
	if(abs(new.err - old.err) < tol) break
}
i
c(u,v)

## 2-step descent. 
## for each iteration, fixed v and descent u first, then the other way.

u <- 1
v <- 1
new.err <- errfunc(u, v)
for(i in 1:15)
{
	old.err <- new.err
	derv1 <- 2*(u*exp(v)-2*v*exp(-u))*(exp(v)+2*v*exp(-u))
	u <- u - rate*derv1
	int.err <- errfunc(u, v)

	derv2 <- 2*(u*exp(v)-2*v*exp(-u))*(u*exp(v)-2*exp(-u))	
	v <- v - rate*derv2

	new.err <- errfunc(u, v)

	print(c(old.err, int.err, new.err))
	#if(abs(new.err - old.err) < tol) break
}