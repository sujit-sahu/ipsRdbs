#' Illustration of the central limit theorem for sampling from the 
#' uniform distribution
#' 
#' @param  nsize Sample size, n. Its default value is 10. 
#' @param  nrep Number of replications. How many samples of size \code{nsize} 
#' should be taken, default value is 10000.     
#' @return  A vector of means of the replicated samples.  The function also 
#' has the side effect of drawing a histogram of the sample means and 
#' two superimposed density functions: one estimated from the data using 
#' the \code{density} function and the other is the density of the CLT 
#' approximated normal distribution. The better the CLT approximation, the 
#' closer are the two superimposed densities. 
#' @examples
#'  see_the_clt_for_uniform()
#' old.par <- par(no.readonly = TRUE) 
#' par(mfrow=c(2, 3))
#' a1 <- see_the_clt_for_uniform(nsize=1)
#' a2 <- see_the_clt_for_uniform(nsize=2)
#' a3 <- see_the_clt_for_uniform(nsize=5)
#' a4 <- see_the_clt_for_uniform(nsize=10)
#' a5 <- see_the_clt_for_uniform(nsize=20)
#' a6 <- see_the_clt_for_uniform(nsize=50)
#' par(old.par)
#' 
#' @export
see_the_clt_for_uniform <- function(nsize = 10, nrep=10000) {
  ## set number of replicate samples
 
  ## First generate sample*nrep many U(0,1)
  x <- runif(nsize * nrep)
  
  ## Put the random values in a matrix form with nsample rows and nrep columns.
  ## Each column is then a replicate
  y <- matrix(x, ncol=nrep)
  
  ## Now find mean for each column
  
  zbar <- apply(y, 2, mean) ## 2 for columns
  
  ## Would like to overlay a density 
  u <- seq(from=min(zbar), to= max(zbar), length=100)
  v <- dnorm(u, mean=0.5, sd=sqrt(1/(12*nsize)))
  v1 <- density(zbar)
  hist(zbar, nclass=20, prob=TRUE, xlab="sample mean", col="grey", xlim=range(c(min(zbar)-0.05, max(zbar)+0.05)), 
       ylim=range(v, v1$y),  main=paste("Histogram of", nrep, "sample means from samples of size", nsize))
  lines(density(zbar), lty=2, col="darkgreen", lwd=2) # add a density estimate with defaults
  ## Now the normal distribution density
  lines(u, v, lty=1, col="red", lwd=2) 
  symb <- c("data density", "CLT density")
  legend(x=mean(zbar), y=max(v)-0.5, legend = symb, lty = c(2, 1), col = c("darkgreen","red"), bty="n")
  zbar 
  }

#' Illustration of the CLT for samples from the Bernoulli distribution 
#' 
#' @inheritParams see_the_clt_for_uniform
#' @param prob True probability of success for the Bernoulli trials 
#' @examples see_the_clt_for_Bernoulli()
#' old.par <- par(no.readonly = TRUE)
#' par(mfrow=c(2, 3))
#' see_the_clt_for_Bernoulli(nsize=30)
#' see_the_clt_for_Bernoulli(nsize=50)
#' see_the_clt_for_Bernoulli(nsize=100)
#' see_the_clt_for_Bernoulli(nsize=500)
#' see_the_clt_for_Bernoulli(nsize=1000)
#' see_the_clt_for_Bernoulli(nsize=5000)
#' par(old.par)
#' @export
see_the_clt_for_Bernoulli <- function(nsize = 10, nrep=10000, prob=0.8) {
  
  ## set number of replicate samples
  #
  ## First generate sample*nrep many U(0,1)
  ## library(extraDistr)
  x <- extraDistr::rbern(nsize * nrep, prob=prob)
  ## Put the random values in a matrix form with nsample rows and nrep columns.
  ## Each column is then a replicate
  y <- matrix(x, ncol=nrep)
  
  ## Now find mean for each column
  
  zbar <- apply(y, 2, mean) ## 2 for columns
  
  zbar <- (zbar - mean(zbar))/sd(zbar)
  
  ## Would like to overlay a density 
  u <- seq(from=min(zbar), to= max(zbar), length=100)
  # v <- dnorm(u, mean=prob, sd=sqrt((prob * (1-prob)/nsize)))
  v <- dnorm(u)
  v1 <- density(zbar)
  
  
  hist(zbar, prob=TRUE, xlab="sample mean", col="grey", xlim=range(c(min(zbar)-0.05, max(zbar)+0.05)), 
       ylim=range(v, v1$y),  main=paste("Histogram of", nrep, "sample means from samples of size", nsize))
  
  
  # lines(density(zbar), lty=2, col="darkgreen", lwd=2) # add a density estimate with defaults
  ## Now the normal distribution density
  
  lines(u, v, lty=1, col="red", lwd=2) 
  
  # symb <- c("data density", "CLT density")
  #legend(x=mean(zbar), y=max(v)-0.5, legend = symb, lty = c(2, 1), col = c("darkgreen","red"), bty="n")
}
###

#' Illustration of the central limit theorem for sampling from the 
#' uniform distribution
#' @param  nsize Sample size, n. Its default value is 10. 
#' @param  nrep Number of replications. How many samples of size \code{nsize} 
#' should be taken, default value is 10000.     
#' @return A list giving x values the density estimates y, from the generated
#'  samples.  The function  also draws the empirical density on the current 
#' graphics device.  
#' @examples
#' a1 <- see_the_wlln_for_uniform(nsize=1, nrep=50000)
#' a2 <- see_the_wlln_for_uniform(nsize=10, nrep=50000)
#' a3 <- see_the_wlln_for_uniform(nsize=50, nrep=50000)
#' a4 <- see_the_wlln_for_uniform(nsize=100, nrep=50000)
#' plot(a4, type="l", lwd=2, ylim=range(c(a1$y, a2$y, a3$y, a4$y)), col=1, 
#' lty=1, xlab="mean", ylab="density estimates")
#' lines(a3, type="l", lwd=2, col=2, lty=2)
#' lines(a2, type="l", lwd=2, col=3, lty=3)
#' lines(a1, type="l", lwd=2, col=4, lty=4)
#' symb <- c("n=1", "n=10", "n=50", "n=100")
#' legend(x=0.37, y=11.5, legend = symb, lty =4:1, col = 4:1)
#' @export
see_the_wlln_for_uniform <- function(nsize = 10, nrep=1000) {
  ## set number of replicate samples
  
  ## First generate sample*nrep many U(0,1)
  x <- runif(nsize * nrep)
  
  ## Put the random values in a matrix form with nsample rows and nrep columns.
  ## Each column is then a replicate
  y <- matrix(x, ncol=nrep)
  
  ## Now find mean for each column
  
  zbar <- apply(y, 2, mean) ## 2 for columns
  
  ## Would like to overlay a density 
  u <- seq(from=min(zbar), to= max(zbar), length=100)
  v <- dnorm(u, mean=0.5, sd=sqrt(1/(12*nsize)))
  v1 <- density(zbar)
  hist(zbar, nclass=20, prob=TRUE, xlab="sample mean", col="grey", 
       xlim=range(c(min(zbar)-0.05, max(zbar)+0.05)), 
       ylim=range(v, v1$y),  main=paste("Histogram of", nrep, 
      "sample means from samples of size", nsize))
  lines(density(zbar), lty=2, col="darkgreen", lwd=2) # 
  # add a density estimate with defaults
  cbind.data.frame(x=v1$x, y=v1$y)
}




