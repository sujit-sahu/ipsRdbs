
 # usethis::use_data(beanie, overwrite = TRUE)
 # usethis::use_data(bill, overwrite = TRUE)
 # usethis::use_data(bodyfat, overwrite = TRUE)
 # usethis::use_data(bombhits, overwrite = TRUE)
 # usethis::use_data(cement, overwrite = TRUE)
 # usethis::use_data(cheese, overwrite = TRUE)
 # usethis::use_data(err_age, overwrite = TRUE)
 # usethis::use_data(ffood, overwrite = TRUE)
 # usethis::use_data(gasmileage, overwrite = TRUE)
#  usethis::use_data(emissions, overwrite = TRUE)
 # usethis::use_data(possum, overwrite = TRUE)
 # usethis::use_data(puffin, overwrite = TRUE)
 # usethis::use_data(rice, overwrite = TRUE)
 # usethis::use_data(wgain, overwrite = TRUE)
 # usethis::use_data(cfail, overwrite = TRUE)

#' @import Rdpack
#' @import ggplot2
#' @import methods
#' @importFrom utils head
#' @importFrom graphics hist
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom stats density
#' @importFrom stats dnorm
#' @importFrom stats runif
#' @importFrom stats sd
#' @importFrom Rdpack reprompt
NULL

#' Age and value of 50 beanie baby toys    
#' 
#' @source Beanie world magazine
#'  @format A data frame with 50 rows and 3 columns:
#' \describe{
#'   \item{name}{Name of the toy}
#'   \item{age}{Age of the toy in months}
#'   \item{value}{Market value of the toy in US dollars}
#' }
#' @examples
#'  summary(beanie)
#'  head(beanie)
"beanie"

#' Wealth, age and region of 225 billionaires in 1992 as reported in the 
#' Fortune magazine
#' 
#' @source Fortune magazine 1992. 
#' @format A data frame with 225 rows and three columns:
#' \describe{
#'   \item{wealth}{Wealth in billions of US dollars}
#'   \item{age}{Age of the billionaire}
#'    \item{region}{five regions:Asia, Europe, Middle East, United States, and Other}
#' }
#' @examples
#' head(bill)
#' summary(bill)
#' library(ggplot2)
#' gg <- ggplot2::ggplot(data=bill, aes(x=age, y=wealth)) +
#' geom_point(aes(col=region, size=wealth)) + 
#' geom_smooth(method="loess", se=FALSE) + 
#' xlim(c(7, 102)) + 
#' ylim(c(1, 37)) + 
#' labs(subtitle="Wealth vs Age of Billionaires", 
#' y="Wealth (Billion US $)", x="Age", 
#' title="Scatterplot", caption = "Source: Fortune Magazine, 1992.")
#' plot(gg)
"bill"



#' Number of bomb hits in London during World War II    
#' 
#' @format A data frame with two columns and six rows:
#' \describe{
#'   \item{numberhit}{The number of  bomb hits during World War II 
#'   in each of the 576 areas in London.}
#'   \item{freq}{Frequency of the number of hits}
#' }
#' @examples
#'  summary(bombhits)
#'  # Create a vector of data 
#'  x <- c(rep(0, 229), rep(1, 211), rep(2, 93), rep(3, 35), rep(4, 7), 5)
#'  y <- c(229, 211, 93, 35, 7, 1) # Frequencies 
#'  rel_freq <- y/576
#'  xbar <- mean(x)
#'  pois_prob <- dpois(x=0:5, lambda=xbar)
#'  fit_freq <- pois_prob * 576
#'   #Check 
#'   cbind(x=0:5, obs_freq=y, rel_freq =round(rel_freq, 4),  
#'   Poisson_prob=round(pois_prob, 4), fit_freq=round(fit_freq, 1))
#'   obs_freq <- y
#'   xuniques <- 0:5
#'   a <- data.frame(xuniques=0:5, obs_freq =y, fit_freq=fit_freq)
#'   barplot(rbind(obs_freq, fit_freq), 
#'   args.legend = list(x = "topright"), 
#'   xlab="No of bomb hits",  
#'   names.arg = xuniques,  beside=TRUE, 
#'   col=c("darkblue","red"), 
#'   legend =c("Observed", "Fitted"), 
#'   main="Observed and Poisson distribution fitted frequencies 
#'   for the number of bomb hits in London")
"bombhits"

#' Breaking strength of cement data 
#'   
#' @format A data frame with 36 rows and 3 columns:
#' \describe{
#'  \item{strength}{Breaking strength in pounds per square inch}
#'  \item{gauger}{Three different gauger machines which mixes cement with water}
#'  \item{breaker}{Three different breakers breaking the cement cubes}
#' }
#' @examples
#'  summary(cement)
"cement"

#' Errors in guessing ages of Southampton mathematicians
#' 
#'  @format A data frame with 550 rows and 10 columns
#' \describe{
#'  \item{group}{Group number of the students guessing the ages}
#'  \item{size}{Number of students in the group}
#'  \item{females}{How many female guessers were in the group}
#'  \item{photo}{Photograph number guessed, can take value 1 to 10.}
#'  \item{sex}{Gender of the photographed person.}
#'  \item{race}{Race of the photographed person.}
#'  \item{est_age}{Estimated age of the photographed person.}
#'  \item{tru_age}{True age of the photographed person.}
#'  \item{error}{The value of error, estimated age minus true age}
#'  \item{abs_error}{Absolute value of the error}
#' }
#' @examples
#'  summary(err_age)
"err_age"


#' Service (waiting) times (in seconds)  of customers at a fast-food 
#' restaurant.    
#' 
#'  @format A data frame with 10 rows and 2 columns:
#' \describe{
#'   \item{AM}{Waiting times for customers served during 9-10AM}
#'   \item{PM}{Waiting times for customers served during 2-3PM}
#' }
#' @examples
#'  summary(ffood)
#'  # 95% Confidence interval for the mean waiting time usig t-distribution
#'  a <- c(ffood$AM, ffood$PM)
#'  mean(a) + c(-1, 1) * qt(0.975, df=19) * sqrt(var(a))/sqrt(20) 
#'  # Two sample t-test for the difference between morning and afternoon times
#'  t.test(ffood$AM, ffood$PM)
"ffood"


#' Nitrous oxide emission data
#' 
#' @source  Australian Traffic Accident Research Bureau 
#'  @format A data frame with thirteen columns and 54 rows. 
#' \describe{
#'   \item{Make}{Make of the car}
#'   \item{Odometer}{Odometer reading of the car}
#'   \item{Capacity}{Engine capacity of the car}
#'   \item{CS505}{A measurement taken while running the engine from a cold 
#'   start for 505 seconds} 
#'  \item{T867}{A measurement taken while running the engine in transition 
#'  from cold to hot for 867 seconds}
#'  \item{H505}{A measurement taken while running the hot engine for 
#'  505 seconds}
#'  \item{ADR27}{A previously used measurement standard}
#'  \item{ADR37}{Result of the Australian standard ADR37test}
#'  \item{logCS505}{Logarithm of CS505}
#'  \item{logT867}{Logarithm of T867}
#'  \item{logH505}{Logarithm of H505}
#'  \item{logADR27}{Logarithm of ADR27}
#'  \item{logADR37}{Logarithm of ADR37} 
#' }
#' @examples
#'  summary(emissions)
#'  
#'  rawdata <- emissions[, c(8, 4:7)]
#'  pairs(rawdata)
#' # Fit the model on the raw scale 
#' raw.lm <- lm(ADR37 ~ ADR27 + CS505  + T867 + H505, data=rawdata) 
#' old.par <- par(no.readonly = TRUE)
#' par(mfrow=c(2,1))
#' plot(raw.lm$fit, raw.lm$res,xlab="Fitted values",ylab="Residuals", main="Anscombe plot") 
#' abline(h=0)
#' qqnorm(raw.lm$res,main="Normal probability plot")
#' qqline(raw.lm$res)
#' # summary(raw.lm)
#' logdata <- log(rawdata)
#' # This only logs the values but not the column names!
#' # We can use the following command to change the column names or you can use
#' # fix(logdata) to do it. 
#' dimnames(logdata)[[2]] <- c("logADR37", "logCS505", "logT867", "logH505", "logADR27")
#' pairs(logdata)
#' log.lm <- lm(logADR37 ~ logADR27 + logCS505  + logT867 + logH505, data=logdata) 
#' plot(log.lm$fit, log.lm$res,xlab="Fitted values",ylab="Residuals", main="Anscombe plot") 
#' abline(h=0)
#' qqnorm(log.lm$res,main="Normal probability plot")
#' qqline(log.lm$res)
#' summary(log.lm)
#' log.lm2 <- lm(logADR37 ~ logADR27 + logT867 + logH505, data=logdata) 
#' summary(log.lm2)
#' plot(log.lm2$fit, log.lm2$res,xlab="Fitted values",ylab="Residuals", main="Anscombe plot") 
#' abline(h=0)
#' qqnorm(log.lm2$res,main="Normal probability plot")
#' qqline(log.lm2$res)
#' par(old.par)
#' #####################################
#' # Multicollinearity Analysis 
#' ######################################
#' mod.adr27 <-  lm(logADR27 ~ logT867 + logCS505 + logH505, data=logdata) 
#' summary(mod.adr27) # Multiple R^2 = 0.9936,
#' mod.t867 <-  lm(logT867 ~ logADR27 + logH505 + logCS505, data=logdata)  
#' summary(mod.t867) # Multiple R^2 = 0.977,
#' mod.cs505 <-  lm(logCS505 ~ logADR27 + logH505 + logT867, data=logdata)  
#' summary(mod.cs505) # Multiple R^2 = 0.9837,
#' mod.h505 <-  lm(logH505 ~ logADR27 + logCS505 + logT867, data=logdata)  
#' summary(mod.h505) # Multiple R^2 = 0.5784,
#' # Variance inflation factors 
#' vifs <- c(0.9936, 0.977, 0.9837, 0.5784)
#' vifs <- 1/(1-vifs) 
#' #Condition numbers 
#' X <- logdata 
#' # X is a copy of logdata 
#' X[,1] <- 1
#' # the first column of X is 1
#' # this is for the intercept 
#' X <- as.matrix(X) 
#' # Coerces X to be a matrix
#' xtx <- t(X) %*% X # Gives X^T X
#' eigenvalues <- eigen(xtx)$values
#' kappa <- max(eigenvalues)/min(eigenvalues)
#' kappa <- sqrt(kappa)
#' # kappa = 244 is much LARGER than 30!
#' 
#' ### Validation statistic
#' # Fit the log.lm2 model with the first 45 observations  
#' # use the fitted model to predict the remaining 9 observations 
#' # Calculate the mean square error validation statistic 
#' log.lmsub <- lm(logADR37 ~ logADR27 + logT867 + logH505, data=logdata, subset=1:45) 
#' # Now predict all 54 observations using the fitted model
#' mod.pred <- predict(log.lmsub, logdata, se.fit=TRUE) 
#' mod.pred$fit # provides all the 54 predicted values 
#' logdata$pred <- mod.pred$fit
#' # Get only last 9 
#' a <- logdata[46:54, ]
#' validation.residuals <- a$logADR37 - a$pred  
#' validation.stat <- mean(validation.residuals^2)
#' validation.stat
"emissions"






#' Weight gain data from 68 first year students during their first 12 
#' weeks in college    
#' 
#'  @format A data frame with three columns and 68 rows:
#' \describe{
#'   \item{student}{Student number, 1 to 68.}
#'   \item{initial}{Initial weight in kilogram}
#'    \item{final}{Final weight in kilogram}
#' }
#' @examples
#'  summary(wgain)
#'  # 95% Confidence interval for mean weight gain 
#'  x <- wgain$final - wgain$initial
#'  mean(x) + c(-1, 1) * qt(0.975, df=67) * sqrt(var(x)/68)
#'  # t-test to test the mean difference equals 0
#'  t.test(x)
"wgain"


#' Weekly number of failures of a university computer system over a period of 
#' two years. This is a data vector containing 104 values.  
#' 
#' @examples
#'  summary(cfail)
#'  # 95% Confidence interval 
#'  c(3.75-1.96 * 3.381/sqrt(104), 3.75+1.96*3.381/sqrt(104)) # =(3.10,4.40).
#'  x <- cfail 
#'  n <- length(x)
#'  h <- qnorm(0.975) 
#'  # 95% Confidence interval Using quadratic inversion 
#'  mean(x) + (h*h)/(2*n) + c(-1, 1) * h/sqrt(n) * sqrt(h*h/(4*n) + mean(x))
#'  # Modelling 
#'  # Observed frequencies 
#'  obs_freq <- as.vector(table(x))
#'  # Obtain unique x values 
#'  xuniques <- sort(unique(x))
#'  lam_hat <- mean(x)
#'  fit_freq <- n * dpois(xuniques, lambda=lam_hat)
#'  fit_freq <- round(fit_freq, 1)
#'  # Create a data frame for plotting 
#'  a <- data.frame(xuniques=xuniques, obs_freq = obs_freq, fit_freq=fit_freq)
#'  barplot(rbind(obs_freq, fit_freq), args.legend = list(x = "topright"), 
#'  xlab="No of weekly computer failures",  
#'  names.arg = xuniques,  beside=TRUE, col=c("darkblue","red"), 
#'  legend =c("Observed", "Fitted"), 
#'  main="Observed and Poisson distribution fitted frequencies 
#'  for the computer failure data: cfail")
"cfail"





