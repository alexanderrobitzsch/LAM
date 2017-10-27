%% File Name: amh.Rd
%% File Version: 0.53

\name{amh}
\alias{amh}
\alias{summary.amh}
\alias{plot.amh}
\alias{logLik.amh}
\alias{coef.amh}
\alias{vcov.amh}
\alias{confint.amh}
\alias{pmle}
\alias{summary.pmle}
\alias{plot.pmle}
\alias{logLik.pmle}
\alias{coef.pmle}
\alias{vcov.pmle}
\alias{confint.pmle}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian Model Estimation with Adaptive Metropolis Hastings Sampling
(\code{amh}) or Penalized Maximum Likelihood Estimation (\code{pmle})
}

\description{
The function \code{amh} conducts a Bayesian statistical analysis using 
the adaptive Metropolis-Hastings
as the estimation procedure (Hoff, 2009). Only univariate prior 
distributions are allowed.
Note that this function is intended just for experimental purpose, not to
replace general purpose packages like \pkg{WinBUGS}, \pkg{JAGS},
\pkg{Stan} or \pkg{MHadaptive}. \cr 

The function \code{pmle} optimizes the penalized likelihood which means that
the posterior is maximized and the maximum a posterior estimate is
obtained. The optimization function \link[stats:optim]{stats::optim}
is used.
}


\usage{
amh(data , nobs , pars , model ,  prior , proposal_sd ,  pars_lower = NULL , 
      pars_upper = NULL , derivedPars = NULL , n.iter = 5000 , n.burnin = 1000 , 
      n.sims=3000, acceptance_bounds = c(.45,.55), proposal_refresh = 50, 
      print_iter = 50 ) 
      
pmle( data , nobs , pars , model ,  prior, pars_lower = NULL , pars_upper = NULL ,
       method = "L-BFGS-B" ,control=list() , verbose = TRUE , hessian = TRUE , ... )      
      
\method{summary}{amh}(object, digits = 3, file = NULL, ...)

\method{plot}{amh}(x , conflevel=.95, digits=3 , lag.max= .1 , 
    col.smooth="red" , lwd.smooth=2 , col.split = "blue" , lwd.split = 2, 
    lty.split=1, col.ci="orange", cex.summ=1, ask=FALSE , ... )

\method{coef}{amh}(object, ...)

\method{logLik}{amh}(object, ...)

\method{vcov}{amh}(object, ...)

\method{confint}{amh}(object , parm , level = .95, ... )

\method{summary}{pmle}(object, digits = 3, file = NULL, ...)

\method{coef}{pmle}(object, ...)

\method{logLik}{pmle}(object, ...)

\method{vcov}{pmle}(object, ...)

\method{confint}{pmle}(object , parm , level = .95, ... )
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Object which contains data
}
\item{nobs}{Number of observations}
  \item{pars}{
Named vector of initial values for parameters
}
  \item{model}{
Function defining the log-likelihood of the model
}
  \item{prior}{
List with prior distributions for the parameters to be sampled (see Examples).
See \code{\link[sirt:prior_model_parse]{sirt::prior_model_parse}}
for more convenient specifications of the prior distributions.
}
  \item{proposal_sd}{
Vector with initial standard deviations for proposal distribution
}
  \item{pars_lower}{
Vector with lower bounds for parameters
}
  \item{pars_upper}{
Vector with upper bounds for parameters
}
\item{derivedPars}{Optional list containing derived parameters from sampled chain}
  \item{n.iter}{
Number of iterations
}
  \item{n.burnin}{
Number of burn-in iterations
}
\item{n.sims}{Number of sampled iterations for parameters}
  \item{acceptance_bounds}{
Bounds for acceptance probabilities of sampled parameters
}
  \item{proposal_refresh}{
Number of iterations for computation of adaptation of porposal
standard deviation
} 
  \item{print_iter}{
Display progress every \code{print_iter}th iteration
}
\item{method}{Optimization in \code{\link[stats:optim]{stats::optim}}}
\item{control}{Control parameters \code{\link[stats:optim]{stats::optim}}}
\item{verbose}{Logical indicating whether progress should be displayed.}
\item{hessian}{Logical indicating whether the Hessian matrix
should be computed}
\item{object}{Object of class \code{amh}}
\item{digits}{Number of digits used for rounding}
\item{file}{File name}
\item{\dots}{Further arguments to be passed}
\item{x}{Object of class \code{amh}}
\item{conflevel}{Confidence level}
\item{lag.max}{Percentage of iterations used for calculation of 
autocorrelation function}
\item{col.smooth}{Color moving average}
\item{lwd.smooth}{Line thickness moving average}
\item{col.split}{Color splitted chain}
\item{lwd.split}{Line thickness splitted chain}
\item{lty.split}{Line type splitted chain}
\item{col.ci}{Color confidence interval}
\item{cex.summ}{Point size summary}
\item{ask}{Logical. If \code{TRUE} the user is asked for input, 
before a new figure is drawn.}
\item{parm}{Optional vector of parameters.}
\item{level}{Confidence level.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List of class \code{amh} including entries

\item{pars_chain}{Data frame with sampled parameters}
\item{acceptance_parameters}{Acceptance probabilities}
\item{amh_summary}{Summary of parameters}
\item{coef}{Coefficient obtained from marginal MAP estimation}
\item{pmle_pars}{Object of parameters and posterior values corresponding
to multivariate maximum of posterior distribution.}
\item{comp_estimators}{Estimates for univariate MAP, multivariate MAP
and mean estimator and corresponding posterior estimates.}
\item{ic}{Information criteria}
\item{mcmcobj}{Object of class \code{mcmc} for \pkg{coda} package}
\item{proposal_sd}{Used proposal standard deviations}
\item{proposal_sd_history}{History of proposal standard
deviations during burn-in iterations}
\item{acceptance_rates_history}{History of acceptance rates for all parameters
during burn-in phase}
\item{\dots}{More values} \cr \cr
}


\references{
Hoff, P. D. (2009). \emph{A first course in Bayesian statistical methods}. 
New York: Springer.
}

\author{
Alexander Robitzsch
}

%\note{
%}

%The sampling of random effects (i.e. hierarchical priors) is not (yet) 
%implemented.


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See the Bayesian CRAN Task View for lot of information about
alternative \R packages.

\code{\link[sirt:prior_model_parse]{sirt::prior_model_parse}}
}

\examples{
\dontrun{	
#############################################################################
# EXAMPLE 1: Constrained multivariate normal distribution
#############################################################################	

#--- simulate data
Sigma <- matrix( c( 
	1 , .55 , .5 , 
	.55 , 1 , .45 , 
	.5 , .45 , 1 ) , nrow=3 , ncol=3 , byrow=TRUE ) 
mu <- c(0,1,1.2)
N <- 400
set.seed(9875)
dat <- MASS::mvrnorm( N , mu , Sigma )
colnames(dat) <- paste0("Y",1:3)
S <- stats::cov(dat)
M <- colMeans(dat)

#-- define maximum likelihood function for normal distribution
fit_ml <- function( S , Sigma , M , mu , n , log=TRUE){
    Sigma1 <- solve(Sigma)
    p <- ncol(Sigma)
    det_Sigma <- det( Sigma )
    eps <- 1E-30
    if ( det_Sigma < eps ){
            det_Sigma <- eps
    }
    l1 <- - p * log( 2*pi ) - t( M - mu ) \%*\% Sigma1 \%*\% ( M - mu ) - 
                  log( det_Sigma )  - sum( diag( Sigma1 \%*\% S ) )
    l1 <- n/2 * l1
    if (! log){ 
    	l1 <- exp(l1) 
    }
    l1 <- l1[1,1]
    return(l1)
}
# This likelihood function can be directly accessed by the loglike_mvnorm function.
        
#--- define data input
data <- list( "S" = S , "M" = M , "n" = N )

#--- define list of prior distributions
prior <- list()
prior[["mu1"]] <- list( "dnorm" , list( x=NA , mean=0 , sd=1 ) )
prior[["mu2"]] <- list( "dnorm" , list( x=NA , mean=0 , sd=5 ) )
prior[["sig1"]] <- list( "dunif" , list( x=NA , 0 , 10 ) )
prior[["rho"]] <- list( "dunif" , list( x=NA ,-1 , 1  ) )

#** alternatively, one can specify the prior as a string and uses
#   the 'prior_model_parse' function 
prior_model2 <- "
   mu1 ~ dnorm(x=NA, mean=0, sd=1)
   mu2 ~ dnorm(x=NA, mean=0, sd=5)   
   sig1 ~ dunif(x=NA, 0,10)
   rho ~ dunif(x=NA,-1,1)
   "
# convert string  
prior2 <- sirt::prior_model_parse( prior_model2 )
prior2  # should be equal to prior

#--- define log likelihood function for model to be fitted
model <- function( pars , data ){ 
    # mean vector
    mu <- pars[ c("mu1", rep("mu2",2) ) ] 
    # covariance matrix
    m1 <- matrix( pars["rho"] * pars["sig1"]^2 , 3 , 3 )
    diag(m1) <- rep( pars["sig1"]^2 , 3 )
    Sigma <- m1
    # evaluate log-likelihood
    ll <- fit_ml( S = data$S , Sigma = Sigma , M = data$M , mu = mu , n = data$n)
    return(ll)
}

#--- initial parameter values
pars <- c(1,2,2,0) 
names(pars) <- c("mu1" , "mu2" , "sig1" , "rho")               
#--- initial proposal distributions
proposal_sd <- c( .4 , .1 , .05 , .1 )
names(proposal_sd) <- names(pars)
#--- lower and upper bound for parameters
pars_lower <- c( -10 , -10 , .001 , -.999 )
pars_upper <- c( 10 , 10 , 1E100 , .999 )

#--- define list with derived parameters
derivedPars <- list( "var1" = ~ I( sig1^2 ) , "d1" = ~ I( ( mu2 - mu1 ) / sig1 ) )

#*** start Metropolis-Hastings sampling
mod <- LAM::amh( data , nobs = data$n , pars=pars , model=model , 
          prior=prior , proposal_sd = proposal_sd ,
          n.iter = 1000 , n.burnin = 300 , derivedPars = derivedPars , 
          pars_lower = pars_lower , pars_upper = pars_upper )

# some S3 methods
summary(mod)
plot(mod, ask=TRUE)
coef(mod)
vcov(mod)
logLik(mod)

#--- compare Bayesian credibility intervals and HPD intervals
ci <- cbind( confint(mod) , coda::HPDinterval(mod$mcmcobj)[-1, ] )
ci
# interval lengths
cbind( ci[,2]-ci[,1] , ci[,4] - ci[,3] )

#--- plot update history of proposal standard deviations
graphics::matplot( x = rownames(mod$proposal_sd_history) ,
          y = mod$proposal_sd_history , type="o" , pch=1:6)

#**** compare results with lavaan package
library(lavaan)
lavmodel <- "
    F=~ 1*Y1 + 1*Y2 + 1*Y3
    F ~~ rho*F
    Y1 ~~ v1*Y1
    Y2 ~~ v1*Y2
    Y3 ~~ v1*Y3   
    Y1 ~ mu1 * 1
    Y2 ~ mu2 * 1    
    Y3 ~ mu2 * 1        
    # total standard deviation
    sig1 := sqrt( rho + v1 )     
    "
# estimate model
mod2 <- lavaan::sem( data = as.data.frame(dat) , lavmodel )
summary(mod2)
logLik(mod2)

#*** compare results with penalized maximum likelihood estimation
mod3 <- LAM::pmle( data=data , nobs= data$n , pars=pars , model = model ,  prior=prior , 
            pars_lower = pars_lower , pars_upper = pars_upper , method = "L-BFGS-B" ,
            control=list( trace=TRUE )  )
# model summaries
summary(res2)
confint(res2)
vcov(res2)

#--- lavaan with covariance and mean vector input
mod2a <- lavaan::sem( sample.cov = data$S , sample.mean = data$M , sample.nobs = data$n , 
                model = lavmodel )
coef(mod2)
coef(mod2a)

#--- fit covariance and mean structure by fitting a transformed
#    covariance structure
#* create an expanded covariance matrix
p <- ncol(S)
S1 <- matrix( NA , nrow= p+1 , ncol=p+1 )
S1[1:p,1:p] <- S + outer( M , M )
S1[p+1,1:p] <- S1[1:p , p+1] <- M
S1[p+1,p+1] <- 1
vars <- c( colnames(S) , "MY" )
rownames(S1) <- colnames(S1) <- vars
#* lavaan model 
lavmodel <- "
    # indicators
    F=~ 1*Y1 + 1*Y2 + 1*Y3
    # pseudo-indicator representing mean structure
    FM =~ 1*MY
    MY ~~ 0*MY
    FM ~~ 1*FM
    F ~~ 0*FM
    # mean structure
    FM =~ mu1*Y1 + mu2*Y2 + mu2*Y3
    # variance structure
    F ~~ rho*F
    Y1 ~~ v1*Y1
    Y2 ~~ v1*Y2
    Y3 ~~ v1*Y3   
    sig1 := sqrt( rho + v1 )     
    "
    
# estimate model
mod2b <- lavaan::sem( sample.cov = S1 ,sample.nobs = data$n , 
                model = lavmodel )
summary(mod2b)
summary(mod2)

#############################################################################
# EXAMPLE 2: Estimation of a linear model with Box-Cox transformation of response
#############################################################################

#*** simulate data with Box-Cox transformation
set.seed(875)
N <- 1000
b0 <- 1.5
b1 <- .3
sigma <- .5
lambda <- 0.3
# apply inverse Box-Cox transformation
  # yl = ( y^lambda - 1 ) / lambda
  # -> y = ( lambda * yl + 1 )^(1/lambda)
x <- stats::rnorm( N ,  mean=0 , sd = 1 )
yl <- stats::rnorm( N , mean=b0 , sd = sigma ) + b1*x
# truncate at zero
eps <- .01
yl <- ifelse( yl < eps  , eps , yl )
y <- ( lambda * yl + 1 ) ^(1/lambda )

#-- display distributions of transformed and untransformed data
   graphics::par(mfrow=c(1,2))
graphics::hist(yl, breaks=20)
graphics::hist(y, breaks=20)
   graphics::par(mfrow=c(1,1))

#*** define vector of parameters
pars <- c( 0  , 0 ,  1 , -.2 )
names(pars) <- c("b0" , "b1" , "sigma" , "lambda" )
#*** input data
data <- list( "y" = y , "x" = x)
#*** define model with log-likelihood function
model <- function( pars , data ){
    sigma <- pars["sigma"]
    b0 <- pars["b0"]
    b1 <- pars["b1"]    
    lambda <- pars["lambda"]
    if ( abs(lambda) < .01){ lambda <- .01 * sign(lambda) }
    y <- data$y
    x <- data$x
    n <- length(y)
    y_lambda <- ( y^lambda - 1 ) / lambda 
    ll <- - n/2 * log(2*pi) - n * log( sigma ) - 
            1/(2*sigma^2)* sum( (y_lambda - b0 - b1*x)^2 ) +
            ( lambda - 1 ) * sum( log( y ) )
    return(ll)
}    
#-- test model function    
model( pars , data )

#*** define prior distributions
prior <- list()
prior[["b0"]] <- list( "dnorm" , list( x=NA , mean=0 , sd=10 ) )
prior[["b1"]] <- list( "dnorm" , list( x=NA , mean=0 , sd=10 ) )
prior[["sigma"]] <- list( "dunif" , list( x=NA , 0 , 10  ) )
prior[["lambda"]] <- list( "dunif" , list( x=NA , -2 , 2 ) )
#*** define proposal SDs
proposal_sd <- c( .1 , .1 , .1 , .1 )
names(proposal_sd) <- names(pars)
#*** define bounds for parameters
pars_lower <- c( -100 , -100 , .01 , -2 )
pars_upper <- c( 100 , 100 , 100 , 2 )

#*** sampling routine
mod <- LAM::amh( data , nobs = N , pars , model ,  prior , proposal_sd ,
        n.iter = 10000 , n.burnin = 2000 , n.sims = 5000 ,
        pars_lower = pars_lower , pars_upper = pars_upper )
#-- S3 methods
summary(mod)
plot(mod , ask=TRUE )

#*** estimating Box-Cox transformation in MASS package
library(MASS)
mod2 <- MASS::boxcox( stats::lm( y ~ x ) , lambda = seq(-1,2,length=100) )
mod2$x[ which.max( mod2$y ) ]

#*** estimate Box-Cox parameter lambda with car package 
library(car)
mod3 <- car::powerTransform( y ~ x )
summary(mod3)  
# fit linear model with transformed response
mod3a <- stats::lm( car::bcPower( y, mod3$roundlam) ~ x )
summary(mod3a)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
