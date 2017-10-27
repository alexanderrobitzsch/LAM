%% File Name: loglike_mvnorm.Rd
%% File Version: 0.16

\name{loglike_mvnorm}
\alias{loglike_mvnorm}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Log-Likelihood Value of a Multivariate Normal Distribution
}

\description{
Computes log-likelihood value of a multivariate normal distribution
given the empirical mean vector and the empirical covariance matrix
as sufficient statistics.
}
\usage{
loglike_mvnorm(M, S, mu, Sigma, n, log = TRUE, lambda = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
Empirical mean vector
}
  \item{S}{
Empirical covariance matrix
}
  \item{mu}{
Population mean vector
}
  \item{Sigma}{
Population covariance matrix
}
  \item{n}{
Sample size
}
  \item{log}{
Optional logical indicating whether the logarithm of the likelihood
should be calculated.
}
  \item{lambda}{
Regularization parameter of the covariance matrix (see Details).
}
}

\details{
The population covariance matrix \eqn{\bold{\Sigma}} is regularized if
\eqn{\lambda} (\code{lambda}) is chosen larger than zero.
Let \eqn{\bold{\Delta _\Sigma}} denote a diagonal matrix containing 
the diagonal entries of \eqn{\bold{\Sigma}}. Then, a regularized matrix
\eqn{\bold{\Sigma}^\ast} is defined as 
\eqn{\bold{\Sigma}^\ast = w \bold{\Sigma} + (1-w)\bold{\Delta _\Sigma }}
with \eqn{w=n/(n+\lambda)}.
}

\value{
Log-likelihood value
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Alexander Robitzsch
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}


\examples{
#############################################################################
# EXAMPLE 1: Multivariate normal distribution
#############################################################################   

library(MASS)

#--- simulate data
Sigma <- c( 1 , .55 , .5 , .55 , 1 , .5 ,.5 , .5 , 1 )
Sigma <- matrix( Sigma , nrow=3 , ncol=3 )
mu <- c(0,1,1.2)
N <- 400
set.seed(9875)
dat <- MASS::mvrnorm( N , mu , Sigma )
colnames(dat) <- paste0("Y",1:3)
S <- cov(dat)
M <- colMeans(dat)

#--- evaulate likelihood
res1 <- LAM::loglike_mvnorm( M=M , S=S , mu=mu , Sigma=Sigma , n = N , lambda = 0 )
# compare log likelihood with somewhat regularized covariance matrix
res2 <- LAM::loglike_mvnorm( M=M , S=S , mu=mu , Sigma=Sigma , n = N , lambda = 1 )
print(res1)
print(res2)	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
