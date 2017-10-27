%% File Name: LAM-package.Rd
%% File Version: 0.07

\name{LAM-package}
\alias{LAM-package}
\alias{LAM}
\docType{package}
\title{
Some Latent Variable Models
}
\description{
    Contains some procedures for latent variable modelling with a 
    particular focus on multilevel data.
    The LAM package contains mean and covariance structure modelling
    for multivariate normally distributed data ('mlnormal'),
    a general Metropolis-Hastings algorithm ('amh') and penalized
    maximum likelihood estimation ('pmle').
}
\details{

The \pkg{LAM} package contains the following main functions: 

\itemize{   


%% mlnormal
\item A general fitting method for mean and covariance structure for
multivariate normally distributed data is the \code{\link{mlnormal}}
function. Prior distributions or regularization methods (lasso penalties)
are also accomodated. Missing values on dependent variables can be 
treated by full information maximum likelihood methods.


%% amh, pmle
\item A general (but experimental) Metropolis-Hastings sampler for Bayesian 
analysis based on MCMC is implemented in the \code{\link{amh}} function.
Deterministic optimization of the posterior distribution (maximum
posterior estimation or penalized maximum likelihood estimation) can be
conduction with the \code{\link{pmle}} function which is based on
\code{\link[stats:optim]{stats::optim}}.


}

}
\author{
Alexander Robitzsch \cr 
IPN - Leibniz Institute for Science and Mathematics Education
at Kiel University \cr \cr
Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de> \cr \cr
}



%\seealso{
%XXXXXXX
%}


\examples{
  ##  > library(LAM)
  ##  ## LAM 0.0-4 (2017-03-03 16:53:46)
  ##
  ##   __         ______     __    __    
  ##  /\ \       /\  __ \   /\ "-./  \   
  ##  \ \ \____  \ \  __ \  \ \ \-./\ \  
  ##   \ \_____\  \ \_\ \_\  \ \_\ \ \_\ 
  ##    \/_____/   \/_/\/_/   \/_/  \/_/ 
  ##                      
}

\keyword{package}
