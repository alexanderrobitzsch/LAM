%% File Name: LAM-package.Rd
%% File Version: 0.16

\name{LAM-package}
\alias{LAM-package}
\alias{LAM}
\docType{package}
\title{
Some Latent Variable Models
}
\description{
    Includes some procedures for latent variable modeling with a 
    particular focus on multilevel data.
    The 'LAM' package contains mean and covariance structure modelling
    for multivariate normally distributed data (mlnormal(); Longford, 1987;
    <doi:10.1093/biomet/74.4.817>), a general Metropolis-Hastings algorithm 
    (amh(); Roberts & Rosenthal, 2001, <doi:10.1214/ss/1015346320>) and 
    penalized maximum likelihood estimation (pmle(); Cole, Chu & Greenland, 
    2014; <doi:10.1093/aje/kwt245>).
}
\details{

The \pkg{LAM} package contains the following main functions:

\itemize{


%% mlnormal
\item A general fitting method for mean and covariance structure for
multivariate normally distributed data is the \code{\link{mlnormal}}
function. Prior distributions or regularization methods (lasso penalties)
are also accommodated. Missing values on dependent variables can be
treated by applying the full information maximum likelihood method
implemented in this function.


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



\references{
Cole, S. R., Chu, H., & Greenland, S. (2013). Maximum likelihood, profile likelihood,
and penalized likelihood: a primer. \emph{American Journal of Epidemiology, 179}(2), 252-260.

Longford, N. T. (1987). A fast scoring algorithm for maximum likelihood estimation in
unbalanced mixed models with nested random effects. \emph{Biometrika, 74}(4), 817-827.

Roberts, G. O., & Rosenthal, J. S. (2001). Optimal scaling for various Metropolis-Hastings
algorithms. \emph{Statistical Science, 16}(4), 351-367.
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
