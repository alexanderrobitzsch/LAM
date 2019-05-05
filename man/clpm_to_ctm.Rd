%% File Name: clpm_to_ctm.Rd
%% File Version: 0.12

\name{clpm_to_ctm}
\alias{clpm_to_ctm}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Transformation of Path Coefficients of Cross-Lagged Panel Model
}

\description{
Transforms path coefficients \eqn{\bold{Phi}(\delta t_1)} of a cross-lagged panel model
(CLPM) based on time interval \eqn{\delta t_1} into a time interval \eqn{\delta t_2}.
The transformation is based on the assumption of a continuous time model (CTM;
Voelkle, Oud, Davidov, & Schmidt, 2012) and a drift matrix \eqn{\bold{A}}.
The transformation relies on the matrix exponential function
(see Kuiper & Ryan, 2018).
}

\usage{
clpm_to_ctm(Phi1, delta1=1, delta2=2, Phi1_vcov=NULL)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Phi1}{Matrix of path coefficients \eqn{\bold{Phi}(\delta t_1)}}
\item{delta1}{Numeric \eqn{\delta t_1}}
\item{delta2}{Numeric \eqn{\delta t_2}}
\item{Phi1_vcov}{Optional covariance matrix for parameter estimates of
\eqn{\bold{Phi}(\delta t_1)}}
}


%\details{
%xx
%}

\value{
A list with following entries

\item{A}{Drift matrix}
\item{A_se}{Standard errors of drift matrix}
\item{A_vcov}{Covariance matrix of drift matrix}
\item{Phi2}{Path coefficients \eqn{\bold{Phi}(\delta t_2)}}
\item{Phi2_se}{Standard errors for \eqn{\bold{Phi}(\delta t_2)}}
\item{Phi2_vcov}{Covariance matrix for \eqn{\bold{Phi}(\delta t_2)}}
}


\references{
Kuiper, R. M., & Ryan, O. (2018). Drawing conclusions from cross-lagged relationships: 
Re-considering the role of the time-interval. 
\emph{Structural Equation Modeling, 25}(5), 809-823. \cr
http://dx.doi.org/10.1080/10705511.2018.1431046

Voelkle, M. C., Oud, J. H., Davidov, E., & Schmidt, P. (2012). An SEM approach to 
continuous time modeling of panel data: Relating authoritarianism and anomia. 
\emph{Psychological Methods, 17}(2), 176-192.
http://dx.doi.org/10.1037/a0027543
}
%\author{
%Alexander Robitzsch
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}


\examples{
#############################################################################
# EXAMPLE 1: Example of Voelkle et al. (2012)
#############################################################################

library(expm)

# path coefficient matrix of Voelkle et al. (2012), but see
# also Kuiper and Ryan (2018)
Phi1 <- matrix( c( .64, .18,
                  .03, .89 ), nrow=2, ncol=2, byrow=TRUE )
# transformation to time interval 2
mod <- LAM::clpm_to_ctm(Phi1, delta1=1, delta2=2)
print(mod)

\dontrun{
#############################################################################
# EXAMPLE 2: Example with two dimensions
#############################################################################

library(STARTS)
library(lavaan)

data(data.starts02, package="STARTS")
dat <- data.starts02$younger_cohort
cormat <- cov2cor(as.matrix(dat$covmat))

#-- estimate CLPM
lavmodel <- "
       a2 ~ a1 + b1
       b2 ~ a1 + b1
       "
mod <- lavaan::sem(lavmodel, sample.cov=cormat, sample.nobs=500)
summary(mod)

#- select parameters
pars <- c("a2~a1", "a2~b1", "b2~a1", "b2~b1")
Phi1 <- matrix( coef(mod)[pars], 2, 2, byrow=TRUE)
Phi1_vcov <- vcov(mod)[ pars, pars ]

# conversion to time interval 1.75
LAM::clpm_to_ctm(Phi1=Phi1, delta1=1, delta2=1.75, Phi1_vcov=Phi1_vcov)

#############################################################################
# EXAMPLE 3: Example with three dimensions
#############################################################################

library(STARTS)
library(lavaan)

data(data.starts02, package="STARTS")
dat <- data.starts02$younger_cohort
cormat <- cov2cor(as.matrix(dat$covmat))

#-- estimate CLPM
lavmodel <- "
       a4 ~ a1 + b1 + c1
       b4 ~ a1 + b1 + c1
       c4 ~ a1 + b1 + c1
       "
mod <- lavaan::sem(lavmodel, sample.cov=cormat, sample.nobs=500)
summary(mod)

#- select parameters
pars <- 1:9
Phi1 <- matrix( coef(mod)[pars], 3, 3, byrow=TRUE)
Phi1_vcov <- vcov(mod)[ pars, pars ]

# conversion frpm time interval 3 to time interval 1
LAM::clpm_to_ctm(Phi1=Phi1, delta1=3, delta2=1, Phi1_vcov=Phi1_vcov)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
