%% File Name: suff_stat_NA_pattern.Rd
%% File Version: 0.07

\name{suff_stat_NA_pattern}
\alias{suff_stat_NA_pattern}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Sufficient Statistics for Dataset with Missing Response Pattern
}

\description{
Computes sufficient statistics for a dataset with an arbitrary missing
response pattern.
}

\usage{
suff_stat_NA_pattern(dat)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{dat}{
Numeric data frame
}
}


%\details{
%xx
%}

\value{
A list with following entries

\item{nobs}{List with number of observations for each missing response pattern}
\item{M}{List with mean vectors}
\item{S}{List with covariance matrices}
\item{varindex}{List with indices of observed variables}
\item{NP}{Number of missing data patterns}
\item{N}{Total sample size}

}
%\references{
%% ~put references to the literature/web site here ~
%}
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
# EXAMPLE 1: Toy example for computation of sufficient statistics
#############################################################################

library(STARTS)

data(data.starts01b, package="STARTS")
dat <- data.starts01b
dat1 <- dat[, paste0("E",1:3)]

#-- compute sufficient statistics
res <- LAM::suff_stat_NA_pattern(dat1)
str(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
