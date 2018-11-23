%% File Name: mlnormal.Rd
%% File Version: 0.51

\name{mlnormal}
\alias{mlnormal}
\alias{summary.mlnormal}
\alias{print.mlnormal}
\alias{logLik.mlnormal}
\alias{coef.mlnormal}
\alias{vcov.mlnormal}
\alias{confint.mlnormal}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
(Restricted) Maximum Likelihood Estimation with Prior Distributions
and Penalty Functions under Multivariate Normality
}

\description{
The \code{mlnormal} estimates statistical model for multivariate normally
distributed outcomes with specified mean structure and
covariance structure (see Details and Examples). Model classes include
multilevel models, factor analysis, structural equation models,
multilevel structural equation models, social relations model and
perhaps more.

The estimation can be conducted under maximum likelihood,
restricted maximum likelihood and
maximum posterior estimation with prior distribution.
Regularization (i.e. LASSO penalties) is also accomodated.
}

\usage{
mlnormal(y, X, id, Z_list, Z_index, beta=NULL, theta, method="ML", prior=NULL,
    lambda_beta=NULL, weights_beta=NULL, lambda_theta=NULL, weights_theta=NULL,
    beta_lower=NULL, beta_upper=NULL,    theta_lower=NULL, theta_upper=NULL,
    maxit=800, globconv=1e-05, conv=1e-06, verbose=TRUE, REML_shortcut=NULL,
    use_ginverse=FALSE, vcov=TRUE, variance_shortcut=TRUE, use_Rcpp=TRUE,
    level=0.95, numdiff.parm=1e-04, control_beta=NULL, control_theta=NULL)

\method{summary}{mlnormal}(object, digits=4, file=NULL, ...)

\method{print}{mlnormal}(x, digits=4, ...)

\method{coef}{mlnormal}(object, ...)

\method{logLik}{mlnormal}(object, ...)

\method{vcov}{mlnormal}(object, ...)

\method{confint}{mlnormal}(object, parm, level=.95, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Vector of outcomes
}
  \item{X}{
Matrix of covariates
}
  \item{id}{
Vector of identifiers (subjects or clusters, see Details)
}
  \item{Z_list}{
List of design matrices for covariance matrix (see Details)
}
  \item{Z_index}{
Array containing loadings of design matrices (see Details).
The dimensions are units \eqn{\times} matrices \eqn{\times} parameters.
}
  \item{beta}{
Initial vector for \eqn{\bold{\beta}}
}
  \item{theta}{
Initial vector for \eqn{\bold{\theta}}
}
  \item{method}{
Estimation method. Can be either \code{"ML"} or \code{"REML"}.
}
  \item{prior}{
Prior distributions. Can be conveniently specified in a string
which is processed by \code{\link{prior_model_parse}}. Only
univariate prior distributions can be specified.
}
  \item{lambda_beta}{
Parameter \eqn{\lambda_{\bold{\beta}}} for penalty function
  \eqn{P( \bold{\beta} )=\lambda_{\bold{\beta}} \sum_h w_{\bold{\beta}h} | \beta _h |}
}
  \item{weights_beta}{
Parameter vector \eqn{\bold{w}_{\bold{\beta}}} for penalty function
  \eqn{P( \bold{\beta} )=\lambda_{\bold{\beta}} \sum_h w_{\bold{\beta}h} | \beta _h |}
}
  \item{lambda_theta}{
Parameter \eqn{\lambda_{\bold{\theta}}} for penalty function
  \eqn{P( \bold{\theta} )=\lambda_{\bold{\theta}}
            \sum_h w_{\bold{\theta}h} | \theta _h | }
}
  \item{weights_theta}{
Parameter vector \eqn{\bold{w}_{\bold{\theta}}} for penalty function
  \eqn{P( \bold{\theta} )=\lambda_{\bold{\theta}}
          \sum_h w_{\bold{\theta}h} | \theta _h | }
}

\item{beta_lower}{Vector containing lower bounds for \eqn{\bold{\beta}} parameter}
\item{beta_upper}{Vector containing upper bounds for \eqn{\bold{\beta}} parameter}
\item{theta_lower}{Vector containing lower bounds for \eqn{\bold{\theta}} parameter}
\item{theta_upper}{Vector containing upper bounds for \eqn{\bold{\theta}} parameter}

  \item{maxit}{
Maximum number of iterations
}
  \item{globconv}{
Convergence criterion deviance
}
  \item{conv}{
Maximum parameter change
}
  \item{verbose}{
Print progress?
}
  \item{REML_shortcut}{
Logical indicating whether computational shortcuts should be used for
REML estimation
}
  \item{use_ginverse}{
Logical indicating whether a generalized inverse should be used
}
  \item{vcov}{
Logical indicating whether a covariance matrix of
\eqn{\bold{\theta}} parameter estimates should be computed in
case of REML (which is computationally demanding)
}
  \item{variance_shortcut}{
Logical indicating whether computational shortcuts for calculating
covariance matrices should be used
}
  \item{use_Rcpp}{
Logical indicating whether the \pkg{Rcpp} package should be used
}
  \item{level}{
Confidence level
}
  \item{numdiff.parm}{
Numerical differentiation parameter
}
  \item{control_beta}{
List with control arguments for \eqn{\bold{\beta}} estimation. The default
is \cr \code{list( maxiter=10, conv=1E-4, ridge=1E-6)}.
}
  \item{control_theta}{
List with control arguments for \eqn{\bold{\theta}} estimation. The default
is \cr \code{list( maxiter=10, conv=1E-4, ridge=1E-6)}.
}
\item{object}{Object of class \code{mlnormal}}
\item{digits}{Number of digits used for rounding}
\item{file}{File name}
\item{parm}{Parameter to be selected for \code{confint} method}
\item{\dots}{Further arguments to be passed}
\item{x}{Object of class \code{mlnormal}}
}


%%%%%%%%% DETAILS
\details{
The data consists of outcomes \eqn{\bold{y}_i} and covariates \eqn{\bold{X}_i}
for unit \eqn{i}. The unit can be subjects, clusters (like schools)
or the full outcome vector. It is assumed that \eqn{\bold{y}_i} is normally
distributed as \eqn{N( \bold{\mu}_i, \bold{V}_i )} where the mean structure is
modelled as \deqn{ \bold{\mu}_i =\bold{X}_i \bold{\beta} } and the covariance
structure \eqn{ \bold{V}_i} depends on a parameter vector \eqn{\bold{\theta}}.
More specifically, the covariance matrix \eqn{ \bold{V}_i}  is modelled as
a sum of functions of the parameter \eqn{\bold{\theta}} and known design matrices
\eqn{\bold{Z}_{im}} for unit \eqn{i} (\eqn{m=1,\ldots,M}). The model is
\deqn{\bold{V}_i=\sum_{m=1}^M \bold{Z}_{im} \gamma_{im}  \qquad \mathrm{with}
\qquad \gamma_{im}=\prod_{h=1}^H \theta_h^{q_{imh}} }
where \eqn{q_{imh}} are non-negative known integers specified in
\code{Z_index} and \eqn{\bold{Z}_{im}} are design matrices specified
in \code{Z_list}.

The estimation follows Fisher scoring (Jiang, 2007; for applications see also
Longford, 1987; Lee, 1990; Gill & Swartz, 2001) and the
regularization approach is as described in Lin, Pang and Jiang (2013)
(see also Krishnapuram, Carin, Figueiredo, & Hartemink, 2005).
}

\value{
List with entries

  \item{theta}{Estimated \eqn{\bold{\theta}} parameter}
  \item{beta}{Estimated \eqn{\bold{\beta}} parameter}
  \item{theta_summary}{Summary of \eqn{\bold{\theta}} parameters}
  \item{beta_summary}{Summary of \eqn{\bold{\beta}} parameters}
  \item{coef}{Estimated parameters}
  \item{vcov}{Covariance matrix of estimated parameters}
  \item{ic}{Information criteria}
  \item{V_list}{List with fitted covariance matrices \eqn{\bold{V}_i}}
  \item{V1_list}{List with inverses of fitted covariance matrices \eqn{\bold{V}_i}}
  \item{prior_args}{Some arguments in case of prior distributions}
  \item{\dots}{More values}
}

\references{
Gill, P. S., & Swartz, T. B. (2001). Statistical analyses for round
robin interaction data.
\emph{Canadian Journal of Statistics, 29}, 321-331.

Jiang, J. (2007). \emph{Linear and generalized linear mixed models and their
applications}. New York: Springer.

Krishnapuram, B., Carin, L., Figueiredo, M. A., & Hartemink, A. J. (2005).
Sparse multinomial logistic regression: Fast algorithms and generalization
bounds. \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence, 27}, 957-968.

Lee, S. Y. (1990). Multilevel analysis of structural equation models.
\emph{Biometrika, 77}, 763-772.

Lin, B., Pang, Z., & Jiang, J. (2013). Fixed and random effects selection
by REML and pathwise coordinate optimization.
\emph{Journal of Computational and Graphical Statistics, 22}, 341-355.

Longford, N. T. (1987). A fast scoring algorithm for maximum likelihood
estimation in unbalanced mixed models with nested random effects.
\emph{Biometrika, 74}, 817-827.
}

\author{
Alexander Robitzsch
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \pkg{lavaan}, \pkg{sem}, \pkg{lava}, \pkg{OpenMx} or \pkg{nlsem}
packages for estimation of
(single level) structural equation models.

See the \pkg{regsem}
and \pkg{lsl} packages for regularized structural equation models.

See \pkg{lme4} or \pkg{nlme} package for estimation of multilevel
models.

See the \pkg{lmmlasso} and \pkg{glmmLasso} packages for regularized
mixed effects models.

See \pkg{OpenMx} and \pkg{xxM} packages (\emph{http://xxm.times.uh.edu/}) for
estimation of multilevel structural equation models.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Two-level random intercept model
#############################################################################

#--------------------------------------------------------------
# Simulate data
#--------------------------------------------------------------

set.seed(976)
G <- 150 ; rg <- c(10,20)   # 150 groups with group sizes ranging from 10 to 20
#* simulate group sizes
ng <- round( stats::runif( G, min=rg[1], max=rg[2] ) )
idcluster <- rep(1:G, ng )
#* simulate covariate
iccx <- .3
x <- rep( stats::rnorm( G, sd=sqrt( iccx) ), ng ) +
            stats::rnorm( sum(ng), sd=sqrt( 1 - iccx) )
#* simulate outcome
b0 <- 1.5 ; b1 <- .4 ; iccy <- .2
y <- b0 + b1*x + rep( stats::rnorm( G, sd=sqrt( iccy) ), ng ) +
         stats::rnorm( sum(ng), sd=sqrt( 1 - iccy) )

#-----------------------
#--- arrange input for mlnormal function

id <- idcluster          # cluster is identifier
X <- cbind( 1, x )      # matrix of covariates
N <- length(id)          # number of units (clusters), which is G

MD <- max(ng)   # maximum number of persons in a group
NP <- 2         # number of covariance parameters theta

#* list of design matrix for covariance matrix
#  In the case of the random intercept model, the covariance structure is
#  tau^2 * J + sigma^2 * I, where J is a matrix of ones and I is the
#  identity matrix
Z <- as.list(1:G)
for (gg in 1:G){
    Ngg <- ng[gg]
    Z[[gg]] <- as.list( 1:2 )
    Z[[gg]][[1]] <- matrix( 1, nrow=Ngg, ncol=Ngg )  # level 2 variance
    Z[[gg]][[2]] <- diag(1,Ngg)            # level 1 variance
}
Z_list <- Z
#* parameter list containing the powers of parameters
Z_index <- array( 0, dim=c(G,2,2) )
Z_index[ 1:G, 1, 1] <- Z_index[ 1:G, 2, 2] <- 1

#** starting values and parameter names
beta <- c( 1, 0 )
names(beta) <- c("int", "x")
theta <- c( .05, 1 )
names(theta) <- c("tau2", "sig2" )

#** create dataset for lme4 for comparison
dat <- data.frame(y=y, x=x, id=id )

#--------------------------------------------------------------
# Model 1: Maximum likelihood estimation
#--------------------------------------------------------------

#** mlnormal function
mod1a <- LAM::mlnormal( y=y, X=X, id=id, Z_list=Z_list, Z_index=Z_index,
            beta=beta, theta=theta, method="ML" )
summary(mod1a)

# lme4::lmer function
library(lme4)
mod1b <- lme4::lmer( y ~ x + (1 | id ), data=dat, REML=FALSE )
summary(mod1b)

#--------------------------------------------------------------
# Model 2: Restricted maximum likelihood estimation
#--------------------------------------------------------------

#** mlnormal function
mod2a <- LAM::mlnormal( y=y, X=X, id=id, Z_list=Z_list, Z_index=Z_index,
            beta=beta, theta=theta, method="REML" )
summary(mod2a)

# lme4::lmer function
mod2b <- lme4::lmer( y ~ x + (1 | id ), data=dat, REML=TRUE )
summary(mod2b)

#--------------------------------------------------------------
# Model 3: Estimation of standard deviation instead of variances
#--------------------------------------------------------------

# The model is now parametrized in standard deviations
# Variances are then modeled as tau^2 and sigma^2, respectively.
Z_index2 <- 2*Z_index       # change loading matrix

# estimate model
mod3 <- LAM::mlnormal( y=y, X=X, id=id, Z_list=Z_list, Z_index=Z_index2,
            beta=beta, theta=theta )
summary(mod3)

#--------------------------------------------------------------
# Model 4: Maximum posterior estimation
#--------------------------------------------------------------

# specify prior distributions for parameters
prior <- "
    tau2 ~ dgamma(NA, 2, .5 )
    sig2 ~ dinvgamma(NA, .1, .1 )
    x ~ dnorm( NA, .2, 1000 )
    "

# estimate model in mlnormal
mod4 <- LAM::mlnormal( y=y, X=X, id=id, Z_list=Z_list, Z_index=Z_index,
            beta=beta, theta=theta, method="REML", prior=prior, vcov=FALSE )
summary(mod4)

#--------------------------------------------------------------
# Model 5: Estimation with regularization on beta and theta parameters
#--------------------------------------------------------------

#*** penalty on theta parameter
lambda_theta <- 10
weights_theta <- 1 + 0 * theta
#*** penalty on beta parameter
lambda_beta <- 3
weights_beta <- c( 0, 1.8 )

# estimate model
mod5 <- LAM::mlnormal( y=y, X=X, id=id, Z_list=Z_list, Z_index=Z_index,
            beta=beta, theta=theta, method="ML", maxit=maxit,
            lambda_theta=lambda_theta, weights_theta=weights_theta,
            lambda_beta=lambda_beta, weights_beta=weights_beta  )
summary(mod5)

#############################################################################
# EXAMPLE 2: Latent covariate model, two-level regression
#############################################################################

# Yb=beta_0 + beta_b*Xb + eb (between level) and
# Yw=beta_w*Xw + ew (within level)

#--------------------------------------------------------------
# Simulate data from latent covariate model
#--------------------------------------------------------------

set.seed(865)
# regression parameters
beta_0 <- 1 ; beta_b <- .7 ; beta_w <- .3
G <- 200      # number of groups
n <- 15      # group size
iccx <- .2   # intra class correlation x
iccy <- .35  # (conditional) intra class correlation y
# simulate latent variables
xb <- stats::rnorm(G, sd=sqrt( iccx ) )
yb <- beta_0 + beta_b * xb + stats::rnorm(G, sd=sqrt( iccy ) )
xw <- stats::rnorm(G*n, sd=sqrt( 1-iccx ) )
yw <- beta_w * xw + stats::rnorm(G*n, sd=sqrt( 1-iccy ) )
group <- rep( 1:G, each=n )
x <- xw + xb[ group ]
y <- yw + yb[ group ]
# test results on true data
lm( yb ~ xb )
lm( yw ~ xw )

# create vector of outcomes in the form
# ( y_11, x_11, y_21, x_21, ... )
dat <- cbind( y, x )
dat
Y <- as.vector( t(dat) )    # outcome vector
ny <- length(Y)
X <- matrix( 0, nrow=ny, ncol=2 )
X[ seq(1,ny,2), 1 ] <- 1   # design vector for mean y
X[ seq(2,ny,2), 2 ] <- 1   # design vector for mean x
id <- rep( group, each=2 )

#--------------------------------------------------------------
# Model 1: Linear regression ignoring multilevel structure
#--------------------------------------------------------------

# y=beta_0 + beta_t *x + e
# Var(y)=beta_t^2 * var_x + var_e
# Cov(y,x)=beta_t * var_x
# Var(x)=var_x

#** initial parameter values
theta <- c( 0, 1, .5 )
names(theta) <- c( "beta_t", "var_x", "var_e")
beta <- c(0,0)
names(beta) <- c("mu_y","mu_x")

# The unit i is a cluster in this example.

#--- define design matrices | list Z_list
Hlist <- list(  matrix( c(1,0,0,0), 2, 2 ), # var(y)
                matrix( c(1,0,0,0), 2, 2 ), # var(y) (two terms)
                matrix( c(0,1,1,0), 2, 2 ), # cov(x,y)
                matrix( c(0,0,0,1), 2, 2 ) ) # var(x)

U0 <- matrix( 0, nrow=2*n ,ncol=2*n )
Ulist <- list( U0, U0, U0, U0 )
M <- length(Hlist)
for (mm in 1:M){    # mm <- 1
    for (nn in 1:n){     # nn <- 0
        Ulist[[ mm ]][ 2*(nn-1) + 1:2, 2*(nn-1) + 1:2 ] <- Hlist[[ mm ]]
    }
}
Z_list <- as.list(1:G)
for (gg in 1:G){
    Z_list[[gg]] <- Ulist
}

#--- define index vectors
Z_index <- array( 0, dim=c(G, 4, 3 ) )
K0 <- matrix( 0, nrow=4, ncol=3 )
colnames(K0) <- names(theta)
# Var(y)=beta_t^2 * var_x + var_e  (matrices withn indices 1 and 2)
K0[ 1, c("beta_t","var_x") ] <- c(2,1)  # beta_t^2 * var_x
K0[ 2, c("var_e") ] <- c(1)  # var_e
# Cov(y,x)=beta_t * var_x
K0[ 3, c("beta_t","var_x")] <- c(1,1)
# Var(x)=var_x
K0[ 4, c("var_x") ] <- c(1)
for (gg in 1:G){
    Z_index[gg,,] <- K0
}

#*** estimate model with mlnormal
mod1a <- LAM::mlnormal( y=Y, X=X, id=id, Z_list=Z_list, Z_index=Z_index,
            beta=beta, theta=theta, method="REML", vcov=FALSE )
summary(mod1a)

#*** estimate linear regression with stats::lm
mod1b <- stats::lm( y ~ x )
summary(mod1b)

#--------------------------------------------------------------
# Model 2: Latent covariate model
#--------------------------------------------------------------

#** initial parameters
theta <- c( 0.12, .6, .5, 0, .2, .2 )
names(theta) <- c( "beta_w", "var_xw", "var_ew",
                "beta_b", "var_xb", "var_eb")

#--- define design matrices | list Z_list
Hlist <- list(  matrix( c(1,0,0,0), 2, 2 ), # var(y)
                matrix( c(1,0,0,0), 2, 2 ), # var(y) (two terms)
                matrix( c(0,1,1,0), 2, 2 ), # cov(x,y)
                matrix( c(0,0,0,1), 2, 2 ) ) # var(x)
U0 <- matrix( 0, nrow=2*n ,ncol=2*n )
Ulist <- list( U0, U0, U0, U0,  # within structure
               U0, U0, U0, U0  )  # between structure
M <- length(Hlist)
#*** within structure
design_within <- diag(n)  # design matrix within structure
for (mm in 1:M){    # mm <- 1
    Ulist[[ mm ]] <- base::kronecker( design_within, Hlist[[mm]] )
}
#*** between structure
design_between <- matrix(1, nrow=n, ncol=n)
      # matrix of ones corresponding to group size
for (mm in 1:M){    # mm <- 1
    Ulist[[ mm + M ]] <-  base::kronecker( design_between, Hlist[[ mm ]] )
}
Z_list <- as.list(1:G)
for (gg in 1:G){
    Z_list[[gg]] <- Ulist
}

#--- define index vectors Z_index
Z_index <- array( 0, dim=c(G, 8, 6 ) )
K0 <- matrix( 0, nrow=8, ncol=6 )
colnames(K0) <- names(theta)
# Var(y)=beta^2 * var_x + var_e  (matrices withn indices 1 and 2)
K0[ 1, c("beta_w","var_xw") ] <- c(2,1)  # beta_t^2 * var_x
K0[ 2, c("var_ew") ] <- c(1)  # var_e
K0[ 5, c("beta_b","var_xb") ] <- c(2,1)  # beta_t^2 * var_x
K0[ 6, c("var_eb") ] <- c(1)  # var_e
# Cov(y,x)=beta * var_x
K0[ 3, c("beta_w","var_xw")] <- c(1,1)
K0[ 7, c("beta_b","var_xb")] <- c(1,1)
# Var(x)=var_x
K0[ 4, c("var_xw") ] <- c(1)
K0[ 8, c("var_xb") ] <- c(1)
for (gg in 1:G){
    Z_index[gg,,] <- K0
}

#--- estimate model with mlnormal
mod2a <- LAM::mlnormal( y=Y, X=X, id=id, Z_list=Z_list, Z_index=Z_index,
            beta=beta, theta=theta, method="ML" )
summary(mod2a)

#############################################################################
# EXAMPLE 3: Simple linear regression, single level
#############################################################################

#--------------------------------------------------------------
# Simulate data
#--------------------------------------------------------------

set.seed(875)
N <- 300
x <- stats::rnorm( N, sd=1.3 )
y <- .4 + .7 * x + stats::rnorm( N, sd=.5 )
dat <- data.frame( x, y )

#--------------------------------------------------------------
# Model 1: Linear regression modelled with residual covariance structure
#--------------------------------------------------------------

# matrix of predictros
X <- cbind( 1, x )
# list with design matrices
Z <- as.list(1:N)
for (nn in 1:N){
    Z[[nn]] <- as.list( 1 )
    Z[[nn]][[1]] <- matrix( 1, nrow=1, ncol=1 )  # residual variance
}
#* loading matrix
Z_index <- array( 0, dim=c(N,1,1) )
Z_index[ 1:N, 1, 1] <- 2  # parametrize residual standard deviation
#** starting values and parameter names
beta <- c( 0, 0 )
names(beta) <- c("int", "x")
theta <- c(1)
names(theta) <- c("sig2" )
# id vector
id <- 1:N

#** mlnormal function
mod1a <- LAM::mlnormal( y=y, X=X, id=id, Z_list=Z, Z_index=Z_index,
            beta=beta, theta=theta, method="ML" )
summary(mod1a)

# estimate linear regression with stats::lm
mod1b <- stats::lm( y ~ x )
summary(mod1b)

#--------------------------------------------------------------
# Model 2: Linear regression modelled with bivariate covariance structure
#--------------------------------------------------------------

#** define design matrix referring to mean structure
X <- matrix( 0, nrow=2*N, ncol=2 )
X[ seq(1,2*N,2), 1 ] <- X[ seq(2,2*N,2), 2 ] <- 1

#** create outcome vector
y1 <- dat[ cbind( rep(1:N, each=2), rep(1:2, N ) ) ]
#** list with design matrices
Z <- as.list(1:N)
Z0 <- 0*matrix( 0, nrow=2,ncol=2)
ZXY <- ZY <- ZX <- Z0
# design matrix Var(X)
ZX[1,1] <- 1
# design matrix Var(Y)
ZY[2,2] <- 1
# design matrix covariance
ZXY[1,2] <- ZXY[2,1] <- 1
# Var(X)=sigx^2
# Cov(X,Y)=beta * sigx^2
# Var(Y)=beta^2 * sigx^2 + sige^2
Z_list0 <- list( ZY, ZY, ZXY, ZX )
for (nn in 1:N){
    Z[[nn]] <- Z_list0
}
#* parameter list containing the powers of parameters
theta <- c(1,0.3,1)
names(theta) <- c("sigx", "beta", "sige" )
Z_index <- array( 0, dim=c(N,4,3) )
for (nn in 1:N){
    # Var(X)
    Z_index[nn, 4, ] <- c(2,0,0)
    # Cov(X,Y)
    Z_index[nn, 3, ] <- c(2,1,0)
    # Var(Y)
    Z_index[nn,1,] <- c(2,2,0)
    Z_index[nn,2,] <- c(0,0,2)
}
#** starting values and parameter names
beta <- c( 0, 0 )
names(beta) <- c("Mx", "My")
# id vector
id <- rep( 1:N, each=2 )

#** mlnormal function
mod2a <- LAM::mlnormal( y=y1, X=X, id=id, Z_list=Z, Z_index=Z_index,
            beta=beta, theta=theta, method="ML" )
summary(mod2a)

#--------------------------------------------------------------
# Model 3: Bivariate normal distribution in (sigma_X, sigma_Y, sigma_XY) parameters
#--------------------------------------------------------------

# list with design matrices
Z <- as.list(1:N)
Z0 <- 0*matrix( 0, nrow=2,ncol=2)
ZXY <- ZY <- ZX <- Z0
# design matrix Var(X)
ZX[1,1] <- 1
# design matrix Var(Y)
ZY[2,2] <- 1
# design matrix covariance
ZXY[1,2] <- ZXY[2,1] <- 1
Z_list0 <- list( ZX, ZY, ZXY  )
for (nn in 1:N){
    Z[[nn]] <- Z_list0
}

#* parameter list
theta <- c(1,1,.3)
names(theta) <- c("sigx", "sigy", "sigxy" )
Z_index <- array( 0, dim=c(N,3,3) )
for (nn in 1:N){
    # Var(X)
    Z_index[nn, 1, ] <- c(2,0,0)
    # Var(Y)
    Z_index[nn, 2, ] <- c(0,2,0)
    # Cov(X,Y)
    Z_index[nn, 3, ] <- c(0,0,1)
}

#** starting values and parameter names
beta <- c( 0, 0 )
names(beta) <- c("Mx", "My")

#** mlnormal function
mod3a <- LAM::mlnormal( y=y1, X=X, id=id, Z_list=Z, Z_index=Z_index,
            beta=beta, theta=theta, method="ML" )
summary(mod3a)

#--------------------------------------------------------------
# Model 4: Bivariate normal distribution in parameters of Cholesky decomposition
#--------------------------------------------------------------

# list with design matrices
Z <- as.list(1:N)
Z0 <- 0*matrix( 0, nrow=2,ncol=2)
ZXY <- ZY <- ZX <- Z0
# design matrix Var(X)
ZX[1,1] <- 1
# design matrix Var(Y)
ZY[2,2] <- 1
# design matrix covariance
ZXY[1,2] <- ZXY[2,1] <- 1
Z_list0 <- list( ZX, ZXY, ZY, ZY  )
for (nn in 1:N){
    Z[[nn]] <- Z_list0
}

#* parameter list containing the powers of parameters
theta <- c(1,0.3,1)
names(theta) <- c("L11", "L21", "L22" )
Z_index <- array( 0, dim=c(N,4,3) )
for (nn in 1:N){
    Z_index[nn,1,] <- c(2,0,0)
    Z_index[nn,2,] <- c(1,1,0)
    Z_index[nn,3,] <- c(0,2,0)
    Z_index[nn,4,] <- c(0,0,2)
}
#** starting values and parameter names
beta <- c( 0, 0 )
names(beta) <- c("Mx", "My")
# id vector
id <- rep( 1:N, each=2 )
#** mlnormal function
mod4a <- LAM::mlnormal( y=y1, X=X, id=id, Z_list=Z, Z_index=Z_index,
            beta=beta, theta=theta, method="ML" )
# parameter with lower diagonal entries of Cholesky matrix
mod4a$theta
# fill-in parameters for Cholesky matrix
L <- matrix(0,2,2)
L[ ! upper.tri(L) ] <- mod4a$theta
#** reconstruct covariance matrix
L %*% t(L)   #=base::tcrossprod(L))
stats::cov.wt(dat, method="ML")$cov
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{Multilevel models}
% \keyword{Structural equation modeling}
