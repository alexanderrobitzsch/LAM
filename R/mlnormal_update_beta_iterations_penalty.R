## File Name: mlnormal_update_beta_iterations_penalty.R
## File Version: 0.12


mlnormal_update_beta_iterations_penalty <- function(beta, prior_args, XVX, XVY,
    control_beta )
{
    beta_parnames <- names(beta)
    maxiter <- control_beta$maxiter
    conv <- control_beta$conv
    # ridge <- control_beta$ridge
    iter <- 1
    do_iterations <- TRUE

    M1 <- XVY
    M2 <- XVX
    beta_GLS <- as.vector( solve( XVX, XVY ) )
    beta <- beta_GLS
    NB <- length(beta)
    lambda <- prior_args$penalty_pars$lambda_beta * prior_args$penalty_pars$weights_beta
    eps <- 1E-50

    while( do_iterations ){
        beta00 <- as.vector(beta)

        # parameter bb
        for (bb in 1:NB){
            T0 <- M1[bb,,drop=FALSE] - M2[bb, - bb, drop=FALSE ] %*% beta[-bb]
            T0_adj <- sirt::soft_thresholding( x=as.vector(T0), lambda=lambda[bb] )
            beta[bb] <- T0_adj / ( M2[bb,bb] + eps )
        }
        beta_change <- mlnormal_parameter_change( pars=beta, pars0=beta00 )
        # beta_change <- max( abs( beta_diff ) )
        do_iterations <- ! ( ( beta_change < conv ) | ( iter >=maxiter ) )
        iter <- iter + 1
    }
    return(beta)
}
