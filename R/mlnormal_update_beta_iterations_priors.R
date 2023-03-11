## File Name: mlnormal_update_beta_iterations_priors.R
## File Version: 0.08
## File Last Change: 2018-11-23


mlnormal_update_beta_iterations_priors <- function(beta, prior_args, XVX, XVY,
    control_beta )
{
    beta_parnames <- names(beta)
    maxiter <- control_beta$maxiter
    conv <- control_beta$conv
    ridge <- control_beta$ridge
    iter <- 1
    do_iterations <- TRUE
    while( do_iterations ){
        beta00 <- as.vector(beta)
        names(beta00) <- beta_parnames
        # evaluate priors and likelihood
        res <- mlnormal_eval_priors_derivative2( pars=beta00,
                    prior=prior_args$prior, h=prior_args$numdiff.parm )
        der <- XVY - XVX %*% beta - res$der
        theta_infomat1 <- XVX - res$infomat
        theta_infomat1 <- mlnormal_covmat_add_ridge( covmat=theta_infomat1, eps=ridge)
        Hinv <- solve(theta_infomat1)
        #*** Newton step
        beta_diff <- Hinv %*% der
        beta <- beta00 + beta_diff
        beta_change <- max( abs( beta_diff ) )
        do_iterations <- ! ( ( beta_change < conv ) | ( iter >=maxiter ) )
        iter <- iter + 1
    }
    return(beta)
}
