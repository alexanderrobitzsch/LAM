## File Name: mlnormal_eval_priors_derivative.R
## File Version: 0.08

mlnormal_eval_priors_derivative <- function( pars, prior, h )
{
    #--- adjust numerical differentiation parameter
    hvec <- mlnormal_adjust_numdiff_parameter( h=h, pars=pars  )
    #--- evaluate pars
    x0 <- mlnormal_eval_priors( pars=pars, prior=prior )
    x1 <- mlnormal_eval_priors( pars=pars + hvec, prior=prior )
    der <- ( x1 - x0 ) / hvec
    return(der)
}
