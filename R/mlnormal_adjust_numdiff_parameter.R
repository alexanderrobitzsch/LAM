## File Name: mlnormal_adjust_numdiff_parameter.R
## File Version: 0.05
## File Last Change: 2018-11-23

mlnormal_adjust_numdiff_parameter <- function( h, pars )
{
    #** select h parameters according to size of parameters
    abs_par <- abs(pars)
    hvec <- h * ifelse( abs_par > 1, abs_par, 1 )
    return(hvec)
}
