## File Name: mlnormal_parameter_change.R
## File Version: 0.03
## File Last Change: 2018-11-23

mlnormal_parameter_change <- function( pars, pars0 )
{
    pars_change <- max( abs( pars - pars0) )
    return(pars_change)
}
