
mlnormal_parameter_change <- function( pars , pars0 )
{
	pars_change <- max( abs( pars - pars0) )
	return(pars_change)
}
