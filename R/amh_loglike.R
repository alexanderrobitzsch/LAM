## File Name: amh_loglike.R
## File Version: 0.04
## File Last Change: 2017-01-18 11:02:46


#########################################################
# evaulate log-likelihood for amh fit
amh_loglike <- function( model , data , pars){
	ll <- do.call( model , list( pars = pars , data = data ) )
	return(ll)
}
