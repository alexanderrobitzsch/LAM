## File Name: amh_loglike.R
## File Version: 0.07
## File Last Change: 2018-11-23


#########################################################
# evaulate log-likelihood for amh fit
amh_loglike <- function( model, data, pars){
    ll <- do.call( model, list( pars=pars, data=data ) )
    return(ll)
}
