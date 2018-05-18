## File Name: amh_init_acceptance_parameters.R
## File Version: 0.02

amh_init_acceptance_parameters <- function(pars)
{
    NP <- length(pars)
    acceptance_parameters <- matrix( 0 , nrow=NP , ncol=3 )
    rownames(acceptance_parameters) <- names(pars)
    colnames(acceptance_parameters) <- c("accepted" , "sampled", "no_change" )
    acceptance_parameters <- as.data.frame(acceptance_parameters)
    acceptance_parameters$no_change <- 1
    return(acceptance_parameters)
}
