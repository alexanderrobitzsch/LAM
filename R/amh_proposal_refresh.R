## File Name: amh_proposal_refresh.R
## File Version: 0.25



#*** refreshing the proposal SD
amh_proposal_refresh <- function( acceptance_parameters, proposal_sd,
        acceptance_bounds, acceptance_rates_history, proposal_equal )
{
    target <- mean(acceptance_bounds)
    acc <- acceptance_parameters[,1] / acceptance_parameters[,2]
    SD.pp <- proposal_sd
    #-- compute new proposal SD
    SD.pp <- ifelse( acc < acceptance_bounds[1],
                    SD.pp / ( 2 - acc / target ), SD.pp )
    SD.pp <- ifelse( acc > acceptance_bounds[2],
                    SD.pp * ( 2 - (1-acc)/(1-target) ), SD.pp )
    SD.pp <- ifelse( acceptance_parameters$no_change >=proposal_equal, proposal_sd, SD.pp )
    proposal_retain <- 1 * ( proposal_sd==SD.pp )
    ind <- attr(acceptance_rates_history,"include")
    acceptance_rates_history[ ind, ] <- acc
    attr(acceptance_rates_history,"include") <- ind + 1
    #-- acceptance parameters
    acceptance_parameters[,1:2] <- 0
    acceptance_parameters$no_change <- acceptance_parameters$no_change + proposal_retain
    acceptance_parameters$no_change[ proposal_retain==0 ] <- 1
    #-- output
    res0 <- list( proposal_sd=SD.pp, acceptance_parameters=acceptance_parameters,
                  acceptance_rates_history=acceptance_rates_history )
    return(res0)
}
