## File Name: amh_sampling_boundary_values.R
## File Version: 0.05
## File Last Change: 2020-05-07

amh_sampling_boundary_values <- function(pars_pp_new, pars_lower,
        pars_upper, pars_old, pp, boundary_ignore=FALSE)
{
    reject <- FALSE
    if ( pars_pp_new < pars_lower[pp] ){
        if (boundary_ignore){
            pars_pp_new <- pars_old[pp]
            reject <- TRUE
        } else {
            pars_pp_new <- pars_lower[pp]
        }
    }
    if ( pars_pp_new > pars_upper[pp] ){
        if (boundary_ignore){
            pars_pp_new <- pars_old[pp]
            reject <- TRUE
        } else {
            pars_pp_new <- pars_upper[pp]
        }
    }
    if ( (pars_old[pp]==pars_lower[pp]) & (pars_pp_new==pars_upper[pp])){
        pars_pp_new <- pars_old[pp]
        reject <- TRUE
    }
    if ( (pars_old[pp]==pars_upper[pp]) & (pars_pp_new==pars_lower[pp])){
        pars_pp_new <- pars_old[pp]
        reject <- TRUE
    }
    if ( (pars_old[pp]==pars_lower[pp]) & (pars_pp_new==pars_lower[pp])){
        pars_pp_new <- pars_old[pp]
        reject <- TRUE
    }
    if ( (pars_old[pp]==pars_upper[pp]) & (pars_pp_new==pars_upper[pp])){
        pars_pp_new <- pars_old[pp]
        reject <- TRUE
    }
    #-- output
    res <- list(pars_pp_new=pars_pp_new, reject=reject)
    return(res)
}
