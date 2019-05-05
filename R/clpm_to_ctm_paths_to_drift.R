## File Name: clpm_to_ctm_paths_to_drift.R
## File Version: 0.02


clpm_to_ctm_paths_to_drift <- function(Phi1, delta1)
{
    requireNamespace("expm")
    A <- expm::logm(x=Phi1) / delta1
    return(A)
}
