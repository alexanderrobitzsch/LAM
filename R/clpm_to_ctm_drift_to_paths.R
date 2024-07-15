## File Name: clpm_to_ctm_drift_to_paths.R
## File Version: 0.02


clpm_to_ctm_drift_to_paths <- function(A, delta2)
{
    requireNamespace('expm')
    Phi2 <- expm::expm(x=A*delta2)
    return(Phi2)
}
