## File Name: clpm_to_ctm.R
## File Version: 0.10

clpm_to_ctm <- function(Phi1, delta1=1, delta2=2, Phi1_vcov=NULL)
{
    A_se <- A_vcov <- NULL
    Phi1_se <- Phi2_se <- Phi2_vcov <- NULL
    #- compute drift matrix
    A <- clpm_to_ctm_paths_to_drift(Phi1=Phi1, delta1=delta1)
    #- compute path coefficient matrix for delta2
    Phi2 <- clpm_to_ctm_drift_to_paths(A=A, delta2=delta2)

    #-- compute gradients
    if (! is.null(Phi1_vcov) ){
        par <- as.vector(Phi1)
        ND <- nrow(Phi1)
        NP <- length(par)
        y <- clpm_to_ctm_estimating_function(x=par, delta1=delta1, delta2=delta2)
        Phi1_se <- vcov_to_se(x=Phi1_vcov, to_matrix=TRUE, byrow=TRUE)
        h <- 1e-4
        grad <- matrix(NA, nrow=2*NP, ncol=NP)
        for (pp in 1:NP){
            y1 <- clpm_to_ctm_estimating_function(x=add_increment(vec=par, pos=pp, val=h),
                            delta1=delta1, delta2=delta2)
            y2 <- clpm_to_ctm_estimating_function(x=add_increment(vec=par, pos=pp, val=-h),
                            delta1=delta1, delta2=delta2)
            grad[,pp] <- (y1- y2) / (2*h)
        }

        A_trafo <- grad[1:NP, ]
        Phi2_trafo <- grad[NP + 1:NP, ]
        A_vcov <- vcov_linear_trafo(V=Phi1_vcov, A=A_trafo)
        A_se <- vcov_to_se(x=A_vcov, to_matrix=TRUE, byrow=TRUE)
        Phi2_vcov <- vcov_linear_trafo(V=Phi1_vcov, A=Phi2_trafo)
        Phi2_se <- vcov_to_se(x=Phi2_vcov, to_matrix=TRUE, byrow=TRUE)
    }
    #--- output
    res <- list(A=A, A_se=A_se, A_vcov=A_vcov, Phi2=Phi2, Phi2_se=Phi2_se,
                    Phi2_vcov=Phi2_vcov, Phi1=Phi1, Phi1_se=Phi1_se,
                    Phi1_vcov=Phi1_vcov)
    return(res)
}
