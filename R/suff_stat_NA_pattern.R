## File Name: suff_stat_NA_pattern.R
## File Version: 0.123


suff_stat_NA_pattern <- function(dat)
{
    requireNamespace('TAM')
    m1 <- rowSums( 1 - is.na(dat) )
    dat <- dat[ m1 > 0, ]
    N <- nrow(dat)
    res <- TAM::tam_NA_pattern(x=dat)
    mp_index <- res$mp.index
    NP <- max(mp_index)
    #--- create objects
    nobs <- list()
    M <- list()
    S <- list()
    varindex <- list()
    for (pp in 1L:NP){
        dat_pp <- dat[ mp_index==pp,, drop=FALSE]
        varindex[[pp]] <- varindex_pp <- which( ! is.na( dat_pp[1,] ) )
        nobs[[pp]] <- nrow(dat_pp)
        dat_pp1 <- dat_pp[, varindex_pp, drop=FALSE]
        M[[pp]] <- colMeans(dat_pp1)
        S[[pp]] <- stats::cov.wt( dat_pp1, method='ML')$cov
    }
    #-- output
    res <- list( nobs=nobs, M=M, S=S, varindex=varindex, NP=NP, N=N)
    class(res) <- 'suff_stat'
    return(res)
}
