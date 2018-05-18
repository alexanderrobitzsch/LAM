## File Name: confint.amh.R
## File Version: 0.04

###########################################
# confidence interval
confint.amh <- function( object , parm , level = .95 ,  ... )
{
    mcmcobj <- object$mcmcobj
    exclude <- "deviance"
    mcmcobj <- mcmcobj[ , ! ( colnames(mcmcobj) %in% exclude ) ]
    if ( ! missing(parm) ){
        mcmcobj <- mcmcobj[,parm]
            }
    q1 <- ( 1 - level ) / 2
    h1 <- apply( mcmcobj , 2 , stats::quantile , q1 )
    q2 <- 1 - ( 1 - level ) / 2
    h2 <- apply( mcmcobj , 2 , stats::quantile , q2 )
    res <- data.frame( h1 , h2)
    colnames(res)[1] <- paste0( round( 100*q1 ,1 ) , " %")
    colnames(res)[2] <- paste0( round( 100*q2 ,1 ) , " %")
    rownames(res) <- colnames(mcmcobj)
    return(res)
}
###############################################
