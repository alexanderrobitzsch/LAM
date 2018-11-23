## File Name: confint.pmle.R
## File Version: 0.09

###########################################
# confidence interval
confint.pmle <- function( object, parm, level=.95,  ... )
{
    am1 <- object$pmle_summary
    if ( ! missing(parm) ){
        am1 <- am1[ am1$parameter %in% parm, ]
    }
    quant <- abs( stats::qnorm( (1-level)/2 ) )
    res <- data.frame( am1$est - quant * am1$se,
                am1$est + quant * am1$se )
    q1 <- ( 1 - level ) / 2
    q2 <- 1  - ( 1 - level ) / 2
    colnames(res)[1] <- paste0( round( 100*q1,1 ), " %")
    colnames(res)[2] <- paste0( round( 100*q2,1 ), " %")
    rownames(res) <- paste(am1$parameter)
    return(res)
}
###############################################
