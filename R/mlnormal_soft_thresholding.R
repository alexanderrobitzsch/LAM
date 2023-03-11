## File Name: mlnormal_soft_thresholding.R
## File Version: 0.13
## File Last Change: 2018-11-23


mlnormal_soft_thresholding <- function( x, lambda )
{
    x_abs <- abs(x)
    x <- ifelse( x_abs > lambda, x - sign(x) * lambda, 0 )
    return(x)
}

