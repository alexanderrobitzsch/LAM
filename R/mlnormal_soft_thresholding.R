## File Name: mlnormal_soft_thresholding.R
## File Version: 0.11
## File Last Change: 2017-03-03 18:23:27


mlnormal_soft_thresholding <- function( x , lambda )
{
    x_abs <- abs(x)
    x <- ifelse( x_abs > lambda , x - sign(x) * lambda , 0 )
    return(x)
}

