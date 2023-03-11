## File Name: mlnormal_abs_approx.R
## File Version: 0.06
## File Last Change: 2018-11-23

mlnormal_abs_approx <- function(x, eps=1E-4, deriv=0)
{
    res <- NULL
    abs_x <- sqrt(x^2 + eps)
    if ( deriv==0){
        res <- abs_x
    }
    if ( deriv==1){
        res <- fct1(x=x, eps=eps)
    }
    if ( deriv==2){
        h <- 1E-4
        h <- h * abs(x)
        y0 <- fct1( x=x, eps=eps )
        y1 <- fct1( x=x+h, eps=eps )
        res <- ( y1 - y0 ) / h
    }
    return(res)
}

fct1 <- function(x, eps=eps)
{
    2*x / sqrt(x^2 +eps)
}
