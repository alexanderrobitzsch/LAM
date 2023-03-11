## File Name: mlnormal_create_disp.R
## File Version: 0.06
## File Last Change: 2018-11-23

mlnormal_create_disp <- function(symbol=".", length=30, line_break=TRUE )
{
    v1 <- paste0( rep(symbol, length=length), collapse="")
    v1 <- paste0( v1, "\n")
    return(v1)
}
