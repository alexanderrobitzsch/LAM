## File Name: mlnormal_create_disp.R
## File Version: 0.03
## File Last Change: 2017-01-18 11:02:49

mlnormal_create_disp <- function(symbol="." , length=30 , line_break = TRUE )
{
	v1 <- paste0( rep(symbol , length= length) , collapse="")
	v1 <- paste0( v1 , "\n")
	return(v1)
}
