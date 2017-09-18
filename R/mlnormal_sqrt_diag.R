## File Name: mlnormal_sqrt_diag.R
## File Version: 0.02
## File Last Change: 2017-01-18 11:02:50

mlnormal_sqrt_diag <- function(matr)
{
	res <- sqrt( diag(matr) )
	return(res)
}
