## File Name: mlnormal_sqrt_diag.R
## File Version: 0.02

mlnormal_sqrt_diag <- function(matr)
{
	res <- sqrt( diag(matr) )
	return(res)
}
