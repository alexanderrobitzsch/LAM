## File Name: mlnormal_covmat_add_ridge.R
## File Version: 0.04

mlnormal_covmat_add_ridge <- function( covmat , eps = 1E-10)
{
	diag(covmat) <- diag(covmat) + eps
	return(covmat)
}
