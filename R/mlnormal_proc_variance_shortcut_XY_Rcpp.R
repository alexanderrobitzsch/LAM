## File Name: mlnormal_proc_variance_shortcut_XY_Rcpp.R
## File Version: 0.13


mlnormal_proc_variance_shortcut_XY_Rcpp <- function(y , X , G , freq_id)
{
	freq_id <- as.matrix( freq_id )
	X <- as.matrix(X)
	#--- Rcpp function
	res <- lam_rcpp_mlnormal_proc_variance_shortcut_XY_restructure( 
				freq_id=freq_id, y=y, X=X, G=G )
	return(res)
}
		
