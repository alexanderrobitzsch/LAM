## File Name: mlnormal_proc_variance_shortcut_XY_Rcpp.R
## File Version: 0.11
## File Last Change: 2017-02-18 18:54:30


mlnormal_proc_variance_shortcut_XY_Rcpp <- function(y , X , G , freq_id){
		freq_id <- as.matrix( freq_id )
		X <- as.matrix(X)
		#--- Rcpp function
		res <- mlnormal_proc_variance_shortcut_XY_restructure(
					freq_id ,  y , X, G ) 
		return(res)
}
		
