## File Name: mlnormal_update_beta_XVX_Rcpp.R
## File Version: 0.11

################################################################
# update beta
mlnormal_update_beta_XVX_Rcpp <- function( NB , Z_index , G ,
		V1_list , X_list , y_list , rcpp_args , X , y){
		res <- mlnormal_update_beta_rcpp_helper(
					rcpp_args$dim_id , rcpp_args$start , rcpp_args$end ,
					G , X , y , rcpp_args$V1 )
		#--- output
		res$XV_list <- NULL
		return(res)
}
