## File Name: mlnormal_proc_variance_shortcut_Z_Rcpp.R
## File Version: 0.15


mlnormal_proc_variance_shortcut_Z_Rcpp <- function( Z_list , Z_index , G , freq_id){
	#-- create list with arguments for Rcpp functions
	rcpp_args <- list( 
					"Z_index" = as.vector(Z_index) ,
					"dim_Z_index" = dim(Z_index) ,
					"update_dim" = as.vector(freq_id[,"update_dim"]) ,
					"start_orig" = as.vector(freq_id[,"start_orig"])-1 ,	
					"end_orig" = as.vector(freq_id[,"end_orig"])-1 ,
					"orig_id" = as.vector(freq_id[,"orig_id"]) ,
					"dim_id" = as.vector(freq_id[,"dim_id"])	,
					"start" = as.vector(freq_id[,"start"])	,
					"end" = as.vector(freq_id[,"end"]),
					"N" = as.integer( max( freq_id[,"end"] ) ) ,
					"max_dim" = as.integer( max( freq_id[,"dim_id"] ))
							)

	#--- load Rcpp function
	res <- mlnormal_proc_variance_shortcut_Z_restructure(
				Z_list , rcpp_args$update_dim , rcpp_args$start_orig , 
				rcpp_args$end_orig , rcpp_args$dim_Z_index , rcpp_args$Z_index , 
				rcpp_args$orig_id, rcpp_args$dim_id )
	freq_id$update_dim <- res$update_dim[,1]						
	
	#--- output
	res <- list( "freq_id" = freq_id , "rcpp_args" = rcpp_args )
	return(res)
}
		
