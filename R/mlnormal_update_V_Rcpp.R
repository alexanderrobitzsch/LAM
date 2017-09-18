## File Name: mlnormal_update_V_Rcpp.R
## File Version: 0.09
## File Last Change: 2017-02-18 18:55:59

###################################################################
# update matrix V and its inverse
mlnormal_update_V_Rcpp <- function( Z_index , G , theta ,
		Z_list , use_ginverse , variance_shortcut , freq_id  ,
		do_compute , rcpp_args ){		
	res <- mlnormal_update_V_rcpp_helper(
				Z_list ,  rcpp_args$Z_index , rcpp_args$dim_id ,
				rcpp_args$dim_Z_index , rcpp_args$start , rcpp_args$end ,
				rcpp_args$N , rcpp_args$max_dim , rcpp_args$do_compute ,
				as.vector(theta) , as.integer(use_ginverse) )
	rcpp_args$V <- res$V
    rcpp_args$V1 <- res$V1	
	#--- output
	res <- list( rcpp_args = rcpp_args , V_list = res$V_list , 
						V1_list = res$V1_list )
	return(res)
}
######################################################################
		
		
