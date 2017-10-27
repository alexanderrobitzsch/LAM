## File Name: mlnormal_update_V_R.R
## File Version: 0.22

##############################################
# update matrix V and its inverse
mlnormal_update_V_R <- function( Z_index , G , theta ,
		Z_list , use_ginverse , variance_shortcut , freq_id  ,
		do_compute , rcpp_args){		
	dimZ <- dim( Z_index )
	Z2 <- dimZ[2]
	V_list <- as.list(1:G)
	V1_list <- V_list
	dimZ <- dim( Z_index )
	Z2 <- dimZ[2]
	do_computation <- TRUE	
	
	for (gg in 1:G){
		# gg <- 1
		# compute V for group gg
		if ( do_compute[gg] ){
			Z_index_gg <- Z_index[gg,,,drop=FALSE]
			Z_list_gg <- Z_list[[gg]] 
			V_gg <- 0*Z_list_gg[[1]]
			for (pp in 1:Z2){
				# pp <- 1				
				# theta^q
				a1 <- prod( theta^( Z_index_gg[1,pp,] ) )
				V_gg <- V_gg + a1 * Z_list_gg[[pp]]
			}
			## use generalized inverse instead of inverse if
			## solve does not work in case of singularity
			if ( ! use_ginverse ){
				V_gg1 <- solve( V_gg ) 
			} else {
				V_gg1 <- sirt::ginverse_sym( V_gg ) 
			}
		}  # 	end do computation			
		V_list[[gg]] <- V_gg				
		V1_list[[gg]] <- V_gg1
	}
	#--- output
	res <- list("V_list" = V_list , "V1_list" = V1_list,
					"rcpp_args" = rcpp_args )
	return(res)
}
######################################################################
		
		
