## File Name: mlnormal_update_ml_derivative_V.R
## File Version: 0.16

##############################################
# derivatives of V
mlnormal_update_ml_derivative_V <- function( N , NT , G ,
			Z_index , Z_list , theta , REML , V1_list ,
			variance_shortcut , freq_id , do_compute ){

	D1_V_list <- as.list(1:NT)
	D1_V_pp_list <- as.list(1:G)
	
	V1_D1V_V1_list <- D1_V_list
	V1_D1V_V1_pp_list <- D1_V_pp_list
	do_computation <- TRUE
	for (pp in 1:NT){
		# pp <- 1  # parameter pp
		for (gg in 1:G){
			# gg <- 1
			if ( do_compute[gg] ){			
				Z_index_gg <- Z_index[ gg ,,, drop=FALSE ]
				index_pp <- which( Z_index_gg[,,pp] != 0 )
				H0 <- 0*Z_list[[gg]][[1]]
				#**** correct these lines!!!
				# derivatives are allowed for powers of parameters
				for (ii in index_pp){
					# ii <- index_pp[1]
					i1 <- Z_index_gg[1,ii,pp]
					# computing derivatives
	#				if ( i1 == 1 ){ a1 <- 1 }
	#				if ( i1 == 2 ){ a1 <- 2*theta[pp] }
					# a1 <- i1 * theta[pp]^(i1-1)   # d/dx x^p = p x^(p-1)
					a1 <- i1 * theta[pp]^( i1-1 ) 
					#*******
					# correction ARb 2016-07-15
					a2 <- prod( ( theta[-pp])^( Z_index_gg[1,ii, - pp ] ) )			
					a1 <- a1*a2
					H0 <- H0 + a1 * Z_list[[gg]][[ii]]
				}
			}
			D1_V_pp_list[[gg]] <- H0
			if (REML){
				if ( do_compute[gg] ){
					V1_gg <- V1_list[[gg]]
					V2_gg <- V1_gg %*% H0 %*% V1_gg
				}
			V1_D1V_V1_pp_list[[gg]] <- V2_gg
			}	
		}  # end group gg
					
		D1_V_list[[pp]] <- D1_V_pp_list
		V1_D1V_V1_list[[pp]] <- V1_D1V_V1_pp_list
	}  # end parameter pp
	#--- output
	res <- list( "D1_V_list" = D1_V_list , V1_D1V_V1_list = V1_D1V_V1_list )
	return(res)
}
