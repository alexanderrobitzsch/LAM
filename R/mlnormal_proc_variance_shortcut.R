## File Name: mlnormal_proc_variance_shortcut.R
## File Version: 0.37
## File Last Change: 2017-01-18 11:02:50

##########################################################################
# process data for shortcuts in variance estimation
mlnormal_proc_variance_shortcut <- function( id , y , X , Z_list ,
		Z_index , use_Rcpp , G ){
zz0 <- Sys.time()		
		#*** group sizes
		freq_id <- rowsum( 1+0*id , id )
		freq_id <- data.frame( 
						as.numeric( rownames(freq_id ) ) , 
						freq_id[,1] )
		colnames(freq_id) <- c("orig_id","dim_id")		
		freq_id$start_orig <-  1 + c(0 , 
								cumsum( freq_id[1:(G-1) , "dim_id"] ) )
		freq_id$end_orig <-  cumsum( freq_id[1:G , "dim_id"] )		
		freq_id <- freq_id[ order( freq_id[,2] ) , ]
		
		G <- nrow(freq_id)
		freq_id$id <- 1:G
		freq_id$update_dim <- c( 1,1 * ( diff(freq_id$dim_id) > 0 ) )
		freq_id$start <-  1 + c(0 , cumsum( freq_id[1:(G-1) , "dim_id"] ) )
		freq_id$end <-  cumsum( freq_id[1:G , "dim_id"] )
				
# cat("##### create freq_id") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1
 
		#--------------------------------------
		#---- check equality of Z_index and Z_list		
		if ( ! use_Rcpp ){
			mlnormal_proc_vs_Z <- mlnormal_proc_variance_shortcut_Z_R
		} else {
			mlnormal_proc_vs_Z <- mlnormal_proc_variance_shortcut_Z_Rcpp
		}			
# cat("**** vor mlnormal_proc_vs_Z")	

		res <- mlnormal_proc_vs_Z( Z_list=Z_list , Z_index = Z_index , G = G , 
		            freq_id = freq_id )
		freq_id <- res$freq_id
		rcpp_args <- res$rcpp_args
		
# cat("##### check equalities") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1									
 
		#--- do compute vector
		do_compute <- freq_id$update_dim == 1		
		if ( use_Rcpp ){
			rcpp_args$do_compute <- as.integer( do_compute )
		}
		
		#---------------------------------------
		#---- rearrange Z_index and Z_list
		Z_index <- Z_index[ freq_id[,1] , , , drop=FALSE]
		Z_list0 <- Z_list
		for (gg in 1:G){
			Z_list[[gg]] <- Z_list0[[  freq_id[gg,1] ]]
		}
#  cat("##### rearrange Z_list and Z_index") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1		

		#---------------------------------------
		#---- rearrange y and X
		if ( use_Rcpp){		
			mlnormal_proc_vs_XY <- mlnormal_proc_variance_shortcut_XY_R
		} else {  # The Rcpp function is slower than the R function
			mlnormal_proc_vs_XY <- mlnormal_proc_variance_shortcut_XY_R
		}				
		res <- mlnormal_proc_vs_XY(y=y, X=X, G=G, freq_id=freq_id)
		y <- res$y
		X <- res$X		
# cat("##### rearrange X and y") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1											
 
		id <- rep( 1:G , freq_id$dim_id )				
 
		#---------------------------------------
		#---------- output
		res <- list( id = id , y = y , X = X , Z_list = Z_list ,
					Z_index = Z_index , freq_id = freq_id , do_compute = do_compute ,
					rcpp_args = rcpp_args )
		return(res)
}
#############################################################################
