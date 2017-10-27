## File Name: mlnormal_proc_variance_shortcut_XY_R.R
## File Version: 0.04


mlnormal_proc_variance_shortcut_XY_R <- function(y , X , G , freq_id){
		y0 <- y
		X0 <- X
		index_orig <- unlist( sapply( 1:G , FUN = function(gg){
			                freq_id[gg,"start_orig"]:freq_id[gg,"end_orig"] } ) )		
		X <- X0[ index_orig , ]
		y <- y0[ index_orig ]	
		res <- list( y=y , X=X)	
		return(res)
}
		
