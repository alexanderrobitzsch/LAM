
mlnormal_verbose_f0 <- function(verbose, disp)
{
	if (verbose){		
		cat(disp)
	    cat("Preprocess data   " , 
		          paste( Sys.time() ) , "\n" )
		utils::flush.console()						
	}	
}		
			
