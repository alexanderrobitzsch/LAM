## File Name: mlnormal_verbose_f0.R
## File Version: 0.02
## File Last Change: 2017-01-18 11:02:50

mlnormal_verbose_f0 <- function(verbose, disp)
{
	if (verbose){		
		cat(disp)
	    cat("Preprocess data   " , 
		          paste( Sys.time() ) , "\n" )
		utils::flush.console()						
	}	
}		
			
