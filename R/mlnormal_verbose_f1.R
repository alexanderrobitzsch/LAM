## File Name: mlnormal_verbose_f1.R
## File Version: 0.02
## File Last Change: 2017-01-18 11:02:50

mlnormal_verbose_f1 <- function(verbose, disp , iter)
{
    if ( verbose ){
	    cat(disp)	
	    cat("Iteration" , iter , "   " , 
		              paste( Sys.time() ) , "\n" )
	}		
}		
			
