
mlnormal_verbose_f1 <- function(verbose, disp , iter)
{
    if ( verbose ){
	    cat(disp)	
	    cat("Iteration" , iter , "   " , 
		              paste( Sys.time() ) , "\n" )
	}		
}		
			
