## File Name: mlnormal_verbose_f2.R
## File Version: 0.02
## File Last Change: 2017-01-18 11:02:50

mlnormal_verbose_f2 <- function(verbose, disp , iter, descriptions ,
		objfun , objfun0 , beta_change , theta_change )
{
    if (verbose){			
		cat( paste0( "   " , descriptions["log_like_verbose2"] , 
			         " = "  , round( objfun , 4 ) , 
			if (iter > 1){ " | Change = " } else {""} ,
			if( iter > 1){ round(  objfun - objfun0 , 6 )} else { ""}	,"\n",sep="") )
		cat( paste0( "    Maximum beta parameter change = " , 
					paste0( round( beta_change  ,6) , collapse=" " ) , "\n" , sep=""))
		cat( paste0( "    Maximum theta parameter change = " , 
					paste0( round( theta_change  ,6) , collapse=" " ) , "\n" , sep=""))				
		utils::flush.console()						
	}	
}		
