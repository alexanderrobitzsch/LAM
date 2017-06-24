
mlnormal_adjust_numdiff_parameter <- function( h , pars ){
    #** select h parameters according to size of parameters
    abs_par <- abs(pars)       
    hvec <- h * ifelse( abs_par > 1 , abs_par , 1 )
	return(hvec)
}
