## File Name: mlnormal_adjust_numdiff_parameter.R
## File Version: 0.02
## File Last Change: 2017-01-18 11:02:49

mlnormal_adjust_numdiff_parameter <- function( h , pars ){
    #** select h parameters according to size of parameters
    abs_par <- abs(pars)       
    hvec <- h * ifelse( abs_par > 1 , abs_par , 1 )
	return(hvec)
}
