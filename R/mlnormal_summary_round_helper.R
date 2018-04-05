## File Name: mlnormal_summary_round_helper.R
## File Version: 0.17

mlnormal_summary_round_helper <- function( obji , digits , exclude = NULL, print=TRUE,
		rownames_null=TRUE, start_index = NULL, end_index = NULL )
{
	NC <- ncol(obji)
	if ( is.null(start_index) ){
		start_index <- 1
	}
	if ( is.null(end_index) ){
		end_index <- NC
	}	
	ind <- seq( start_index, end_index )
	if ( ! is.null(exclude) ){
		if ( is.numeric(exclude) ){
			exclude <- colnames(obji)[exclude]
		}	
		ind2 <- which( colnames(obji) %in% exclude )
		ind <- setdiff( ind , ind2 )
	}	
	obji[,ind] <- round( obji[,ind] , digits )
	if (rownames_null){
		rownames(obji) <- NULL
	}
	if (print){
		print(obji)
		# invisible(obji)	
	} else {
		return(obji)
	}
}
