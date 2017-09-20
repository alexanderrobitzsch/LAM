## File Name: mlnormal_summary_round_helper.R
## File Version: 0.13
## File Last Change: 2017-09-19 14:07:10

mlnormal_summary_round_helper <- function( obji , digits , exclude = NULL, print=TRUE,
		rownames_null=TRUE)
{
	NC <- ncol(obji)
	ind <- 1:NC
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
		invisible(obji)	
	} else {
		return(obji)
	}
}
