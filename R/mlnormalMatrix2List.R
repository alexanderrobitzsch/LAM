

mlnormalMatrix2List <- function( G , mat , freq_id ){
	matlist <- as.list(1:G)
	for (gg in 1:G){
		ind_gg <- seq( freq_id[ gg , "start"] , freq_id[ gg , "end"] )
		matlist[[gg]] <- mat[ ind_gg , seq( 1 , freq_id[ gg, "dim_id"] ) ]
	}
	return(matlist)
}
