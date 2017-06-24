
mlnormalCheckMatrixListDifference <- function( matlist1 , matlist2 ){
	G <- length(matlist1)
	val <- 0
	for (gg in 1:G){
		val_gg <- max( abs( matlist1[[gg]] - matlist2[[gg]] ))
		if ( val_gg > val ){
			val <- val_gg
		}
	}
	return(val)
}
