## File Name: mlnormal_check_matrix_list_difference.R
## File Version: 0.04
## File Last Change: 2018-11-23

mlnormal_check_matrix_list_difference <- function( matlist1, matlist2 ){
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
