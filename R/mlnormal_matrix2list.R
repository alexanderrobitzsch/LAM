## File Name: mlnormal_matrix2list.R
## File Version: 0.054


mlnormal_matrix2list <- function( G, mat, freq_id )
{
    matlist <- as.list(1L:G)
    for (gg in 1L:G){
        ind_gg <- seq( freq_id[ gg, 'start'], freq_id[ gg, 'end'] )
        matlist[[gg]] <- mat[ ind_gg, seq( 1, freq_id[ gg, 'dim_id'] ) ]
    }
    return(matlist)
}
