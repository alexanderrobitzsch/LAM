## File Name: mlnormal_proc_variance_shortcut_Z_R.R
## File Version: 0.114


mlnormal_proc_variance_shortcut_Z_R <- function( Z_list, Z_index, G, freq_id)
{

    gg0 <- freq_id[ 1, 'orig_id' ]
    Z_list_gg0 <- Z_list[[gg0]]
    NZ <- length(Z_list_gg0)
    Z_index_gg0 <- Z_index[gg0,,]
    for (gg in 2L:G){
        gg1 <- freq_id[ gg, 'orig_id' ]
        Z_list_gg <- Z_list[[gg1]]
        Z_index_gg <- Z_index[gg1,,]
        if ( freq_id[ gg, 'update_dim']==0 ){
            crit1 <- mlnormal_equal_matrix( mat1=Z_index_gg0, mat2=Z_index_gg )
            crit2 <- mlnormal_equal_list_matrices( list_mat1=Z_list_gg0,
                                list_mat2=Z_list_gg, dim_list=NZ)
            crit <- 1 * crit1 * crit2
            if ( crit==0 ){
                freq_id[ gg, 'update_dim' ] <- 1
            }
        }
        Z_list_gg0 <- Z_list_gg
        Z_index_gg0 <- Z_index_gg
        gg0 <- gg1
    }

    #--- output
    res <- list( freq_id=freq_id, rcpp_args=NULL)
    return(res)
}

