## File Name: mlnormal_update_beta_XVX_R.R
## File Version: 0.083


#-- update beta
mlnormal_update_beta_XVX_R <- function( NB, Z_index, G,
        V1_list, X_list, y_list, rcpp_args, X, y)
{
    XVX <- matrix( 0, nrow=NB, ncol=NB)
    XVY <- matrix( 0, nrow=NB, ncol=1)
    dimZ <- dim( Z_index )
    XV_list <- as.list(1L:G)
    for (gg in 1L:G){
        # compute V for group gg
        V_gg1 <- V1_list[[gg]]
        X_gg <- X_list[[gg]]
        y_gg <- y_list[[gg]]
        XV_gg <- crossprod( X_gg, V_gg1    )
        XV_list[[gg]] <- XV_gg
        XVX <- XVX + XV_gg %*% X_gg
        XVY <- XVY + XV_gg %*% y_gg
    }
    #--- output
    res <- list( XVX=XVX, XVY=XVY, XV_list=XV_list )
    return(res)
}
