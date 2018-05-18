## File Name: mlnormal_update_beta_XVX_Rcpp.R
## File Version: 0.14

################################################################
# update beta
mlnormal_update_beta_XVX_Rcpp <- function( NB , Z_index , G ,
        V1_list , X_list , y_list , rcpp_args , X , y)
{
    res <- lam_rcpp_mlnormal_update_beta( dim_id=rcpp_args$dim_id,
                startIndex=rcpp_args$start, endIndex=rcpp_args$end ,
                G=G, X=X, y=y, V1=rcpp_args$V1 )
    #--- output
    res$XV_list <- NULL
    return(res)
}
