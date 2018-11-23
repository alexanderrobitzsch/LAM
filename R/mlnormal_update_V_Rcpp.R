## File Name: mlnormal_update_V_Rcpp.R
## File Version: 0.17

###################################################################
# update matrix V and its inverse
mlnormal_update_V_Rcpp <- function( Z_index, G, theta,
        Z_list, use_ginverse, variance_shortcut, freq_id ,
        do_compute, rcpp_args )
{
    res <- lam_rcpp_mlnormal_update_V( Z_list=Z_list,
                Z_index=rcpp_args$Z_index, dim_id=rcpp_args$dim_id,
                dim_Z_index=rcpp_args$dim_Z_index, startIndex=rcpp_args$start,
                endIndex=rcpp_args$end, N=rcpp_args$N, max_dim=rcpp_args$max_dim,
                do_compute=rcpp_args$do_compute, theta=as.vector(theta),
                use_ginverse=as.integer(use_ginverse) )
    rcpp_args$V <- res$V
    rcpp_args$V1 <- res$V1
    #--- output
    res <- list( rcpp_args=rcpp_args, V_list=res$V_list,
                        V1_list=res$V1_list )
    return(res)
}
######################################################################


