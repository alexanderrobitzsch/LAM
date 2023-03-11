## File Name: mlnormal_update_beta_GLS.R
## File Version: 0.05
## File Last Change: 2018-11-23



mlnormal_update_beta_GLS <- function( XVX, XVY, control_beta)
{
    XVX <- mlnormal_covmat_add_ridge( covmat=XVX, eps=control_beta$ridge )
    beta <- solve(XVX, XVY )
    return(beta)
}
