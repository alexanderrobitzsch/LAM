## File Name: mlnormal_information_matrix_reml.R
## File Version: 0.194


#--- compute information matrix under REML
mlnormal_information_matrix_reml <- function( V1zero,
        V1_list, id_list, G, X, D1_V_list, XVX, NT, N )
{

    zz0 <- Sys.time()
    #**** compute information matrix for REML
    # V1=V^-1
    # P=V1 - V1 * X * (X'V1X)^-1 * X' V1
    # V1*D1 is already computed

    V1 <- mlnormal_fill_matrix_from_list( V1=V1zero, V1_list=V1_list,
                    id_list=id_list, G=G )
    dim_X <- dim(X)
    # XV1=t(X) * V1
    XV1 <- matrix( 0, nrow=dim_X[2], ncol=dim_X[1] )
    for (gg in 1L:G){
        ind_gg <- id_list[[gg]]
        XV1[, ind_gg] <- crossprod( X[ ind_gg, ], V1_list[[gg]] )
    }
    # V1 * X=t( t(X) * t(V1) )=t( t(X) * V1 )=t( XV1 )
    XV2 <- solve( XVX ) %*% XV1
    #            P <- V1 - V1 %*% X %*% XV2
    P <- V1 - crossprod( XV1, XV2 )

    #-------------- computation without loops -------------------------------
    # P dV/d theta_i
    # tr( P DV_i P DV_j )=tr( DV_i P DV_j P )
    theta_infomat <- matrix( 0, nrow=NT, ncol=NT)

    for (pp in 1L:NT){
        for (qq in pp:NT){
            D1_V_pp <- mlnormal_fill_matrix_from_list( V1=V1zero, V1_list=D1_V_list[[pp]],
                                  id_list=id_list, G=G )
            D1_V_qq <- mlnormal_fill_matrix_from_list( V1=V1zero, V1_list=D1_V_list[[qq]],
                                  id_list=id_list, G=G )
            theta_infomat[pp,qq] <- 0.5 * sum( ( P %*% D1_V_pp ) * ( P %*% D1_V_qq ) )
            theta_infomat[qq,pp] <- theta_infomat[pp,qq]
        }
    }

    #------------------- computation with loops ------------------------
    theta_infomat <- matrix( 0, nrow=NT, ncol=NT)
    for (pp in 1L:NT){
        for (qq in pp:NT){
            P0 <- matrix( 0, nrow=N, ncol=N )
            for (gg in 1L:G){
                for (hh in 1L:G){
                    ind_gg <- id_list[[gg]]
                    ind_hh <- id_list[[hh]]
                    P0[ ind_gg, ind_hh ] <- D1_V_list[[pp]][[gg]] %*%
                                P[ ind_gg, ind_hh ] %*% D1_V_list[[qq]][[hh]]
                }
            }
            theta_infomat[pp,qq] <- 0.5 * sum(  P0 * P )
            theta_infomat[qq,pp] <- theta_infomat[pp,qq]
        }
    }

    #-- output
    res <- list( theta_infomat=theta_infomat )
    return(res)
}

