## File Name: mlnormal_update_theta_ml.R
## File Version: 1.11
## File Last Change: 2018-11-23

###################################################
# update theta
mlnormal_update_theta_ml <- function( y, X, beta,
    Z_index, NT, G, id_list, V1_list, D1_V_list, theta,
    P, REML, V1zero, REML_shortcut, V1_D1V_V1_list, XVX,
    variance_shortcut, freq_id, do_compute, prior_args,
    control_theta, iter )
{
zz0 <- Sys.time()
    # calculate residuals
    yresid <- y - X %*% beta
    der <- rep(0,NT)
    do_computation <- TRUE

    # list with V^(-1)%*% d V / d theta_i
    V1_D1V_list <- as.list(1:NT)
    theta0 <- theta

    for (pp in 1:NT){
        deri <- 0
        deri_t2 <- 0
        H11 <- as.list(1:G)
        #----- ! REML
        if ( ( ! REML ) | REML_shortcut  ){
            for (gg in 1:G){
                # gg <- 1
                PV_gg <- V1_list[[gg]]
                if ( do_compute[gg] ){
                    M1 <- PV_gg %*% D1_V_list[[pp]][[gg]]
                    tM1 <- 1/2*sum( diag(M1) )  # tracemat
                }
                H11[[gg]] <- M1
                deri <- deri - tM1
                ind <- id_list[[gg]]
                yr <- yresid[ind]
                deri_t2 <- deri_t2 + 1/2* crossprod( yr, M1 ) %*% PV_gg %*% yr
            }  ## end gg
        V1_D1V_list[[pp]] <- H11
        der[pp] <- deri + deri_t2
        }  # end if ( ! REML )
#cat("### not REML computation ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1

        #--- REML shortcut estimation | somewhat faster
        if (REML_shortcut & REML){
            V1_D1V_V1_pp_list <- V1_D1V_V1_list[[pp]]
            dim_X <- dim(X)
            NX <- dim_X[2]
            N <- dim_X[1]
            G00 <- matrix( 0, nrow=NX, ncol=N)
            for (gg in 1:G){
                ind_gg <- id_list[[gg]]
                G00[, ind_gg] <- crossprod( X[ ind_gg, ], V1_D1V_V1_pp_list[[gg]] )
            }
            G00 <- G00 %*% X
            der_add <- 0.5 * sum( diag( solve(XVX) %*% G00 ) )
            der[pp] <- der[pp] + der_add
        }
# cat("### not REML shortcut (alternative) ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1

        #----- REML
        if ( ! REML_shortcut ){
            D1_V_pp_list <- D1_V_list[[pp]]
#  cat("* compute H_pp (V1)") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1
            N <- dim(P)[1]
            H_pp <- matrix( 0, nrow=N, ncol=N)
            for (gg in 1:G){
                for (hh in 1:G){
                    # hh <- 1
                    ind_gg <- id_list[[gg]]
                    ind_hh <- id_list[[hh]]
                    H_pp[ ind_hh, ind_gg ] <- P[ ind_hh, ind_gg ] %*% D1_V_pp_list[[gg]]
                }
            }
#cat("### ! REML shortcut computation ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1

            if ( ! REML_shortcut ){
                V1_D1V_list[[pp]] <- H_pp
            }
            deri <- -0.5*sum(diag( H_pp ))
#            deri_t2 <- 0.5 * t(y) %*% H_pp %*% P %*% y
            Py <- P %*% y
            deri_t2 <- 0
            for (gg in 1:G){
                ind_gg <- id_list[[gg]]
                Py_gg <- Py[ ind_gg, 1]
                deri_t2 <- deri_t2 + .5* crossprod( Py_gg, D1_V_pp_list[[gg]] ) %*% Py_gg
            }
            der[pp] <- deri + deri_t2
        }   # if REML
# cat("### REML computation ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1

    }  # end parameter pp

# cat("-- start theta_infomat") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1

    #***** compute expected information matrix
    theta_infomat <- matrix( 0, nrow=NT, ncol=NT)
    for (pp in 1:NT){
        for (qq in 1:pp){
            # pp <- 1
            # qq <- 1
            #-------- ML -----------------
            if ( REML_shortcut ){
                for (gg in 1:G){
                    # gg <- 1
                    if ( do_compute[gg] ){
                        #*** remove this line @ARb 2016-07-15
                        # a2 <- V1_D1V_list[[pp]][[gg]] * V1_D1V_list[[qq]][[gg]]
                        # ta2 <- 1/2 * sum(a2)
                        # a2 <- V1_D1V_list[[pp]][[gg]] %*% V1_D1V_list[[qq]][[gg]]
                        # ta2 <- tracemat( a2 ) / 2
                        a2 <- V1_D1V_list[[pp]][[gg]] * t( V1_D1V_list[[qq]][[gg]] )
                        ta2 <- sum(a2) / 2
                    }
                    ### simplify computation of a2
                    theta_infomat[pp,qq] <- theta_infomat[pp,qq] + ta2
                }
            }
            #-------- REML -----------------
            if ( ! REML_shortcut ){
                a23 <- V1_D1V_list[[pp]] * V1_D1V_list[[qq]]
                theta_infomat[pp,qq] <- 1/2 * sum(a23)
            }
            theta_infomat[qq,pp] <- theta_infomat[pp,qq]
        }  # end qq
    }   # end pp
    theta_infomat1 <- theta_infomat

    #---- evaluate prior if provided
    if ( prior_args$use_prior ){
        res <- mlnormal_eval_priors_derivative2( pars=theta,
                    prior=prior_args$prior, h=prior_args$numdiff.parm )
        # der <- der + res$der
        der <- der - res$der
        theta_infomat1 <- theta_infomat1 - res$infomat
    }
    #--- derivative in case of penalties
    if ( prior_args$use_penalty ){
        res_newton <- mlnormal_eval_penalty_update_theta( theta=theta,
                    prior_args=prior_args, iter=iter, der=der,
                    theta_infomat1=theta_infomat1,
                    control_theta=control_theta)
        theta <- res_newton$theta
    }

    #--- Newton step
    if ( prior_args$use_prior | prior_args$use_GLS ){
        res_newton <- mlnormal_update_theta_newton_step( theta=theta, der=der,
                        theta_infomat=theta_infomat1, control_theta=control_theta )
        theta <- res_newton$theta
    }

    # bounds
    theta <- sirt::bounds_parameters( pars=theta, lower=control_theta$theta_lower,
                    upper=control_theta$theta_upper )
    theta_change <- mlnormal_parameter_change( pars=theta, pars0=theta0 )

    # convert to vector
    theta <- mlnormal_as_vector_names(pars=theta, parnames=names(theta0) )

    #--- output
    res <- list( "der"=der, "yresid"=yresid, "V1_D1V_list"=V1_D1V_list,
            "theta_infomat"=res_newton$theta_infomat, "Hinv"=res_newton$Hinv,
            "theta"=theta, "theta_change"=theta_change,
            theta_increment=theta - theta0 )
    return(res)
}
