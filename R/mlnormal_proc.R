## File Name: mlnormal_proc.R
## File Version: 0.36

###############################################################
# preprocessing for mlnormal function
mlnormal_proc <- function( id, X, y, beta, theta, REML_shortcut, method,
        Z_list, Z_index, variance_shortcut, use_Rcpp )
{

zz0 <- Sys.time()
    #*** number of groups (ids)
    G <- length( unique(id))
    #*** rename identifiers
    id <- match( id, unique(id))
    if ( sum( diff(id) < 0 ) > 0 ){
            stop("id vector must be ordered!")
    }

    #**** reorder identifiers for faster computation
    reorder_obs <- freq_id <- NULL
    do_compute <- rep( TRUE, G )
# cat("-- before variance_shortcut") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1

    rcpp_args <- NULL
    if (variance_shortcut){
        res <- mlnormal_proc_variance_shortcut( id=id, y=y, X=X, Z_list=Z_list,
                   Z_index=Z_index, use_Rcpp=use_Rcpp, G=G  )
        id <- res$id
        y <- res$y
        X <- res$X
        Z_list <- res$Z_list
        Z_index <- res$Z_index
        freq_id <- res$freq_id
        do_compute <- res$do_compute
        rcpp_args <- res$rcpp_args
        # reorder_obs <- res$reorder_obs
    }

# cat("-- after variance_shortcut") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1

    #*** create id_list
    id_list <- as.list(1:G)
    for (gg in 1:G){
        ind <- which( id==gg  )
        id_list[[gg]] <- ind
    }
    # create X list
    X_list <- as.list(1:G)
    y_list <- as.list(1:G)
    for (gg in 1:G){
        ind <- id_list[[gg]]
        X_list[[gg]] <- X[ ind, ]
        y_list[[gg]] <- y[ ind ]
    }

    N <- length(y)
    id_dim <- rowsum( rep(1,N), id )[,1]

    #--- objects of beta if they are not existing
    if ( is.null(beta) ){
        beta <- rep( 0, ncol(X) )
    }


    #--- names
    NT <- length(theta)
    NB <- length(beta)
    if ( is.null( names( theta )) ){
        names(theta) <- paste0( "theta", 1:NT )
    }
    if ( is.null( names( beta )) ){
        names(beta) <- colnames(X)
        ind <- names(beta)==""
        names(beta)[ ind ] <- paste0( "beta", 1:NB )[ind]
    }
    #--- REML
    REML <- if ( method=="REML" ){ TRUE } else { FALSE }

    #--- REML_shortcut
    if ( is.null(REML_shortcut) ){
        REML_shortcut <- TRUE
    }

    #--- Vector with descriptions
    des_ll <- "Log likelihood"
    if ( REML ){ des_ll <- "Restricted log likelihood" }

    des_method <- "Maximum Likelihood Estimation"
    if ( REML ){ des_method <- "Restricted Maximum Likelihood Estimation" }

    #-- vector
    descriptions <- c( "log_like_verbose"=des_ll, des_method=des_method )

    if ( ! variance_shortcut ){
        use_Rcpp <- FALSE
    }

    #--- output
    res <- list( "id_list"=id_list, "G"=G, "y_list"=y_list,
                "X_list"=X_list, "N"=N, "NB"=NB,
                X=as.matrix(X), y=as.matrix(y),
                id=id,
                "NT"=NT, "theta"=theta,Z_list=Z_list, Z_index=Z_index,
                freq_id=freq_id, do_compute=do_compute,
                "beta"=beta, REML=REML, REML_shortcut=REML_shortcut,
                descriptions=descriptions, rcpp_args=rcpp_args,
                use_Rcpp=use_Rcpp
                    )
    return(res)
}
###############################################################

# cat("-- end preproc") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1
