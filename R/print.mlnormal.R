## File Name: print.mlnormal.R
## File Version: 0.14
## File Last Change: 2019-05-05


# print for mlnormal object
print.mlnormal <- function( x, digits=4, ...)
{

    object <- x
    d1 <- utils::packageDescription("LAM")
    cat( paste( d1$Package, " ", d1$Version, " (", d1$Date, ")", sep=""), "\n\n" )

    cat("Call:\n", paste(deparse(object$CALL), sep="\n", collapse="\n"),
                "\n\n", sep="")

    cat( object$descriptions["des_method"], "\n\n")

    cat( "Number of observations=", object$ic$N, "\n" )
    cat( "Number of clusters=", object$ic$G, "\n\n" )

    cat( "Deviance=", round( object$deviance, 2 ), " | " )
    cat( paste0( object$descriptions["log_like_verbose"]
                    , "=" ), round( -object$deviance/2, 2 ), "\n" )

    cat( "Number of estimated parameters=", object$ic$np, "\n" )
    cat( "  Number of estimated beta parameters=", object$ic$np.beta,
                "\n" )
    cat( "  Number of estimated theta parameters=", object$ic$np.theta,
                "\n\n" )

    if ( ! object$REML ){
        cat( "AIC=", round( object$ic$AIC, 1 ), " | penalty=",
                round( object$ic$AIC - object$ic$deviance,2 ),
                    "   | AIC=-2*LL + 2*p  \n" )
        cat("\n")
    }

    cat("Beta Parameters\n")
    obji <- object$beta
    print( round( obji, digits ) )

    cat("\nTheta Parameters\n")
    obji <- object$theta
    print( round( obji, digits ) )
}
#*******************************************************
