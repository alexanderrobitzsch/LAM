## File Name: summary.pmle.R
## File Version: 0.29

#############################################################
summary.pmle <- function( object , digits=3 , file=NULL ,... )
{
    # open sink
    CDM::osink( file = file , suffix = paste0( "__SUMMARY.Rout") )

    cat("-----------------------------------------------------------------\n")

    #- package and R session
    sirt::sirt_summary_print_package_rsession(pack="LAM")

    cat( object$description , "\n\n")

    #- print call
    sirt::sirt_summary_print_call(CALL=object$CALL)

    cat( "Date of Analysis:" , "\n" )
    cat( "   Start:" , paste( object$time$start ) , "\n" )
    cat( "   End  :" , paste( object$time$end ) , "\n" )
    cat("Computation time:" , print(object$time$end - object$time$start), "\n\n")

    cat( "Optimization function =" , object$optim_fct , "\n" )
    cat( "Gradient provided =" , object$use_grad , "\n" )
    cat( "Convergence Code =" , object$results_optim$convergence , "\n" )
    cat( "CONVERGED =" , object$converged , "\n" )

    cat("-----------------------------------------------------------------\n")
    cat( "Deviance = " , round( object$deviance , 2 ) , "\n" )
    cat( "Log Likelihood = " , round( object$ic$loglike , 2 ) , "\n" )
    cat( "Log Prior = " , round( object$ic$prior , 2 ) , "\n" )
    cat( "Log Posterior = " , round( object$ic$post , 2 ) , "\n\n" )

    cat( "Number of persons = " , object$ic$n , "\n" )
    cat( "Number of estimated parameters = " , object$ic$np , "\n\n" )

    cat( "AIC  = " , round( object$ic$AIC , 0 ) , " | penalty =" , round( object$ic$AIC - object$ic$deviance ,2 ) ,
            "   | AIC = -2*LL + 2*p  \n" )
    cat( "AICc = " , round( object$ic$AICc , 0 ) ," | penalty =" , round( object$ic$AICc - object$ic$deviance ,2 ) )
        cat("    | AICc = -2*LL + 2*p + 2*p*(p+1)/(n-p-1)  (bias corrected AIC)\n" )
    cat( "BIC  = " , round( object$ic$BIC , 0 ) , " | penalty =" , round( object$ic$BIC - object$ic$deviance ,2 ) ,
            "   | BIC = -2*LL + log(n)*p  \n" )
    cat( "CAIC = " , round( object$ic$CAIC , 0 ) ," | penalty =" , round( object$ic$CAIC - object$ic$deviance ,2 ) )
        cat("   | CAIC = -2*LL + [log(n)+1]*p  (consistent AIC)\n\n" )

    cat("-----------------------------------------------------------------\n")
    cat("Prior Summary \n")
    obji <- object$prior_summary
    print(obji)

    cat("-----------------------------------------------------------------\n")
    cat("Parameter Summary \n")
    obji <- object$pmle_summary
    mlnormal_summary_round_helper(obji, digits=digits, print=TRUE, start_index=2)

    # close sink
    CDM::csink( file = file )
}
#############################################################
