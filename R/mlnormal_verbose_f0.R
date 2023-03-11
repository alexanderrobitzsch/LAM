## File Name: mlnormal_verbose_f0.R
## File Version: 0.05
## File Last Change: 2018-11-23

mlnormal_verbose_f0 <- function(verbose, disp)
{
    if (verbose){
        cat(disp)
        cat("Preprocess data   ", paste( Sys.time() ), "\n" )
        utils::flush.console()
    }
}

