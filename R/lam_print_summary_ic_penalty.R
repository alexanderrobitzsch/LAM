## File Name: lam_print_summary_ic_penalty.R
## File Version: 0.04
## File Last Change: 2020-05-07

lam_print_summary_ic_penalty <- function(ic, crit, digits)
{
    res <- round( ic[[crit]] - ic$deviance, digits)
    return(res)
}
