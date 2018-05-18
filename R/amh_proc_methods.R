## File Name: amh_proc_methods.R
## File Version: 0.03


amh_proc_methods <- function(amh_summary, mcmcobj)
{
    coef1 <- amh_summary$MAP[-1]
    names(coef1) <- amh_summary$parameter[-1]
    vcov1 <- stats::var(mcmcobj)[-1,-1]
    #--- output
    res <- list( coef1 = coef1, vcov1 = vcov1)
    return(res)
}
