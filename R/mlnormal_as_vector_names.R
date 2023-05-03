## File Name: mlnormal_as_vector_names.R
## File Version: 0.03


mlnormal_as_vector_names <- function(pars, parnames)
{
    pars <- as.vector(pars)
    names(pars) <- parnames
    return(pars)
}
