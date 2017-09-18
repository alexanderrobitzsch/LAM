## File Name: mlnormal_as_vector_names.R
## File Version: 0.02
## File Last Change: 2017-01-18 11:02:49


mlnormal_as_vector_names <- function(pars , parnames)
{
	pars <- as.vector(pars)
	names(pars) <- parnames
	return(pars)
}
