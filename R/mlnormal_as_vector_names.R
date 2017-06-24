

mlnormal_as_vector_names <- function(pars , parnames)
{
	pars <- as.vector(pars)
	names(pars) <- parnames
	return(pars)
}
