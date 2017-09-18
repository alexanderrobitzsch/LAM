## File Name: mlnormal_eval_penalty.R
## File Version: 0.03
## File Last Change: 2017-01-18 11:02:49


mlnormal_eval_penalty <- function( beta , theta , penalty_pars )
{
	penalty_beta <- penalty_pars$lambda_beta * penalty_pars$weights_beta * abs(beta)
	penalty_theta <- penalty_pars$lambda_theta * penalty_pars$weights_theta * abs(theta)
	res <- list( penalty_beta = sum(penalty_beta) , 
						penalty_theta = sum(penalty_theta))						
	return(res)
}
