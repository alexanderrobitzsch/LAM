## File Name: mlnormal_proc_control.R
## File Version: 0.10

mlnormal_proc_control <- function( control_beta, control_theta,
        beta_lower, beta_upper, theta_lower, theta_upper)
{
    # initial control list for beta and theta
    control0 <- list( maxiter=10, conv=1E-4, ridge=1E-6 )

    #*** beta | add arguments to a list
    # set defaults
    control_beta0 <- control0
    # replacements
    control_beta0 <- mlnormal_update_control_list(control=control_beta,
            control0=control_beta0)
    control_beta0$beta_lower <- beta_lower
    control_beta0$beta_upper <- beta_upper

    #*** theta | add arguments to a list
    # set defaults
    control_theta0 <- control0
    # replacements
    control_theta0 <- mlnormal_update_control_list(control=control_theta,
            control0=control_theta0)
    control_theta0$theta_lower <- theta_lower
    control_theta0$theta_upper <- theta_upper

    #--- output
    res <- list( control_beta=control_beta0,
                        control_theta=control_theta0 )
    return(res)
}
