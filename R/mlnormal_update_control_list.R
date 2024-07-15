## File Name: mlnormal_update_control_list.R
## File Version: 0.041

mlnormal_update_control_list <- function(control, control0)
{
    # control0 is the output list
    # the elements in control are written into control0
    if ( ! is.null(control) ){
        N <- length(control)
        for (nn in 1L:N){
            control0[[ names(control)[nn] ]] <- control[[nn]]
        }
    }
    return(control0)
}
