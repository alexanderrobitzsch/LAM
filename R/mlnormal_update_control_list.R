## File Name: mlnormal_update_control_list.R
## File Version: 0.03
## File Last Change: 2018-05-18

mlnormal_update_control_list <- function(control, control0)
{
    # control0 is the output list
    # the elements in control are written into control0
    if ( ! is.null(control) ){
        N <- length(control)
        for (nn in 1:N){
            control0[[ names(control)[nn] ]] <- control[[nn]]
        }
    }
    return(control0)
}
