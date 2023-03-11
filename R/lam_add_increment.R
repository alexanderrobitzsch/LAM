## File Name: lam_add_increment.R
## File Version: 0.02
## File Last Change: 2019-05-06

lam_add_increment <- function(vec, pos, val)
{
    vec[pos] <- vec[pos] + val
    return(vec)
}
