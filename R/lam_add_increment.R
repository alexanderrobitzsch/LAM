## File Name: lam_add_increment.R
## File Version: 0.02

lam_add_increment <- function(vec, pos, val)
{
    vec[pos] <- vec[pos] + val
    return(vec)
}
