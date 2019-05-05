## File Name: add_increment.R
## File Version: 0.01

add_increment <- function(vec, pos, val)
{
    vec[pos] <- vec[pos] + val
    return(vec)
}
