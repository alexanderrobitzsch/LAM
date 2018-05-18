## File Name: mlnormal_equal_matrix.R
## File Version: 0.17

mlnormal_equal_matrix <- function( mat1  , mat2 , eps=1E-30){
        res <- all( mat1 == mat2 )
        return(res)
}
####################
#            maxval <- max( abs( mat1 - mat2 ))
#            res <- if ( maxval < eps ){ TRUE } else { FALSE }
#        res <- identical( mat1 , mat2 )
#        res <- all.equal( mat1 , mat2 )
