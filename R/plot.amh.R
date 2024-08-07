## File Name: plot.amh.R
## File Version: 0.256


######################################################
# plot results of objects of class amh
plot.amh <- function( x, conflevel=.95,
    digits=3, lag.max=.1, col.smooth="red", lwd.smooth=2,
    col.split="blue", lwd.split=2, lty.split=1,
    col.ci="orange", cex.summ=1, ask=FALSE, ... )
{

    # x <- mcmcobj
    object <- x    # rename x into object
    mcmcobj <- (object$mcmcobj)
    lag.max <- round(nrow(mcmcobj) * lag.max )
    round.summ <- digits

    # mcmcobj <- (object$mcmcobj)[[1]]
    lag.max <- min( nrow(mcmcobj), lag.max )
    # index vector
    a1 <- attr(mcmcobj,'mcpar')
    iterindex <- seq(a1[1], a1[2], a1[3] )

    smcmcobj <- object$amh_summary
    VV <- ncol(mcmcobj)
    ci.quant <- - stats::qnorm( (1-conflevel)/2 )
    graphics::par( mfrow=c(2,2))

    for (vv in 1L:VV){
        x.vv <- as.vector( mcmcobj[,vv] )
        parm.vv <- colnames(mcmcobj)[vv]
        sparm.vv <- smcmcobj[ smcmcobj$parameter==parm.vv, ]

        #***
        # traceplot
        graphics::plot( iterindex, x.vv, type='l',
                    main=paste0( 'Traceplot of ', parm.vv ),
                    xlab='Iterations', ylab='', ... )
        x1 <- as.numeric( x.vv )
        xmin <- min(x1)
        xmax <- max(x1)

        # 3 splits for chain
        NX <- length(x.vv)
        NS <- NX / 3
        g1 <- round(seq( 0, NX, length=4 ))

        for (ii in 1L:3){
            i1 <- seq(g1[ii]+1, g1[ii+1] )
            m1 <- mean( x1[ i1 ] )
            graphics::lines( iterindex[ i1 ], rep( m1, length(i1) ), col=col.split,
                    lwd=lwd.split, lty=lty.split )
        }
        # l1 <- loess( x1 ~ iterindex )$fitted
        l1 <- lam_moving_average(x1, B=round( lag.max / 2 ), fill=FALSE)
        graphics::lines( iterindex,l1, col=col.smooth, lwd=lwd.smooth )
        # graphics::lines( iterindex, x.vv )

        #**** density estimate
        graphics::plot( stats::density( x.vv ), main=paste0( 'Density of ', parm.vv ) )

        c1 <- stats::quantile( x1, ( 1 - conflevel  ) / 2 )
        c2 <- stats::quantile( x1, 1 - ( 1 - conflevel  ) / 2 )
        graphics::lines( c(c1,c2), c(0,0), col=col.ci, lwd=3 )
        graphics::points( sparm.vv$Mean, 0, pch=17, col=col.ci, cex=1.5)
        #*** plot autocorrelation function
        # stats::acf( x.vv, lag.max=lag.max,
        #    main=paste0( 'Autocorrelation of ', parm.vv ) )
        #@@@@@@
        mtitle <- paste0( 'Autocorrelation of ', parm.vv )
        m1 <- stats::acf( x.vv, lag.max=lag.max, plot=FALSE)
        acf1 <- m1$acf[,1,1]
        # iter_vv <- c(0,iterindex[ 1L:lag.max])
        thin_lag <- round( mean( diff(iterindex) ) )
        iter_vv <- seq( 0, lag.max )*thin_lag
        # blue dashed line at
        bd <- .05
        ylim <- c( min( acf1, - bd), 1 )
        graphics::plot( iter_vv, acf1, xlab='Lag', ylab='ACF', main=mtitle, type='n',
                ylim=ylim)
        NL <- length(iter_vv)
        for (hh in 1L:NL){
            graphics::lines( rep( iter_vv[hh],2), c(0, acf1[hh]) )
        }
        graphics::abline( h=bd, col='blue', lty=2)
        graphics::abline( h=-bd, col='blue', lty=2)

        #*** numerical summary
        graphics::plot( c(0,1), c(0,1), axes=FALSE, xlab='', ylab='',
                    main=paste0( 'Summary of ', parm.vv ), type='n', ...)
        x0 <- 0 ; y0 <- 0
        heights.summ=c( .05,  .15, .25,  .35, .45, .55, .65, .75)
        graphics::text( x0 + .0015, y0 + heights.summ[8], 'Posterior Mean=',
                                cex=cex.summ, pos=4)
        graphics::text( x0 + .5, y0 + heights.summ[8],
                paste0( format.numb( x=mean( x1 ), digits=round.summ)  ), pos=4 )
        hvv <- heights.summ[7]
        graphics::text( x0 + .0015, y0 + hvv, 'Posterior Mode=', cex=cex.summ, pos=4)
        graphics::text( x0 + .5, y0 + hvv,
                paste0( format.numb( x=sparm.vv$MAP, digits=round.summ)  ), pos=4 )

        graphics::text( x0 + .0015, y0 + heights.summ[6], 'Posterior SD=',
                            cex=cex.summ, pos=4)
        graphics::text( x0 + .5, y0 + heights.summ[6],
                paste0( format.numb( x=stats::sd(x1), digits=round.summ)  ), pos=4 )

        hvv <- heights.summ[5]
        graphics::text( x0 + .0015, y0 + hvv,
                            paste( round(100*conflevel ),
                                    '% Credibility Interval=',sep=''),
                            cex=cex.summ, pos=4 )

        hvv <- heights.summ[4]
        ci.lower <- format.numb( stats::quantile( x1, ( 1 - conflevel  ) / 2 ),
                                digits=round.summ )
        ci.upper <- format.numb( stats::quantile( x1, 1- ( 1 - conflevel  ) / 2 ),
                                digits=round.summ )
        graphics::text( x0 + .25, y0 + hvv,
                            paste( '[', ci.lower,    ',', ci.upper, ']',  sep=''),
                            cex=cex.summ, pos=4)
        hvv <- heights.summ[3]
        graphics::text( x0 + .0015, y0 + hvv, 'Rhat=', cex=cex.summ, pos=4)
        graphics::text( x0 + .5, y0 + hvv,
                paste0( format.numb( x=sparm.vv$Rhat, digits=3)  ), pos=4 )
        hvv <- heights.summ[2]
        graphics::text( x0 + .0015, y0 + hvv, 'SERatio=', cex=cex.summ, pos=4)
        graphics::text( x0 + .5, y0 + hvv,
                paste0( format.numb( x=sparm.vv$SERatio, digits=3)  ), pos=4 )

        hvv <- heights.summ[1]
        graphics::text( x0 + .0015, y0 + hvv, 'Effective Sample Size=',
                        cex=cex.summ, pos=4)
        graphics::text( x0 + .705, y0 + hvv,
                paste0( format.numb( x=sparm.vv$effSize, digits=1)  ), pos=4 )
        graphics::par(ask=ask)
    }
    graphics::par(mfrow=c(1,1))
}


#--- format numbers
format.numb <- function( x, digits )
{
    a1 <- round( x, digits ) + 10^{-(digits +1 ) }
    a1 <- substring( a1, 1, nchar(a1) - 1 )
    return(a1)
}

