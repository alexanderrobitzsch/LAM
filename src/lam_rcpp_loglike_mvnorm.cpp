//// File Name: lam_rcpp_loglike_mvnorm.cpp
//// File Version: 0.889


// [[Rcpp__interfaces(r, cpp)]]  substitute "__" by "::"
// for exporting Rcpp functions for use in another package


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

using namespace Rcpp;
using namespace arma;


///********************************************************************
//** quadratic form
double lam_rcpp_quadratic_form( arma::colvec w, arma::mat A)
{
    int H = A.n_rows;
    double quadval=0;
    double fac=1;
    for (int ii=0; ii<H; ii++){
        for (int jj=ii; jj<H; jj++){
            if (ii==jj){ fac = 1; } else { fac = 2;}
            quadval += fac * w(ii,0) * A(ii,jj) * w(jj,0);
        }
    }
    return quadval;
}

///********************************************************************
//** log-likelihood multivariate normal distribution
///** lam_rcpp_loglike_mvnorm
// [[Rcpp::export]]
double lam_rcpp_loglike_mvnorm( arma::colvec M, arma::mat S, arma::colvec mu,
        arma::mat Sigma, double n, bool use_log, double lambda, bool ginv, double eps )
{
    int p = Sigma.n_rows;
    // redefine matrix
    arma::mat sigma0(p,p);
    for (int rr=0; rr<p; rr++){
        for (int cc=0; cc<p; cc++){
            sigma0(rr,cc) = Sigma(rr,cc);
        }
    }

    // regularization of input covariance matrix
    if ( lambda > 0){
        double w = n /( n + lambda ) ;
        for (int ii=0; ii<p; ii++){
            for (int jj=ii+1; jj<p;jj++){
                sigma0(ii,jj) = w*sigma0(ii,jj);
                sigma0(jj,ii) = sigma0(ii,jj);
            }
        }
    }

    // inverse Sigma matrix
    arma::mat Sigma1(p,p);
    if (ginv){
        Sigma1 = arma::pinv(sigma0);
    } else {
        Sigma1 = arma::inv(sigma0);
    }

    // determinant of Sigma
    double det_Sigma = arma::det(sigma0);
    if ( det_Sigma < eps){ det_Sigma = eps ; }

    // quadratic form
    arma::colvec mudiff = M - mu;
    double quadval = lam_rcpp_quadratic_form( mudiff, Sigma1);

    // trace( Sigma1 * S)
    // sum( diag( Sigma1 %*% S ) )
    double traceval=0;
    int p1 = Sigma1.n_rows;
    for (int ii=0; ii<p1; ii++){
        for (int hh=0; hh<p1; hh++){
            traceval += Sigma1(ii,hh) * S(hh,ii);
        }
    }

    // l1 <- - p * log( 2*pi) - t( M - mu ) %*% Sigma1 %*% ( M - mu ) -
    //             log( det_Sigma )  - sum( diag( Sigma1 %*% S ) )
    double pi1 = 3.14159265358979;
    double l1 = - p * std::log(2*pi1) - quadval - std::log( det_Sigma ) - traceval;
    double ll = n/2 * l1;
    if (! use_log){ ll = std::exp(ll); }

    //-- output
    return ll;
}
///********************************************************************


///********************************************************************
//** extract submatrix
arma::mat lam_rcpp_loglike_mvnorm_na_pattern_extract_submatrix(
            Rcpp::IntegerVector varindex, arma::mat A )
{
    int NS = varindex.size();
    arma::mat S0(NS,NS);
    int ii0=0;
    int hh0=0;
    for (int ii=0; ii<NS; ii++){
        for (int hh=0; hh<NS; hh++){
            ii0 = varindex[ii]-1;
            hh0 = varindex[hh]-1;
            S0(ii,hh) = A(ii0, hh0);
        }
    }
    return S0;
}

///********************************************************************
//** extract subvector
arma::colvec lam_rcpp_loglike_mvnorm_na_pattern_extract_subvector(
            Rcpp::IntegerVector varindex, arma::colvec w )
{
    int NS = varindex.size();
    arma::colvec S0(NS,1);
    int ii0=0;
    for (int ii=0; ii<NS; ii++){
        ii0 = varindex[ii]-1;
        S0(ii,0) = w(ii0, 0);
    }
    return S0;
}

///********************************************************************
//** log-likelihood multivariate normal distribution
///** lam_rcpp_loglike_mvnorm_na_pattern_rcpp
// [[Rcpp::export]]
double lam_rcpp_loglike_mvnorm_na_pattern_rcpp( Rcpp::List suff_stat, arma::colvec mu,
            arma::mat Sigma, bool use_log, double lambda, bool ginv, double eps )
{
    int NP = suff_stat["NP"];
    Rcpp::List varindex = suff_stat["varindex"];
    Rcpp::List S = suff_stat["S"];
    Rcpp::List M = suff_stat["M"];
    Rcpp::List nobs = suff_stat["nobs"];

    double nobs_pp;
    arma::mat Sigma_pp;
    arma::colvec mu_pp;
    double ll = 0;

    for (int pp=0; pp<NP; pp++){
        Rcpp::IntegerVector varindex_pp = varindex[pp];
        arma::mat S_pp = S[pp];
        arma::colvec M_pp = M[pp];
        nobs_pp = nobs[pp];
        Sigma_pp = lam_rcpp_loglike_mvnorm_na_pattern_extract_submatrix( varindex_pp, Sigma );
        mu_pp = lam_rcpp_loglike_mvnorm_na_pattern_extract_subvector( varindex_pp, mu );
        ll += lam_rcpp_loglike_mvnorm( M_pp, S_pp, mu_pp, Sigma_pp, nobs_pp,
                use_log, lambda, ginv, eps );
    }

    //-- output
    return ll ;
}
///********************************************************************




//    return Rcpp::List::create(
//            Rcpp::Named("sigma0") = sigma0,
//            Rcpp::Named("Sigma1") = Sigma1
//            );
