//// File Name: lam_rcpp_mlnormal.cpp
//// File Version: 2.28

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

using namespace Rcpp;
using namespace arma;


// user includes

///********************************************************************
///** lam_rcpp_mlnormal_proc_variance_shortcut_Z_restructure
// [[Rcpp::export]]
Rcpp::List lam_rcpp_mlnormal_proc_variance_shortcut_Z_restructure( Rcpp::List Z_list,
    Rcpp::IntegerVector update_dim,
    Rcpp::NumericVector start_orig, Rcpp::NumericVector end_orig,
    Rcpp::NumericVector dim_Z_index, Rcpp::NumericVector Z_index,
    Rcpp::NumericVector orig_id, Rcpp::NumericVector dim_id )
{
    int G = dim_Z_index[0];
    int NM = dim_Z_index[1];
    int NP = dim_Z_index[2];

    // copy of update_dim
    Rcpp::NumericMatrix update_dim2(G,1);
    update_dim2(_,0) = update_dim;

    int orig_gg=1;
    int orig_gg1=1;
    int dim_gg=1;
    double Z1=0.0;
    double Z2=0.0;
    double val=0.0;

    //----- compare Z_index
    for (int gg = 0; gg < G; gg++){
        orig_gg = orig_id[gg];
        if ( update_dim2(gg,0) == 0){
            orig_gg1 = orig_id[gg-1];
        }
        for( int mm = 0; mm < NM; mm++){
            if ( update_dim2(gg,0) == 0){
                for( int pp = 0; pp < NP; pp++){
                    Z1 = Z_index[ orig_gg + mm*G + pp*G*NM - 1];
                    Z2 = Z_index[ orig_gg1 + mm*G + pp*G*NM - 1];
                    if ( Z1 != Z2 ){
                        update_dim2(gg,0) = 1;
                    }
                }  // end parameter pp
            }
        }  // end matrices mm
    }  // end groups gg

    double eps = 1E-15;
    Rcpp::NumericMatrix Z_gg_mm, Z_gg1_mm;
    //----- compare Z_list
    for (int gg=0; gg < G; gg++){
        orig_gg = orig_id[gg];
        if ( update_dim2(gg,0) == 0){
            orig_gg1 = orig_id[gg-1];
        }
        if ( update_dim2(gg,0) == 0 ){
            Rcpp::List Z_gg1 = Rcpp::as<Rcpp::List>( Z_list[ orig_gg1 - 1] );
            Rcpp::List Z_gg = Rcpp::as<Rcpp::List>( Z_list[ orig_gg - 1] );
            dim_gg = dim_id[gg];
            if ( update_dim2(gg,0) == 0 ){
                for (int mm=0; mm < NM; mm++){
                    Rcpp::NumericMatrix Z_gg1_mm = Rcpp::as<Rcpp::NumericMatrix>( Z_gg1[mm] );
                    Rcpp::NumericMatrix Z_gg_mm = Rcpp::as<Rcpp::NumericMatrix>( Z_gg[mm] );
                    for (int rr=0; rr < dim_gg; rr++){
                        for (int cc=rr; cc < dim_gg; cc++){
                            val = std::abs( Z_gg1_mm(rr,cc) - Z_gg_mm(rr,cc) );
                            if ( val > eps ){
                                update_dim2(gg,0) = 1;
                            }
                        }  // end cc
                    }  // end rr
                }   // end mm
            }
        }
    }  // end gg

    //----- OUTPUT
    return Rcpp::List::create(
                Rcpp::Named("NM") = NM,
                Rcpp::Named("NP") = NP,
                Rcpp::Named("Z_gg") = Z_gg_mm,
                Rcpp::Named("update_dim") = update_dim2
        );
}
///********************************************************************

///********************************************************************
///** lam_rcpp_mlnormal_proc_variance_shortcut_XY_restructure
// [[Rcpp::export]]
Rcpp::List lam_rcpp_mlnormal_proc_variance_shortcut_XY_restructure(
    Rcpp::NumericMatrix freq_id, Rcpp::NumericVector y,
    Rcpp::NumericMatrix X, int G )
{
    int N = X.nrow();
    int V = X.ncol();
    Rcpp::NumericMatrix X1(N,V);
    Rcpp::NumericVector y1(N);
    int hh=0;
    int min_gg=0;
    int max_gg=0;
    for (int gg = 0; gg < G; gg++){
        min_gg = freq_id(gg,2) - 1;
        max_gg = freq_id(gg,3);
        for (int mm = min_gg; mm < max_gg; mm++){
            y1[hh] = y[mm];
            for (int vv=0; vv < V; vv++){
                X1(hh,vv) = X(mm,vv);
            }
            hh ++;
        }
    }

    //***** OUTPUT
    return Rcpp::List::create(
            Rcpp::Named("N") = N,
            Rcpp::Named("V") = V,
            Rcpp::Named("X") = X1,
            Rcpp::Named("y") = y1
        );
}
///********************************************************************

///********************************************************************
///** lam_rcpp_mlnormal_update_V
// [[Rcpp::export]]
Rcpp::List lam_rcpp_mlnormal_update_V( Rcpp::List Z_list,
    Rcpp::NumericVector Z_index, Rcpp::NumericVector dim_id,
    Rcpp::NumericVector dim_Z_index, Rcpp::NumericVector startIndex,
    Rcpp::NumericVector endIndex, int N,
    int max_dim, Rcpp::NumericVector do_compute,
    Rcpp::NumericVector theta, int use_ginverse )
{
    int G = dim_id.size();
    int NM = dim_Z_index[1];
    int NP = dim_Z_index[2];

    Rcpp::NumericMatrix V( N, max_dim );
    Rcpp::NumericMatrix V1( N, max_dim );
    Rcpp::List V_list(G);
    Rcpp::List V1_list(G);
    arma::mat V_gg;
    arma::mat V1_gg;
    Rcpp::List Z_gg;
    Rcpp::NumericMatrix Z_gg_mm;
    int dim_gg = 0;
    double val, zval, pow_gg_mm_pp;
    double eps = 1E-15;

    for (int gg = 0; gg < G; gg++){
        if ( do_compute[gg] == 1 ){
            dim_gg = dim_id[gg];
            Z_gg = Rcpp::as<Rcpp::List>( Z_list[gg] );
            V_gg = arma::zeros<arma::mat>(dim_gg,dim_gg);
            pow_gg_mm_pp = 0;
            for (int mm = 0; mm <NM; mm++){
                Z_gg_mm = Rcpp::as<Rcpp::NumericMatrix>( Z_gg[mm] );
                val = 1;
                for (int pp = 0; pp < NP; pp ++){
                    pow_gg_mm_pp = Z_index[ gg + mm*G + pp*G*NM  ];
                    if ( pow_gg_mm_pp > 0 ){
                        val =  std::pow( theta[pp], pow_gg_mm_pp ) * val;
                    }
                }
                for (int rr=0;rr<dim_gg;rr++){
                    for (int cc=0;cc<dim_gg;cc++){
                        zval = Z_gg_mm(rr,cc);
                        if (rr <= cc ){
                            if ( ( zval > eps ) | ( zval < - eps ) ){
                                V_gg(rr,cc) = V_gg(rr,cc) + val* zval;
                            }
                        } else {
                            V_gg(rr,cc) = V_gg(cc,rr);
                        }
                    }
                }
            }
            // calculate inverse
            if (use_ginverse){
                V1_gg = arma::pinv( V_gg );
            } else {
                V1_gg = arma::inv( V_gg );
            }
        }
        // fill matrices V and V1
        for( int rr=0; rr < dim_gg; rr++){
            for( int cc=0; cc < dim_gg; cc++){
                V( startIndex[gg] - 1 + rr, cc ) = V_gg( rr, cc );
                V1( startIndex[gg] - 1 + rr, cc ) = V1_gg( rr, cc );
            }
        }
        // fill list
        V_list[gg] = V_gg;
        V1_list[gg] = V1_gg;
    }

    //***** OUTPUT
    return Rcpp::List::create(
            Rcpp::Named("V") = V,
            Rcpp::Named("V1") = V1,
            Rcpp::Named("V_list") = V_list,
            Rcpp::Named("V1_list") = V1_list
        );
}
///********************************************************************

///********************************************************************
///** lam_rcpp_mlnormal_update_beta
// [[Rcpp::export]]
Rcpp::List lam_rcpp_mlnormal_update_beta( Rcpp::NumericVector dim_id,
    Rcpp::NumericVector startIndex, Rcpp::NumericVector endIndex,
    int G, Rcpp::NumericMatrix X, Rcpp::NumericVector y,
    Rcpp::NumericMatrix V1 )
{
    int NB = X.ncol();
    Rcpp::NumericMatrix XVX(NB,NB);
    Rcpp::NumericMatrix XVY(NB,1);
    int dim_gg=0;
    int rr1=0;
    int cc1=0;
    for (int pp=0; pp < NB; pp++){
        for (int qq=pp; qq < NB; qq++){
            XVX(pp,qq) = 0;
            if (pp==qq){
                XVY(qq,0) = 0;
            }
            for (int gg=0; gg <G; gg++){
                dim_gg = dim_id[gg];
                for (int rr=0; rr < dim_gg; rr++){
                    for (int cc=0; cc < dim_gg; cc++){
                        rr1 = startIndex[gg] + rr - 1;
                        cc1 = startIndex[gg] + cc - 1;
                        XVX(pp,qq) += X(rr1,pp) * V1(rr1,cc) * X(cc1,qq);
                        if (pp==qq){
                            XVY(pp,0) += X(rr1,pp) * V1(rr1,cc) * y[cc1];
                        }
                    }  // end cc
                }   // end rr
                if (pp < qq){
                    XVX(qq,pp) = XVX(pp,qq);
                }  // end if ( pp < qq )
            }  // end gg
        }   // end qq
    }   // end pp

    //***** OUTPUT
    return Rcpp::List::create(
            Rcpp::Named("XVX") = XVX,
            Rcpp::Named("XVY") = XVY
        );
}
///********************************************************************
