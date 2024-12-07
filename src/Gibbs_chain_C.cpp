#include <Rcpp.h>
using namespace Rcpp;
//' @name Gibbs_chain_C
//' @title A Gibbs sampler function using Rcpp
//' @description Samples are drawn according to the idea of Gibbs sampler using Rcpp
//' @useDynLib SA24168406
//' @param a one of the parameters of binary density
//' @param b one of the parameters of binary density
//' @param n one of the parameters of binary density
//' @param N the number of samples generated
//' @param burn the number of samples discarded
//' @param x0 the initial x-value
//' @param y0 the initial y-value
//' @return A NumericMatrix of the samples \code{n}
//' @examples
//' \dontrun{
//'     set.seed(12345)
//'     N <- 1e4
//'     burn <- 1000
//'     a <- 2
//'     b <- 4
//'     n <- 16
//'     x0 <- sample(0:n,1)
//'     y0 <- runif(1)
//'     xC <- Gibbs_chain_C(a, b, n, N, burn, x0, y0)
//' }
//' @export

// [[Rcpp::export]]
NumericMatrix Gibbs_chain_C(double a, double b, int n, int N, int burn, int x0, double y0) {
  NumericMatrix X(N, 2);
  X(0,0) = x0;
  X(0,1) = y0;
  int x;
  double y;
  for(int i = 1; i < N; i++){
    y = X(i-1,1);
    X(i,0) = rbinom(1,n,y)[0];
    x = X(i,0);
    X(i,1) = rbeta(1,x+a,n-x+b)[0];
  }
  NumericMatrix x1(N-burn, 2);
  for(int i = burn; i < N; i++){
    x1(i - burn,0) = X(i,0);
    x1(i - burn,1) = X(i,1);
  }
  return x1;
}
