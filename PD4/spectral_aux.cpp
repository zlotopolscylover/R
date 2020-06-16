#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
IntegerVector order_cpp(NumericVector x){
  NumericVector sorted = clone(x).sort();
  return match(sorted, x);
}
// [[Rcpp::export]]
NumericMatrix Mnn(NumericMatrix X, int M){
  int p = X.nrow();   
  int n = X.ncol();
  if(M > p) stop("M is higher than d! Error.");
  NumericMatrix tmp(p, p);
  NumericMatrix wynik(p, M + 1);
  NumericMatrix wynik1(p, M);
  for(int i = 0; i < p; i++){
    for(int j = 0; j < p; j++){
      double suma = 0.0;
      for(int k = 0; k < n; k++){
        suma = suma + (X(i,k) - X(j,k))*(X(i,k) - X(j,k));
      }
      tmp(i,j) = sqrt(suma);
    }
    NumericVector sorted = tmp(i,_);
    IntegerVector ord(p);            
    ord = order_cpp(sorted);
    for(int l = 0; l < M + 1; l++){
      wynik(i,l) = ord[l];
    }
  }
  
  for(int i = 0; i < p; i++){
    for(int j = 0; j < M; j++){
      wynik1(i,j)=wynik(i,j+1);
    }
  }
  return wynik1;
}
