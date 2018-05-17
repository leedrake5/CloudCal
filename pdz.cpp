//
//  pdz.cpp
//  
//
//  Created by Lee Drake on 5/16/18.
//

//
#include <Rcpp.h>
#include <fstream>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector readPDZ24(std::string fileName, int start, int size) {
    uint32_t a[size];
    std::ifstream file (fileName, std::ios::in | std::ios::binary);
    if (file.is_open()) {
        file.seekg(start);
        file.read ((char*)&a, sizeof(a));
        file.close();
    }
    NumericVector res(size);
    for (unsigned long long int i = 0; i < size; ++i)
    res(i) = (a[i]) ;
    return res;
}

// [[Rcpp::export]]
NumericVector readPDZ25(std::string fileName, int start, int size) {
    uint32_t a[size];
    std::ifstream file (fileName, std::ios::in | std::ios::binary);
    if (file.is_open()) {
        file.seekg(start);
        file.read ((char*)&a, sizeof(a));
        file.close();
    }
    NumericVector res(size);
    for (unsigned long long int i = 0; i < size; ++i)
    res(i) = (a[i]) ;
    return res;
}

// [[Rcpp::export]]
NumericVector getevch24(std::string fileName, int start, int size) {
    uint32_t a[size];
    std::ifstream file (fileName, std::ios::in | std::ios::binary);
    if (file.is_open()) {
        file.seekg(start);
        file.read ((char*)&a, sizeof(a));
        file.close();
    }
    NumericVector res(size);
    for (unsigned long long int i = 0; i < size; ++i)
    res(i) = (a[i]) ;
    return res;
}



