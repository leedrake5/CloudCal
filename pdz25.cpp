//
//  pdz.cpp
//
//
//  Created by Lee Drake on 3/11/19
//

//
#include <Rcpp.h>
#include <fstream>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector readPDZ25(std::string fileName) {
    std::ifstream file (fileName, std::ios::in | std::ios::binary);
    
    int main()
    {
        bool eof = false;
        int offset = 0;
        short  record;
        int  recordSize;
        
        std::ifstream file(fileName, std::ios::in | std::ios::binary);
        
        while (!file.eof) {
            file.seekg(offset);            // to start of next record
            file.read((char *)record, sizeof(short));
            file.read((char *)recordSize, sizeof(int);
                      switch (record) {
                          case 0: readRecord0(recordSize);  break;
                          case 1: readRecord1(recordSize);  break;
                          case 2: readRecord2(recordSize);  break;
                          case 3: readRecord3(recordSize);  break;
                              // more if needed
                          default: cout << "wrong record number";
                      }
                      offset += recordSize + 6;    // the record number and size are included separately
            }
    }
  
}
