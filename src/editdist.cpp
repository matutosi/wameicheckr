#include <Rcpp.h>
#include <vector> 
#include <string>
#include <algorithm>
#include <numeric>
using namespace Rcpp;

//' Convert string into string vector
//' 
//' @param str A string to be converted. 
//' @param len Dividing length of string. 
//' @export
// [[Rcpp::export]]
std::vector<std::string> str2strvec(std::string str, int len=1){
  std::vector<std::string> s;
  int n = str.size();
  for(int i=0; i < n; i = i + len) s.push_back(str.substr(i, len));
  return s;
}

//' Editing distance (Levenshtein distance) of two string vectors
//' 
//' @param str1 A string to be compared. 
//' @param str2 A string to be compared. 
//' @param len Dividing length of string. 
//' @export
// [[Rcpp::export]]
int editdist(std::string s1, std::string s2, int len=1){
  std::vector<std::string> str1 = str2strvec(s1, len);
  std::vector<std::string> str2 = str2strvec(s2, len);
  int m = str1.size();
  int n = str2.size();
// // for debug
Rcout << "m, n: " << m << ", " << n << "\n";
  int d[m+1][n+1];
  d[0][0] = 0;
  for(int i=0; i < m+1; i++) d[i][0] = i;
  for(int j=0; j < n+1; j++) d[0][j] = j;
  int cost;
  for(int i=1; i<m+1; i++){
    for(int j=1; j<n+1; j++){
      if(str1[i-1] == str2[j-1]){ cost = 0; } else {cost = 1;}
      d[i][j] = std::min({d[i-1][j]+1, d[i][j-1]+1, d[i-1][j-1] + cost});
    }
  }
//  // for debug
Rcout << "d\n";
for(int i=0; i<m+1; i++){
  for(int j=0; j<n+1; j++){
    Rcout << d[i][j] << " ";
  }
  Rcout << "\n";
}
  return d[m][n];
}
