#include <Rcpp.h>
#include <vector> 
#include <string>
#include <algorithm>
#include <numeric>
#include "editdist.h"
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

// 動的計画法．表を 2 行だけ持ち回るので，メモリは O(n)．
// 可変長配列だと GCC 拡張になり，長い文字列でスタックを壊す恐れがあるため．
int editdist_dp(const std::vector<std::string> &a, const std::vector<std::string> &b){
  const std::size_t m = a.size(), n = b.size();
  std::vector<int> prev(n + 1), cur(n + 1);
  for(std::size_t j = 0; j <= n; j++) prev[j] = j;
  for(std::size_t i = 1; i <= m; i++){
    cur[0] = i;
    for(std::size_t j = 1; j <= n; j++){
      const int cost = (a[i - 1] == b[j - 1]) ? 0 : 1;
      cur[j] = std::min(std::min(prev[j] + 1, cur[j - 1] + 1), prev[j - 1] + cost);
    }
    prev.swap(cur);
  }
  return prev[n];
}

//' Editing distance (Levenshtein distance) of two string vectors
//' 
//' @param s1 A string to be compared. 
//' @param s2 A string to be compared. 
//' @param len Dividing length of string. 
//' @export
// [[Rcpp::export]]
int editdist(std::string s1, std::string s2, int len=1){
  std::vector<std::string> str1 = str2strvec(s1, len);
  std::vector<std::string> str2 = str2strvec(s2, len);
  if(str1 == str2){
    return 0;
  }
  return editdist_dp(str1, str2);
}
