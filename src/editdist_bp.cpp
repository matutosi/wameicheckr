// Faster edit distance for editdist_multi().
//
// Two changes over editdist() in editdist.cpp:
//   1. editdist_pairs() computes every pair in C++, so that editdist_multi()
//      does not have to call into C++ once per pair from R.  This is where
//      most of the speed-up comes from.
//   2. Long strings use Myers' bit-parallel algorithm instead of dynamic
//      programming.  Short strings stay on dynamic programming, because
//      building the bit mask table costs more than the whole DP table when
//      the string is short.
//
// Bit-parallel reference implementation (char based):
//   https://qiita.com/aflc/items/f4299700471cc11f1d1c
// Here the strings are split into tokens by len, so that escaped unicode
// (len = 6) is compared character by character, as editdist() does.

#include <Rcpp.h>
#include <algorithm>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>
using namespace Rcpp;

// 切り替えの閾値(トークン数)は，下の 2 つの関数の bp_min の既定値 16．
// これ以上の長さなら bit-parallel を使う．短い文字列では，ビットマスクを
// 作る手間の方が DP 全体より重いため．暫定値なので要再検討．
// Rcpp の属性は定数を既定値に書けないので，直接 16 と書いている．

// bit-parallel が扱えるトークン数の上限(1 ブロック分)．
static const std::size_t BP_MAX = 64;

static std::vector<std::string> tokenize(const std::string &str, int len){
  std::vector<std::string> tokens;
  tokens.reserve(str.size() / len + 1);
  for(std::size_t i = 0; i < str.size(); i += len) tokens.push_back(str.substr(i, len));
  return tokens;
}

// 動的計画法．2 行だけ持ち回るので，メモリは O(n)．
static int editdist_dp(const std::vector<std::string> &a,
                       const std::vector<std::string> &b){
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

// Myers の bit-parallel 法(1 ブロック)．pat は 1 から BP_MAX トークン．
static int editdist_bp64(const std::vector<std::string> &pat,
                         const std::vector<std::string> &txt){
  const std::size_t m = pat.size();
  const std::uint64_t top = std::uint64_t(1) << (m - 1);
  std::unordered_map<std::string, std::uint64_t> peq;
  for(std::size_t i = 0; i < m; i++) peq[pat[i]] |= (std::uint64_t(1) << i);

  std::uint64_t pv = ~std::uint64_t(0);
  std::uint64_t mv = 0;
  int score = m;
  for(std::size_t j = 0; j < txt.size(); j++){
    std::unordered_map<std::string, std::uint64_t>::const_iterator it = peq.find(txt[j]);
    const std::uint64_t eq = (it == peq.end()) ? 0 : it->second;
    const std::uint64_t xv = eq | mv;
    const std::uint64_t xh = (((eq & pv) + pv) ^ pv) | eq;
    std::uint64_t ph = mv | ~(xh | pv);
    std::uint64_t mh = pv & xh;
    if(ph & top){ score++; } else if(mh & top){ score--; }
    ph = (ph << 1) | 1;
    mh = mh << 1;
    pv = mh | ~(xv | ph);
    mv = ph & xv;
  }
  return score;
}

// トークン列 2 つの編集距離．長さを見て DP と bit-parallel を選ぶ．
static int editdist_tokens(const std::vector<std::string> &t1,
                           const std::vector<std::string> &t2,
                           int bp_min){
  if(t1 == t2) return 0;
  if(t1.empty()) return t2.size();
  if(t2.empty()) return t1.size();
  // 短い方をパターンにすると，必要なビット数が最小になる
  const std::vector<std::string> *pat = &t1, *txt = &t2;
  if(pat->size() > txt->size()) std::swap(pat, txt);
  if(bp_min > 0 && pat->size() >= (std::size_t)bp_min && pat->size() <= BP_MAX){
    return editdist_bp64(*pat, *txt);
  }
  return editdist_dp(*pat, *txt);
}

//' Editing distance of all combinations of two string vectors
//' 
//' Computes every pair in C++, so that R does not have to loop over pairs.
//' The result is ordered as `tidyr::expand_grid(s1 = input, s2 = reference)`.
//' 
//' @param input Vector of string to be compared. 
//' @param reference Vector of string to be compared. 
//' @param len Dividing length of string. 
//' @param bp_min Minimum length (in tokens) to use the bit-parallel
//'   algorithm.  0 means never.
//' 
//' @return Integer vector of `length(input) * length(reference)`. 
//' 
//' @noRd
// [[Rcpp::export]]
IntegerVector editdist_pairs(std::vector<std::string> input,
                             std::vector<std::string> reference,
                             int len = 1,
                             int bp_min = 16){
  std::vector< std::vector<std::string> > ti(input.size()), tr(reference.size());
  for(std::size_t i = 0; i < input.size(); i++)     ti[i] = tokenize(input[i], len);
  for(std::size_t j = 0; j < reference.size(); j++) tr[j] = tokenize(reference[j], len);

  IntegerVector out(input.size() * reference.size());
  std::size_t k = 0;
  for(std::size_t i = 0; i < ti.size(); i++){
    for(std::size_t j = 0; j < tr.size(); j++){
      out[k++] = editdist_tokens(ti[i], tr[j], bp_min);
    }
  }
  return out;
}

//' Editing distance with bit-parallel algorithm
//' 
//' Returns the same value as `editdist()`.  Kept for testing and for
//' tuning `bp_min`; `editdist_pairs()` is what the package uses.
//' 
//' @param s1 A string to be compared. 
//' @param s2 A string to be compared. 
//' @param len Dividing length of string. 
//' @param bp_min Minimum length (in tokens) to use the bit-parallel
//'   algorithm.  0 means never.
//' 
//' @return Integer. 
//' 
//' @noRd
// [[Rcpp::export]]
int editdist_bp(std::string s1, std::string s2, int len = 1,
                int bp_min = 16){
  return editdist_tokens(tokenize(s1, len), tokenize(s2, len), bp_min);
}
