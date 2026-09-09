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
#include "editdist.h"
using namespace Rcpp;

// 切り替えの閾値(トークン数)は，下の 2 つの関数の bp_min の既定値 18．
// これ以上の長さなら bit-parallel を使う．短い文字列では，ビットマスクを
// 作る手間の方が DP 全体より重いため．
// ref_sc(学名 76,379 件)に対する実測では 8 から 20 がほぼ横ばいで，
// 18 から 20 が最速(常に DP の 0.67 倍)．24 以上は DP に落ちる分だけ遅くなる．
// ref_jp(和名 51,809 件)は 99.9 % が 16 文字未満なので，どの値でも変わらない．
// Rcpp の属性は定数を既定値に書けないので，直接 18 と書いている．

// bit-parallel が扱えるトークン数の上限(1 ブロック分)．
static const std::size_t BP_MAX = 64;

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
                             int bp_min = 18){
  std::vector< std::vector<std::string> > ti(input.size()), tr(reference.size());
  for(std::size_t i = 0; i < input.size(); i++)     ti[i] = str2strvec(input[i], len);
  for(std::size_t j = 0; j < reference.size(); j++) tr[j] = str2strvec(reference[j], len);

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
                int bp_min = 18){
  return editdist_tokens(str2strvec(s1, len), str2strvec(s2, len), bp_min);
}

// str_length() と同じ数え方(UTF-8 の文字数)．
// editdist_norm() を R と同じ式で計算するために要る．
static std::size_t utf8_len(const std::string &s){
  std::size_t n = 0;
  for(std::size_t i = 0; i < s.size(); i++){
    if((static_cast<unsigned char>(s[i]) & 0xC0) != 0x80) n++;
  }
  return n;
}

//' Editing distance of close pairs only
//'
//' Returns only the pairs that satisfy
//' `editdist < min_dist | editdist_norm < min_dist_norm`, so that R never
//' builds the full `length(input) * length(reference)` table.
//' `maybe()` and `mosiya()` use this; `editdist_multi()` keeps returning
//' every combination, because that is its published behaviour.
//'
//' The normalised distance is the same as `editdist_norm()`:
//' `editdist / max(nchar(s1), nchar(s2)) * len`.
//'
//' @param input Vector of string to be compared.
//' @param reference Vector of string to be compared.
//' @param len Dividing length of string.
//' @param min_dist Minimum editing distance.
//' @param min_dist_norm Minimum normalised editing distance.
//' @param bp_min Minimum length (in tokens) to use the bit-parallel
//'   algorithm.  0 means never.
//'
//' @return Data frame of 4 columns: `input_id` and `reference_id` are
//'   1-based indices into `input` and `reference`.
//'
//' @noRd
// [[Rcpp::export]]
DataFrame editdist_close_pairs(std::vector<std::string> input,
                               std::vector<std::string> reference,
                               int len = 1,
                               double min_dist = 4,
                               double min_dist_norm = 0.2,
                               int bp_min = 18){
  const std::size_t ni = input.size(), nr = reference.size();
  std::vector< std::vector<std::string> > ti(ni), tr(nr);
  std::vector<std::size_t> li(ni), lr(nr);
  for(std::size_t i = 0; i < ni; i++){
    ti[i] = str2strvec(input[i], len);
    li[i] = utf8_len(input[i]);
  }
  for(std::size_t j = 0; j < nr; j++){
    tr[j] = str2strvec(reference[j], len);
    lr[j] = utf8_len(reference[j]);
  }

  std::vector<int> out_i, out_j, out_d;
  std::vector<double> out_n;
  for(std::size_t i = 0; i < ni; i++){
    for(std::size_t j = 0; j < nr; j++){
      // 正規化した距離が min_dist_norm 未満になる編集距離の上限．
      // これと min_dist の大きい方を超える距離は，どちらの条件も満たさない．
      const double max_len = (double)(li[i] > lr[j] ? li[i] : lr[j]);
      const double lim_norm = min_dist_norm * max_len / (double)len;
      const double lim = (min_dist > lim_norm) ? min_dist : lim_norm;
      // トークン数の差は編集距離の下限なので，これで足切りできる
      const std::size_t a = ti[i].size(), b = tr[j].size();
      const double lower = (double)(a > b ? a - b : b - a);
      if(lower >= lim) continue;
      const int d = editdist_tokens(ti[i], tr[j], bp_min);
      const double norm = (max_len > 0) ? ((double)d / max_len * (double)len)
                                        : R_NaN;
      if(! ((double)d < min_dist || norm < min_dist_norm)) continue;
      out_i.push_back((int)i + 1);
      out_j.push_back((int)j + 1);
      out_d.push_back(d);
      out_n.push_back(norm);
    }
  }
  return DataFrame::create(
    _["input_id"]      = out_i,
    _["reference_id"]  = out_j,
    _["editdist"]      = out_d,
    _["editdist_norm"] = out_n);
}
