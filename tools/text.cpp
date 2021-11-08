#include <Rcpp.h>
#include <vector> 
#include <string>
#include <algorithm>
#include <numeric>
using namespace Rcpp;

// [[Rcpp::export]]
// https://tech.retrieva.jp/entry/2019/06/17/101030
std::vector<int> EditDistance(const std::string& query, const std::string& text, int k) {
  size_t m = query.size(), n = text.size();
  std::vector<int> result;
  std::vector<int> prev(m + 1, 0);
  for (size_t i = 0; i < m; i++) {
    prev[i+1] = prev[i] + 1;
  }
  std::vector<int> next(m + 1, 0);
  for (size_t j = 0; j < n; j++) {
    for (size_t i = 0; i < m; i++) {
      if (query[i] == text[j]) {
        next[i+1] = prev[i];
      } else {
        next[i+1] = std::min({prev[i] + 1, prev[i+1] + 1, next[i] + 1});
      }
    }
    if (next[m] <= k) {
      result.push_back(j);
    }
    prev.swap(next);
  }
  return result;
}

// [[Rcpp::export]]
// https://tech.retrieva.jp/entry/2019/06/17/101030
std::vector<int> EditDistanceBitVector(const std::string& query, const std::string& text, int k) {
  size_t m = query.size(), n = text.size();
  std::vector<int> result;
  std::unordered_map<char, uint64_t> peq;
  for (size_t i = 0; i < m; i++) {
    peq[query[i]] |= uint64_t(1) << i;
  }
  uint64_t pv = ~ uint64_t(0);
  uint64_t mv = 0;
  int score = m;
  for (size_t j = 0; j < n; j++) {
    uint64_t eq = peq.find(text[j]) != peq.end() ? peq[text[j]] : 0;
    uint64_t xv = eq | mv;
    uint64_t xh = (((eq & pv) + pv) ^ pv) | eq;
    uint64_t ph = mv | ~ (xh | pv);
    uint64_t mh = pv & xh;
    if ((ph & (1 << (m - 1))) != 0) {
      score++;
    } else if ((mh & (1 << (m - 1))) != 0) {
      score--;
    }
    ph <<= 1;
    mh <<= 1;
    pv = mh | ~ (xv | ph);
    mv = ph & xv;
    if (score <= k) {
      result.push_back(j);
    }
  }
  return result;
}

// [[Rcpp::export]]
// https://rosettacode.org/wiki/Levenshtein_distance#C.2B.2B
// Compute Levenshtein Distance
// Martin Ettl, 2012-10-05
size_t uiLevenshteinDistance(const std::string &s1, const std::string &s2)
{
  const size_t
    m(s1.size()),
    n(s2.size());
  if( m==0 ) return n;
  if( n==0 ) return m;
  // allocation below is not ISO-compliant,
  // it won't work with -pedantic-errors.
  size_t costs[n + 1];
  for( size_t k=0; k<=n; k++ ) costs[k] = k;
  size_t i { 0 };
  for (char const &c1 : s1) 
  {
    costs[0] = i+1;
    size_t corner { i },
           j      { 0 };
    for (char const &c2 : s2)
    {
      size_t upper { costs[j+1] };
      if( c1 == c2 ) costs[j+1] = corner;
      else {
        size_t t(upper<corner? upper: corner);
        costs[j+1] = (costs[j]<t?costs[j]:t)+1;
      }
      corner = upper;
      j++;
    }
    i++;
  }
  return costs[n];
}
