#ifndef WAMEICHECKR_EDITDIST_H
#define WAMEICHECKR_EDITDIST_H

#include <string>
#include <vector>

// Defined in editdist.cpp
std::vector<std::string> str2strvec(std::string str, int len);
int editdist_dp(const std::vector<std::string> &a, const std::vector<std::string> &b);

#endif
