#ifndef UTILS_H
#define UTILS_H

#include <string>

namespace utils {
void usage();
void typecheckingError(const std::string& msg, int lineNumber, int column);
}  // namespace utils
#endif