#ifndef read_util_h_
#define read_util_h_ 1

#include <fstream>
#include <string>
#include <vector>

std::tuple<std::string,std::string> my_split(const std::string&) ;
std::string read_string_until_empty_line(std::ifstream& in_channel) ;
std::vector<std::string> read_array_until_empty_line(std::ifstream& in_channel) ;

std::string replace_extension(const std::string&,const char*) ;
std::string replace_path(const std::string&,const char*,const char*) ;
std::string basename(const std::string&) ;
bool path_is_absolute(const std::string&) ;

std::string tex_of_string(const std::string&) ;

std::vector<std::string> stringvector_of_string(const std::string&s,const std::string&sep) ;

#endif
