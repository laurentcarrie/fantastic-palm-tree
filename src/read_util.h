#ifndef read_util_h_
#define read_util_h_ 1

#include <fstream>
#include <string>
#include <vector>

#include "read_util.hc"

std::tuple<std::string,std::string> my_split(const std::string&) ;
std::string read_string_until_empty_line(std::ifstream& in_channel) ;
std::vector<std::string> read_array_until_empty_line(std::ifstream& in_channel) ;


std::string strip_string(const std::string&) ;
std::string extension(const std::string&) ;
std::string replace_extension(const std::string&,const char*) ;
std::string replace_path(const std::string&,const char*,const char*) ;
std::string basename(const std::string&) ;
std::string dirname(const std::string&) ;
bool path_is_absolute(const std::string&) ;
std::string normalize_path(const std::string& path) ;

std::string tex_of_string(const std::string&) ;

std::vector<std::string> stringvector_of_string(const std::string&s,const std::string&sep) ;

template<class T> T walk_tree(const std::string& root_dir,T& acc,std::function<void (T&,const std::string&,bool)>& accumulator) ;
std::vector<std::string> sub_tree(const std::string&) ;

#endif
