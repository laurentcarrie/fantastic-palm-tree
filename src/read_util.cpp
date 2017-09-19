#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <vector>
#include <numeric>
#include <functional>
#include <algorithm>
#include <string.h>
#include <dirent.h>

#include "read_util.h"

void check_note(char a) {
    switch (a) {
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
        return ;
    default :
    {
        std::ostringstream oss ;
        oss << "not a note : " << a << std::endl ;
        throw std::runtime_error(oss.str()) ;
    }
    }
}


std::tuple<std::string,std::string> my_split(const std::string& s) {
    auto pos = s.find(' ') ;
    if (pos==std::string::npos) {
      // std::cout << "my_split " << s << " ; no s2" << std::endl; 
        return std::make_tuple(s,std::string("")) ;
    } else {
        std::string s1(s.substr(0,pos)) ;
        std::string s2(s.substr(pos+1,s.size()-pos-1)) ;
	// std::cout << "my_split " << s << " -> " << s1 << " ; " << s2 << std::endl; 
        return std::make_tuple(s1,s2) ;
    }
}


void strip_string(std::string& s) {
    if (s=="") return ;
    if (s[0]==' ') {
        s.erase(s.begin()) ;
        strip_string(s) ;
    }
    return ;
}

std::vector<std::string> read_array_until_empty_line(std::ifstream& fin) {
  std::function<
  void (std::ifstream& fin,std::vector<std::string>& acc)
    > r =
    [&r](std::ifstream&fin,std::vector<std::string>& acc) {
    if (fin.eof() || fin.bad() || fin.fail()) { 
      // std::reverse(acc.begin(),acc.end()) ;
      return ;
    }
    char line[1001] ;
    fin.getline(line,1000) ;
    std::string l(line) ;
    strip_string(l) ;
    if ( l == "" ) {
      // std::reverse(acc.begin(),acc.end()) ;
      return ;
    }
    // std::cout << __FILE__ << ":" << __LINE__ << " -> '" << l << "'" << std::endl ;
    acc.push_back(l) ;
    r(fin,acc) ;
  } ;

  std::vector<std::string> acc ;
  r(fin,acc) ;
  return acc ;
}




std::string read_string_until_empty_line(std::ifstream& fin) {
    std::vector<std::string> a = read_array_until_empty_line(fin) ;
    std::string ret = std::accumulate(a.begin(),a.end(),std::string(""),[](std::string acc,std::string inc) {
	if (acc=="") { return inc ; }
	else {
	  return acc + "\n" + inc ;
	}
      }) ;
    return ret ;
}


std::string extension(const std::string& filename) {
  auto pos = filename.rfind('.') ;
  if (pos==std::string::npos) {
    return "" ;
  } else {
    std::string ret(filename.substr(pos+1,filename.length()-pos-1)) ;
    return ret ;
  }
}

std::string replace_extension(const std::string& filename,const char* ext) {
  auto pos = filename.rfind('.') ;
  if (pos==std::string::npos) {
    return filename + std::string(ext) ;
  } else {
    std::string s1(filename.substr(0,pos)) ;
    std::string ret = s1 + std::string(ext) ;
    return ret ;
  }
}
std::string replace_path(const std::string& filename,const char* path1,const char* path2) {
  auto pos = filename.find(path1) ;
  if (pos!=0) {
    std::ostringstream oss ;
    oss << "Cannot find '" << path1 << "', for replacement in '" << filename << "'" << std::endl ;
    throw std::runtime_error(oss.str()) ;
  }
  
  std::string ret(path2) ;
  ret += filename.substr(strlen(path1),filename.size()-strlen(path1)) ;

  return ret ;
}

std::string basename(const std::string& filename) {
  auto pos = filename.rfind('/') ;
  if (pos==std::string::npos) {
    return filename ;
  } else {
    std::string s1(filename.substr(pos+1,filename.size()-pos-1)) ;
    return s1 ;
  }
}

std::string dirname(const std::string& filename) {
  auto pos = filename.rfind('/') ;
  if (pos==std::string::npos) {
    return filename ;
  } else {
    std::string s1(0,pos) ;
    return s1 ;
  }
}

void mkdir_p(const std::string& dirname) {
}

bool path_is_absolute(const std::string& path) {
  auto pos = path.find('/') ;
  if (pos==std::string::npos) {
    return false ;
  }
  if (pos==0) {
    return true ;
  }
  return false ;
}


std::string tex_of_string (const std::string& s) {
  return s ;
}

std::vector<std::string> stringvector_of_string(const std::string&s, const std::string& sep) {
  std::function< void (const std::string& s,std::vector<std::string>& acc) > r = 
    [&sep,&r](const std::string& s,std::vector<std::string>& acc) {
    if ( s == "" ) return ;
    auto pos = s.find(sep) ;
    if (pos==std::string::npos) {
      acc.push_back(s) ;
      return ;
    } else {
      std::string s1(s.substr(0,pos)) ;
      std::string s2(s.substr(pos+2-sep.size(),s.size()-pos-2+sep.size())) ;
      if (s1 != "") { acc.push_back(s1) ; }
      if (! (s1.size() < s.size())) { throw std::runtime_error("algorithm stringvector_of_string") ; }
      r(s2,acc) ;
    }
    return ;
  };

  std::vector<std::string> acc ;
  r(s,acc) ;

  return acc ;
}


std::vector<std::string> sub_tree(const std::string& root_dir) {

  typedef std::vector<std::string> T ;
  T ret ;
  std::function<void (T&acc,const std::string& root_dir,bool)> r = 
    [](T& acc,const std::string& filename,bool is_dir) {
    if (! is_dir) { acc.push_back(filename) ; }
  } ;
  walk_tree(root_dir,ret,r) ;
  return ret ;
}




		     
