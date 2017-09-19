#ifndef read_util_hc_defined__
#define read_util_hc_defined__ 1
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

template<class T> T walk_tree(const std::string& root_dir,
			      T& acc,
			      std::function<void (T&,const std::string&,bool is_dir)>& accumulator) {

  std::function<void (T& acc,const std::string& root_dir)> r =
    [&r,&accumulator](T& acc,const std::string& root_dir) {
    
    DIR* dir = opendir(root_dir.c_str()) ;
    struct dirent* dp ;
    while ( (dp=readdir(dir)) != NULL ) {
      std::string s(dp->d_name) ;
      if ( (s!="." && s!="..") ) {
	if (dp->d_type == DT_REG) {
	  accumulator(acc,root_dir + "/" + std::string(dp->d_name),false) ;
	} else if (dp->d_type == DT_DIR) {
	  accumulator(acc,root_dir + "/" + std::string(dp->d_name),true) ;
	  r(acc,root_dir + "/" + std::string(dp->d_name)) ;
	}
	else {
	  std::cout << "unknown dir entry : " << dp->d_name ;
	}
      }
    }
  } ;
  r(acc,root_dir) ;
  return acc ;
}


#endif

