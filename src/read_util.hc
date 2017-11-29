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

#ifndef WIN32
#include <dirent.h>
#else
#include <windows.h>
#include <tchar.h> 
#include <stdio.h>
#include <strsafe.h>
#pragma comment(lib, "User32.lib")
#endif

#include "read_util.h"

#ifndef WIN32
template<class T> T walk_tree(const std::string& root_dir,
			      T& acc,
			      std::function<void (T&,const std::string&,bool is_dir)>& accumulator) {

  std::function<void (T& acc,const std::string& root_dir)> r =
    [&r,&accumulator](T& acc,const std::string& root_dir) {
    
	std::cout << "open dir ''" << root_dir << "''" << std::endl ;
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
#else
// version windows
#define ValidDir(data) strcmp(data.cFileName,".")&&strcmp(data.cFileName,"..")

template<class T> T walk_tree(const std::string& root_dir,
			      T& acc,
			      std::function<void (T&,const std::string&,bool is_dir)>& accumulator) {
  std::function<void (T& acc,const std::string& root_dir)> r =
    [&r,&accumulator](T& acc,const std::string& root_dir) {
    
   WIN32_FIND_DATA ffd;
   //int filesize;
   //char szDir[MAX_PATH];
   //size_t length_of_arg;
   HANDLE hFind = INVALID_HANDLE_VALUE;
   DWORD dwError=0;
      
	std::cout << __FILE__ << ":" << __LINE__ << std::endl ;
	std::cout << "root dir '" << root_dir << "'" << std::endl ;

	std::string s = root_dir + "\\*" ;
    hFind = FindFirstFile(s.c_str(), &ffd);
	if (hFind == INVALID_HANDLE_VALUE ) {
		throw std::runtime_error("bad file " + root_dir ) ;
	}

	do {
		std::cout << "found '" << ffd.cFileName << "'" << std::endl ;
		if (!strcmp(ffd.cFileName,".")) {
			std::cout << "skip" << ffd.cFileName << std::endl ;
		}
		else if (!strcmp(ffd.cFileName,"..")) {
			std::cout << "skip" << ffd.cFileName << std::endl ;
		}
		else if (ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)   {
				std::cout << "------- is a directory " << std::endl ;   
			    accumulator(acc,root_dir + "/" + std::string(ffd.cFileName),true) ;
			    r(acc,root_dir + "/" + std::string(ffd.cFileName)) ;
		}
		else {
			std::cout << "------- is a file " << std::endl ;   
			accumulator(acc,root_dir + "/" + std::string(ffd.cFileName),false) ;
		}
	} while (FindNextFile(hFind, &ffd) != 0);
  } ;
  r(acc,root_dir) ;
  return acc ;
}

#endif

#endif

