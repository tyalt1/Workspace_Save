#!/bin/bash

#This is a pre-commit git hook that runs astlye.
#Assumes astyle is installed and in PATH
#Assumes astyle options are in .astylerc in root of repo

#Matches files that were changed and staged, but not deleted.
#Matches files with extensions:
# *.c/*.h
# *.cpp/*.hpp
# *.cc/*.hh
# *.cxx/*.hxx
# *.c++/*.h++
changed_files=$(git diff --staged --name-status | awk '$1!="D" && $2~/\.(c|h)(?:p{2}|\1|x{2}|\+{2})?$/ {print $2}')
astyle --options=.astylerc --lineend=linux --suffix=none ${changed_files[@]}
git add ${changed_files[@]}
