#!/bin/bash

#Init variables
lang_pack=0

#Check Flags
# Flags
# -l Turns on the optional language packages.
while getopts ":l" o; do
	case ${o} in
		l) lang_pack=1;;
	esac
done

if [ -n `command -v apt-get` ]; then
	pack_man='sudo apt-get --yes --quiet'
	install="$pack_man install"
else
	echo 'Error: Could not find apt-get.' >& 2 # Print error message into stderr
	exit 1
fi

#Add repositories
sudo add-apt-repository --yes ppa:webupd8team/java
sudo add-apt-repository --yes ppa:webupd8team/sublime-text-2

#Update and Upgrade
$pack_man update && $pack_man upgrade

#Media
$install vlc

#Languages
$install gcc #C
$install g++ #C++
$install oracle-java8-installer #Java
$install python3 && $install idle3 #Python and Python IDLE
$install perl

#Bonus Languages
if [ $lang_pack != 0 ]; then
	$install ruby
	$install gprolog #GNU Prolog
	$install scala
	$install erlang
	$install leiningen #Leiningen build of Clojure
	$install haskell-platform #Haskell
	$install r-base #R statistical language
fi

#Development
$install sublime-text
$install filezilla
$install texlive #LaTeX
$install doxygen
$install doxygen-doc #Doxygen Documentation
$install doxygen-gui #Doxywizard

#Git
$install git
git config --global user.name 'Tyler Alterio'
git config --global user.email 'tyalt1@gmail.com'
git config --global core.editor sublime-text
git config --global push.default current
git config --global alias.ls 'log --decorate --oneline --graph --all' #Alternative to git log
git config --global alias.s 'status --short' #Alternative to git status
git config --global alias.edit 'config --global --edit' #Edit config page in default editor
git config --global alias.restart 'reset --hard' #Resets to last commit
git config --global alias.rewind 'reset HEAD~' #Undoes last commit
git config --global alias.root 'rev-parse --show-toplevel' #Path to top of git repo

#IDEs
$install qtcreator
$install codeblocks
