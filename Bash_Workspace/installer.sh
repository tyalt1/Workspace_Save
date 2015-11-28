#!/bin/bash

#Resolve package manager
if [ -n `command -v apt-get` ]; then
	pack_man='sudo apt-get'
	install="$pack_man install --yes --quiet"
else
	echo 'Error: Could not find apt-get.' >& 2 # Print error message into stderr
	exit 1
fi

#Add repositories
sudo add-apt-repository --yes ppa:webupd8team/java
sudo add-apt-repository --yes ppa:webupd8team/atom
#Spotify repository
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys D2C19886
echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list

#Update and Upgrade
$pack_man update --yes && $pack_man upgrade --yes

#Media
$install vlc
$install spotify-client

#Languages
$install gcc g++ #C and C++
$install oracle-java8-installer #Java
$install python idle python3 idle3 #Python and Python IDLE
$install perl
$install leiningen #Leiningen build of Clojure

#Bonus Languages
$install ruby
$install gprolog #GNU Prolog
$install scala
$install erlang
$install haskell-platform #Haskell
$install r-base #R statistical language

#Development
$install vim
$install filezilla
$install texlive #LaTeX
$install doxygen doxygen-doc doxygen-gui #Doxygen, Docs, and Doxywizard

#Atom
$install atom
apm install minimap
apm install merge-conflicts
apm install atom-paredit
apm install language-python
apm install autocomplete-python
apm install language-clojure
apm install linter-clojure
apm install language-latex
apm install language-llvm
apm install language-doxygen

#Git
$install git
git config --global user.name 'Tyler Alterio'
git config --global user.email 'tyalt1@gmail.com'
git config --global core.editor vim
git config --global push.default current
git config --global alias.ls 'log --decorate --oneline --graph --all' #Alternative to git log
git config --global alias.s 'status --short' #Alternative to git status
git config --global alias.edit 'config --global --edit' #Edit config page in default editor
git config --global alias.restart 'reset --hard' #Resets to last commit
git config --global alias.rewind 'reset HEAD~' #Undoes last commit
git config --global alias.root 'rev-parse --show-toplevel' #Path to top of git repo

#IDEs
$install arduino
$install qtcreator
$install codeblocks
