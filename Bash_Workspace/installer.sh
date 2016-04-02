#!/bin/bash

#Resolve package manager
if [ -z $(which apt-get) ]; then
	echo 'Error: Could not find apt-get.' >& 2 # Print error message into stderr
	exit 1
fi

#Resolve root access
if [ $(id -u) != 0 ]; then
	echo 'Error: Must be run as root.' >& 2
	exit 1
fi

install='apt-get install --yes --quiet'

#Add repositories
add-apt-repository --yes ppa:webupd8team/java
add-apt-repository --yes ppa:webupd8team/atom
add-apt-repository --yes ppa:libretro/stable

#Spotify repository
apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys D2C19886
echo deb http://repository.spotify.com stable non-free | tee /etc/apt/sources.list.d/spotify.list

#Update and Upgrade
apt-get update --yes
apt-get upgrade --yes
apt-get autoremove --yes

#Media/Games
$install vlc
$install spotify-client
$install chromium-browser
$install retroarch retroarch-* libretro-* #Emulation

#Languages
$install build-essential #gcc, g++, make, and some libs
$install oracle-java8-installer #Java
$install python idle python-pip #Python 2
$install python3 idle3 python3-pip #Python 3
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
$install tree #ls alternative
$install terminator #terminal emulator
$install vim
$install filezilla
$install texlive #LaTeX
$install doxygen doxygen-doc doxygen-gui #Doxygen, Docs, and Doxywizard
if [ -z $(which docker) ]; then
	#Install Docker if not already installed.
	#Add user to docker group with `sudo usermod -aG docker <user-here>`
	wget -qO- https://get.docker.com/ | sh
fi

#Atom
$install atom
apm install minimap pdf-view todo
apm install merge-conflicts
apm install atom-paredit language-clojure linter-clojure #Clojure Utility
apm install language-python autocomplete-python #Python Utility
apm install linter-gcc #C/C++ Utility
apm install language-erlang language-elixir #Erlang
apm install language-latex language-llvm language-doxygen \
						language-arduino language-docker

#Git
$install git
git config --global user.name 'Tyler Alterio'
git config --global user.email 'tyalt1@gmail.com'
git config --global core.editor vim
git config --global push.default current
git config --global alias.ls 'log --decorate --oneline --graph --all -15' #Alternative to git log
git config --global alias.s 'status --short' #Alternative to git status
git config --global alias.edit 'config --global --edit' #Edit config page in default editor
git config --global alias.restart 'reset --hard' #Resets to last commit
git config --global alias.rewind 'reset HEAD~' #Undoes last commit
git config --global alias.root 'rev-parse --show-toplevel' #Path to top of git repo

#IDEs
$install arduino
$install qtcreator
$install codeblocks
