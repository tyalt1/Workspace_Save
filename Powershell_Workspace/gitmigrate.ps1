Param( [Parameter(Mandatory)][string]$svnurl, [Parameter(Mandatory)][string]$gitrepo )
<#!
	\breif Powershell script for porting SVN repos into Philips Color Kinetics Github.
	\param[in] $svnurl URL of svn repo. Required.
	\param[in] $gitrepo Name of new git repo. Required.
	\note Install/Configure posh-git with powershell to work best. Add .../Git/cmd/ to path to work.
	\note Have your authors file named 'authors.txt' and be in the same path as this script.
#>

#Clone svn repo as a git repo locally
git svn clone --authors-file=authors.txt $svnurl $gitrepo

#Go into git repo
#'cd' is alias of 'Set-Location'
Set-Location .\$gitrepo

#Push git repo to github
git remote add origin https://github.com/colorkinetics/$gitrepo.git
git pull origin master #If there is anything already in the repo.
git push --set-upstream origin master #'-u' is same as '--set-upstream'

#Leave git repo
Set-Location ..