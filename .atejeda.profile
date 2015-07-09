# atejeda
# [[ -f $HOME/.atejeda_profile ]] && . $HOME/.atejeda_profile

# locate configuration
#export LC_ALL=en_US.UTF-8
#export LANG=en_US.UTF-8
#export LANGUAGE=en_US.UTF-8
#export LC_CTYPE=en_US.UTF-8

# user defined
# export PS1="\[\e[00;37m\]\T \u@\H \W >\[\e[0m\] "

# aliases
alias ll='ls -l'
alias ssh-aoc='ssh ssh.aoc.nrao.edu -l $USER $*'
alias ssh-cv='ssh ssh.cv.nrao.edu -l $USER $*'
alias acs-env='. /alma/ACS-current/ACSSW/config/.acs/.bash_profile.acs'
alias ssh-dell='ssh 146.88.7.84 $*'
alias gpg-file='gpg -c --cipher-algo AES256'
alias reload-profile='. ~/.bashrc'

# functions
function svn-remove {
    for f in `svn status | grep ! | cut -d " " -f8`; do echo svn remove $f; done;
}

# exports
ECLIPSE=/export/home/hangar18/atejeda/Desktop/eclipse-luna-4.4.2
export PATH=$ECLIPSE:$PATH
export PATH=$HOME/bin:$PATH
export EDITOR=vim

