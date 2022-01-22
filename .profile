# CURRENT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# if not interactive, do nothing
[[ $- != *i* ]] && return

# aliases
alias ls='ls -G'
alias ll='ls -lhtr'
alias edit_xmonad='vim ~/.xmonad/xmonad.hs'
alias edit_xmobar='vim ~/.xmobarrc'
alias edit_profile='vim ~/.profile'
alias gpgenc='gpg -c --cipher-algo AES256'

# locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8


# ps1
export PS1_='[\u@\h \W]\$ '
export PS1='$ '

# gpg
export GPG_TTY=$(tty)

# java
#export JAVA_HOME=$(dirname $(readlink -f $(which java)))/../

# python
export PYENV_ROOT=$HOME/.pyenv
export PATH=$PYENV_ROOT/bin:$PATH
eval "$(pyenv init --path)"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# google cloud

_google_cloud_sdk="$HOME/google-cloud-sdk"
_google_cloud_path="$_google_cloud_sdk/path.bash.inc"
[[ -f $_google_cloud_path ]] && . $_google_cloud_path
_google_cloud_completion="$_google_cloud_sdk/completion.bash.inc"
[[ -f $_google_cloud_completion ]] && . $_google_cloud_completion

# rubygems
export GEM_HOME=$HOME/gems
export PATH=$HOME/gems/bin:$PATH
export PATH=$HOME/.local/share/gem/ruby/3.0.0/bin:$PATH

# rust / cargo
export PATH=$HOME/.cargo/bin:$PATH

# path stuff
export PATH=$HOME/.local/bin:$PATH

# macos stuff
if [[ $(uname) == "Darwin" ]]; then
    export BASH_SILENCE_DEPRECATION_WARNING=1
    export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_221.jdk/Contents/Home
    alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
fi;
