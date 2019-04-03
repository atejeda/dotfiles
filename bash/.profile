# some more ls aliases
alias ll='ls -lF'
alias la='ls -A'
alias l='ls -CF'
alias ef='gpg -c --cipher-algo AES256'
alias oo='xdg-open'
alias gemacs='emacs -nw -bg color-232'
alias driver='gemacs ~/Desktop/alma/code/development/pipeline-driver/ADAPT/pipeline-driver/'

function load_almasw {
  [[ ! -f ~/.load_alma ]] && return
  source /alma/ACS-current/ACSSW/config/.acs/.bash_profile.acs
}

function show_almasw {
  echo "ACS     = ${ALMASW_RELEASE:-none}"
  echo "INTROOT = ${INTROOT:-none}"
  echo "INTLIST = ${INTLIST:-none}"
}

function load_profile {
  . ~/.bashrc
}

function load_nvm {
  [[ ! -f ~/.load_nvm ]] && return
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
}

function load_conda {
  [[ ! -f ~/.load_conda ]] && return
  export PATH="$HOME/anaconda3/bin:$PATH"
}

function load_intlist {
  [[ ! -f ~/.load_alma ]] && return
  local INTLIST_ROOT=/alma/ACS-current/intlist
  local INTLIST_FILE=${INTLIST_ROOT}/intlist
  unset INTLIST
  local LOCAL_INTLIST=""
  if [[ -f ${INTLIST_FILE} ]]; then
    for _intlist in $(cat ${INTLIST_FILE}); do
      LOCAL_INTLIST=${LOCAL_INTLIST}:${INTLIST_ROOT}/${_intlist}
    done
    export INTLIST=${LOCAL_INTLIST}
  fi
}

function load_introot {
  [[ ! -f ~/.load_alma ]] && return
  local INTROOT_DIRECTORY=$PWD/$([[ -n $1 ]] && echo $1 || echo INTROOT)
  getTemplateForDirectory INTROOT ${INTROOT_DIRECTORY} 2>&1 > /dev/null
  unset INTROOT
  export INTROOT=${INTROOT_DIRECTORY};
  load_profile
}

function get_git {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \[\1\]/'
}

function rename_tmux_window {
  if [[ -n "${TMUX}" ]]; then
    tmux rename-window "${PWD}$(get_git)"
  fi
}

load_intlist
load_almasw
#load_nvm
#load_conda

export PATH=${HOME}/bin:${PATH}
export EDITOR='emacs -nw'
export PS1='$(rename_tmux_window)> '
