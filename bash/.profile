# atejeda profile

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

function encrypt_file {
  gpg -c --cipher-algo AES256 $1
}

function load_almasw {
  [[ ! -f ~/.alma ]] && return
  source /alma/ACS-current/ACSSW/config/.acs/.bash_profile.acs
  echo "ALMA profile loaded..."
  echo "* * *"
  echo "ACS     = $ALMASW_RELEASE"
  echo "INTROOT = ${INTROOT:none}"
  echo "INTLIST = ${INTLIST:none}"
  echo "* * *"
}

function load_profile {
  . ~/.bashrc
}

function load_nvm {
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
}

function load_conda {
  export PATH="~/miniconda3/bin:$PATH"
}

function load_intlist {
  [[ ! -f ~/.alma ]] && return
  local INTLIST_ROOT=/alma/ACS-current/intlist
  local INTLIST_FILE=${INTLIST_ROOT}/intlist
  unset INTLIST
  if [[ -f ${INTLIST_FILE} ]]; then
    for _intlist in $(cat ${INTLIST_FILE}); do
      export INTLIST=${INTLIST}:${INTLIST_ROOT}/${_intlist}
    done
  fi
}

function load_introot {
  [[ ! -f ~/.alma ]] && return
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

export PATH=${HOME}/bin:${PATH}
export EDITOR=vim
#export PS1='$(rename_tmux_window)\e[1m\e[32m>\e[39m\e[21m '
export PS1='$(rename_tmux_window)> '

# export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.161-3.b14.el6_9.x86_64
# ulimit -u 4096
