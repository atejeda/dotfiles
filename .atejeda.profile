# atejeda
# [[ -f $HOME/dotfiles/.atejeda_profile ]] && . $HOME/dotfiles/.atejeda_profile

# locales
#export LC_ALL=en_US.UTF-8
#export LANG=en_US.UTF-8
#export LANGUAGE=en_US.UTF-8
#export LC_CTYPE=en_US.UTF-8

# aliases
#alias ll='ls -lhtrF'
#alias la='ls -A'
#alias l='ls -CF'

# ssh-connections
alias encrypt-file='gpg -c --cipher-algo AES256'
alias reload-profile=". ~/.bashrc"

# functions

# exports
export PATH=$HOME/bin:$PATH
export EDITOR=vim

# alma
PROFILE_ALMA=/alma/ACS-current/ACSSW/config/.acs/.bash_profile.acs
INTLIST_ROOT=/alma/intlist
INTLIST_FILE=$INTLIST_ROOT/intlist

# setup the the intlist
if [[ -f $INTLIST_FILE ]]; then
    for _intlist in $(cat $INTLIST_FILE); do
        export INTLIST=$INTLIST_ROOT/$_intlist:$INTLIST
    done
fi

# load alma profile
[[ -f $PROFILE_ALMA ]] && . $PROFILE_ALMA

function show-alma {
    if [[ -f $INTLIST_FILE ]]; then
        echo "intlist:"
        for _intlist in $(cat $INTLIST_FILE); do
            echo "$INTLIST_ROOT/$_intlist"
        done
    fi
}
