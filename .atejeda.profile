# [[ -f $HOME/dotfiles/.atejeda.profile ]] && . $HOME/dotfiles/.atejeda.profile

# general aliases
alias encrypt-file='gpg -c --cipher-algo AES256'
alias reload-profile=". ~/.bashrc"

# exports
export PATH=$HOME/bin:$PATH
export EDITOR=vim
export PS1="# "

# load alma stuff
if [[ -f $HOME/.alma ]]; then

    PROFILE_ALMA=/alma/ACS-current/ACSSW/config/.acs/.bash_profile.acs
    INTLIST_ROOT=/alma/intlist
    INTLIST_FILE=$INTLIST_ROOT/intlist

    unset INTLIST
    if [[ -f $INTLIST_FILE ]]; then
        for _intlist in $(cat $INTLIST_FILE); do
            export INTLIST=$INTLIST:$INTLIST_ROOT/$_intlist
        done
    fi

    function show-alma {
        echo "ACS     = "`readlink -f /alma/ACS-current`
        [[ ! -z $INTROOT ]] && echo "INTROOT = "$INTROOT
        if [[ -f $INTLIST_FILE ]]; then
            echo "INTLIST ="
            for _intlist in $(cat $INTLIST_FILE); do
                echo "    $INTLIST_ROOT/$_intlist"
            done
        fi
    }

    function generate-introot {
        getTemplateForDirectory INTROOT $PWD/INTROOT >> /dev/null;
        export INTROOT=$PWD/$1_introot;
        reload-profile;
        show-alma;
    }

    # load profile
    . $PROFILE_ALMA
    [[ -f $HOME/.conda ]] && export PATH=/home/atejeda/anaconda2/bin:$PATH
    export PS1="# "
fi

# several outputs
echo "# `date`"
