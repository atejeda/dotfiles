# atejeda
# [[ -f $HOME/.atejeda.alma.profile ]] && . $HOME/.atejeda.alma.profile

# intlist 
INTLIST_ROOT=/alma/intlist
INTLIST_FILE=$INTLIST_ROOT/intlist
if [[ -f $INTLIST_FILE ]]; then
    for _intlist in $(cat $INTLIST_FILE); do
        export INTLIST=$INTLIST_ROOT/$_intlist:$INTLIST
    done
fi

# alma
PROFILE_ALMA=/alma/ACS-current/ACSSW/config/.acs/.bash_profile.acs
[[ -f $PROFILE_ALMA ]] && . $PROFILE_ALMA

# functions
function alma-show {
    echo "{"
    printf "  INTLIST = {\n"
    if [[ -f $INTLIST_FILE ]]; then
        for _intlist in $(cat $INTLIST_FILE); do
            printf "    $INTLIST_ROOT/$_intlist\n"
        done
    fi
    printf "  }\n"
    printf "  ACS = $(basename `readlink -f /alma/ACS-current`)\n"
    printf "  INTROOT = $INTROOT\n"
    echo "}"
}

