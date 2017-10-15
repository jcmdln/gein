# /etc/alias

## cp
alias cp="cp -ip"
alias cr="cp -ir"

## curl
alias hurl="curl -f#LO"

## df
alias df="df -h"
alias di="df -hi"

## free
alias free="free -h"

## ls
ls="-hSF --group-directories-first --color=auto"
alias l="ls $ls"
alias la="ls -aC $ls"
alias ll="ls -al $ls"
alias ls="ls -C $ls"

## mkdir
alias mkdir="mkdir -vp $@"

## mv
alias mv="mv -i"

## rm
alias rf="rm -rf"
alias rm="rm -i"
alias rr="rm -ir"


### Extras #######################################

mkcd() {
    mkdir -vp $1
    cd $1
}
