# /etc/profile.d/alias.sh

## cp
alias cp="cp -ip"
alias cr="cp -ipr"

## curl
alias hurl="curl -f#LO"

## df
alias df="df -h"
alias di="df -hi"

## free
alias free="free -h"

## ls
ls="-hSF --group-directories-first --color=auto"
alias l="ls -C $ls"
alias la="ls -aC $ls"
alias ll="ls -al $ls"
alias ls="ls -C $ls"

## mkdir
alias mkdir="mkdir -vp"

## mv
alias mv="mv -i"

## rm
alias rf="rm -rf"
alias rm="rm -i"
alias rr="rm -ir"

## tar
mktgz() {
    tar -cfz $1.tar.gz $@
}
mktxz() {
    tar -cfJ $1.tar.xz $@
}
alias untar="tar -xf"


### Extras #######################################

weather() {
    curl http://wttr.in/$1
}
