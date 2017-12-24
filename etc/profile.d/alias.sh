# /etc/profile.d/alias.sh

## cd
mkcd() {
    mkdir -vp $1; cd $1
}

## cp
alias cp="cp -ip"
alias cr="cp -ir"

## curl
alias hurl="curl -f#LO"

## df
alias df="df -h"
alias di="df -hi"

## dig
alias dit="dig +noall +nocmd +answer +multiline"

## du
alias du="du -hs ./*"

## free
alias free="free -h"

## ls
ls="-hSF --group-directories-first --color=auto"
alias l="ls -C $ls"
alias la="ls -aC $ls"
alias ll="ls -al $ls"
alias ls="ls -C $ls"

## mkdir
alias mkdir="mkdir -vp $@"

## more
alias more="less"

## mv
alias mv="mv -i"

## nano
alias nano="nano -EOSWcimx"

## ps
alias pss="ps -af | head -1; ps -af | grep -v grep"
alias psw="watch 'ps -af | sort -hrk4'"

## rm
alias rf="rm -rf"
alias rm="rm -i"
alias rr="rm -ir"

## screen
alias screen="screen"

## scrot
alias screenshot="scrot -s ~/Pictures/'Screenshot-%Y%m%d'.png"

## tar
mktgz() {
    tar -vcfz $1.tar.gz $@
}
mktxz() {
    tar -vcfJ $1.tar.xz $@
}
alias untar="tar -vxf"


### Extras #######################################

weather() {
    curl http://wttr.in/$1
}
