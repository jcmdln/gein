# /etc/profile.d/env.sh

## Environment variables
export ARCHFLAGS="-arch x86_64"
export EDITOR="emacs -Q -nw"
export HISTTIMEFORMAT="(%m/%d/%y) %T "
export LANG="en_US.UTF-8"
export PAGER="${PAGER:-/usr/bin/less}"
export PROMPT_COMMAND="history -a"
export PS1="[\u@\h \w]\$ "
export SSH_KEY_PATH="${HOME}/.ssh/rsa_id"
export TERM="xterm"

## Golang
GOPATH="$HOME/.go"
GOBIN="$GOPATH/bin"
GODOC="$GOPATH/doc"
export GOBIN GODOC GOPATH

## Racket
RKTPATH="/usr/racket:/usr/local/racket:$HOME/racket:$HOME/.racket"
RKTBIN="$RKTROOT/bin"
RKTMAN="$RKTROOT/man"

## Path
CODEPATH="$GOBIN:$RKTPATH"
GLOBALPATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin"
LOCALPATH="$HOME/.local/bin:$HOME/.bin:$HOME/bin"
export PATH="$GLOBALPATH:$LOCALPATH:$CODEPATH"

## Man
PROGMAN="$RKTMAN"
GLOBALMAN="/usr/man:/usr/share/man:/usr/local/man:/usr/local/share/man"
LOCALMAN="$HOME/.local/man:$HOME/.local/share/man"
export MANPATH="$GLOBALMAN:$LOCALMAN:$PROGMAN"

## Create missing user directories
[ ! -e $HOME/Documents ] && mkdir $HOME/Documents
[ ! -e $HOME/Downloads ] && mkdir $HOME/Downloads
[ ! -e $HOME/Images    ] && mkdir $HOME/Images
[ ! -e $HOME/Projects  ] && mkdir $HOME/Projects
[ ! -e $HOME/Templates ] && mkdir $HOME/Templates
[ ! -e $HOME/Videos    ] && mkdir $HOME/Videos

## Add missing user configuration
[ ! -e $HOME/.xinitrc ] && cp /etc/xinitrc $HOME/.xinitrc
