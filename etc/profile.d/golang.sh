# /etc/profile.d/golang.sh

GOPATH="$HOME/.go:$HOME/Projects"
GOBIN="$GOPATH/bin"
GODOC="$GOPATH/doc"

export GOPATH GOBIN GODOC
export PATH="$PATH:$GOBIN"
