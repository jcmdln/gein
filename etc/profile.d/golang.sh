# /etc/profile.d/golang.sh

GOPATH="$HOME/.go"
GOBIN="$GOPATH/bin"
GODOC="$GOPATH/doc"

export GOPATH GOBIN GODOC
export PATH="$PATH:$GOBIN"
