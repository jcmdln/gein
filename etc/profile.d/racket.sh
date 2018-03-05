# /etc/profile.d/racket.sh

RKTPATH="/usr/racket:/usr/local/racket"
RKTPATH="$RKTPATH:$HOME/racket:$HOME/.racket:$HOME/.local/racket"

RKTBIN="/usr/racket/bin:/usr/local/racket/bin"
RKTBIN="$RKTBIN:$HOME/racket/bin:$HOME/.racket/bin:$HOME/.local/racket/bin"

RKTMAN="/usr/racket/man:/usr/local/racket/man"
RKTMAN="$RKTMAN:$HOME/racket/man:$HOME/.racket/man:$HOME/.local/racket/man"

export PATH="$PATH:$RKTBIN"
export MANPATH="$MANPATH:$RKTMAN"

unset RKTPATH RKTBIN RKTMAN
