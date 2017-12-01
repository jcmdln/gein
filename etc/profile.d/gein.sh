#!/bin/env sh

gein-update() {
    emerge --sync
    emerge -av -uDN @system
    emerge -av -uDN @world
    emerge -av depclean
    revdep-rebuild -av
}
