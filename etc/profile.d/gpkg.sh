# /etc/profile.d/gpkg.sh

gein() {
    if [ $EUID -ne 0 ]; then
        if [ -e $(command -v sudo) ]; then
            SU="sudo"
        else
            SU="su -c"
        fi
    fi

    case $1 in
        -s|sync)
            $SU emerge -q --sync
            ;;

        -i|install)
            $SU emerge -av --quiet-build ${@:2}
            ;;

        -r|remove)
            $SU emerge -avc --quiet-build ${@:2}
            $SU revdep-rebuild -q
            ;;

        -p|purge)
            $SU gein -r $(qlist -CI ${@:2})
            $SU revdep-rebuild -q
            ;;

        -c|clean)
            $SU emerge -avuDN --quiet-build @world
            $SU eclean --deep distfiles
            $SU revdep-rebuild -q
            ;;

        -u|update)
            $SU emerge -avuDU \
                --keep-going --with-bdeps=y --quiet-build @world
            $SU eclean --deep distfiles
            $SU revdep-rebuild -q
            ;;

        -U|upgrade)
            $SU emerge -avuDN --quiet-build @system
            $SU eclean --deep distfiles
            $SU revdep-rebuild -q
            ;;

        *)
            echo "gpkg: Available options:"
            echo "  -s, sync       Sync Portage"
            echo "  -i, install    Install a package"
            echo "  -r, remove     Safely remove a package"
            echo "  -p, purge      Remove unneeded packages"
            echo "  -c, clean      Remove unneeded packages"
            echo "  -u, update     Update @world without rebuild"
            echo "  -U, upgrade    Update @system and @world with rebuild"
            ;;
    esac
}
