# /etc/profile.d/gein.sh

gpkg() {
    if [ $EUID -ne 0 ]; then
        if [ -e $(command -v sudo) ]; then
            SU="sudo"
        else
            SU="su -c"
        fi
    fi

    case $1 in
        -s|sync)
            echo "gpkg: syncing Portage..."
            $SU emerge -q --sync &&
            echo "gpkg: Portage sync completed"
            ;;

        -i|install)
            $SU emerge -av --quiet-build ${@:2}
            ;;

        -r|remove)
            $SU emerge -avc --quiet-build ${@:2}
            $SU revdep-rebuild -q
            ;;

        -p|purge)
            $SU gpkg -r $(qlist -CI ${@:2})
            $SU revdep-rebuild -q
            ;;

        -c|clean)
            $SU eclean --deep distfiles
            $SU revdep-rebuild -q
            ;;

        -u|update)
            case $2 in
                -w|world)
                    $SU emerge -avuDU --keep-going --with-bdeps=y \
                        --quiet-build @world
                    $SU revdep-rebuild -q
                    ;;

                -s|system)
                    $SU emerge -avuDN --quiet-build @system
                    $SU revdep-rebuild -q
                    ;;

                *)
                    echo "gpkg: update: Available options:"
                    echo "  world          Update world packages"
                    echo "  system         Update system packages"
                    ;;
            esac
            ;;

        *)
            echo "gpkg: Available options:"
            echo "  -s, sync       Sync Portage"
            echo "  -i, install    Install a package"
            echo "  -r, remove     Safely remove a package"
            echo "  -p, purge      Remove unneeded packages"
            echo "  -c, clean      Remove unneeded packages"
            echo "  -u, update     Update packages"
            echo "    world        Update world packages"
            echo "    system       Update system packages"
            ;;
    esac
}
