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
        -S|sync)
            echo "gpkg: syncing Portage..."
            $SU emerge -q --sync &&
            echo "gpkg: Portage sync completed"
            ;;

        -i|install)
            $SU emerge -av --quiet-build ${@:2}
            ;;

        -r|--remove)
            $SU emerge -avc --quiet-build ${@:2}
            ;;

        -p|--purge)
            $SU gpkg -r $(qlist -CI ${@:2})
            ;;

        -c|--clean)
            $SU emerge -av --depclean --quiet-build
            ;;

        -u|--update)
            case $2 in
                -w|--world)
                    $SU emerge -avuDU --keep-going --with-bdeps=y \
                        --quiet-build @world
                    ;;

                -s|--system)
                    $SU emerge -avuDN --quiet-build @system
                    ;;

                *)
                    echo "gpkg: update: Available options:"
                    echo "  -w, --world      Update world packages"
                    echo "  -s, --system     Update system packages"
                    ;;
            esac
            ;;

        *)
            echo "gpkg: Available options:"
            echo "  -S, --sync       Sync Portage"
            echo "  -i, --install    Install a package"
            echo "  -r, --remove     Safely remove a package"
            echo "  -p, --purge      Remove unneeded packages"
            echo "  -c, --clean      Remove unneeded packages"
            echo "  -u, --update     Update packages"
            echo "    -w, --world    Update world packages"
            echo "    -s, --system   Update system packages"
            ;;
    esac
}
