# /etc/profile.d/azryn.sh

azryn() {
    case $1 in
        cleanup)
            sudo emerge -avc --quiet-build && \
            sudo emerge -avuUD --quiet-build @world && \
            sudo eclean packages && \
            sudo eclean-dist --deep --fetch-restricted
            ;;

        install)
            sudo emerge -av --quiet-build ${@:2}
            ;;

        update)
            sudo emerge -v --sync
            ;;

        upgrade)
            sudo emerge -avuDN --quiet-build @system && \
            sudo emerge -avuDN --quiet-build @world && \
            sudo emerge -avc --quiet-build && \
            sudo revdep-rebuild
            ;;

        *)
            echo "azryn: Invalid option: $@"
            echo "Available options:"
            echo "  cleanup    Sync portage and remove junk"
            echo "  install    Install a package"
            echo "  update     Sync portage and update"
            echo "  upgrade    Upgrade @system and @world"
            ;;
    esac
}
