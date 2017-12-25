# /etc/profile.d/azryn.sh

azryn() {
    case $1 in
        cleanup|-c)
            sudo emerge -avuDN --quiet-build @world && \
            sudo revdep-rebuild
            ;;

        install|-i)
            sudo emerge -av --quiet-build ${@:2} && \
            sudo revdep-rebuild
            ;;

        remove|-r)
            sudo emerge -avc --quiet-build ${@:2} && \
            sudo revdep-rebuild
            ;;

        sync|-s)
            sudo emerge -v --sync
            ;;

        update|-u)
            sudo emerge -avuDN --quiet-build @world && \
            sudo revdep-rebuild
            ;;

        upgrade|-U)
            sudo emerge -avuDN --quiet-build @system && \
            sudo revdep-rebuild
            ;;

        *)
            echo "Available options:"
            echo "  cleanup, -c    Safely remove unused packages from @world"
            echo "  install, -i    Install a package"
            echo "  remove,  -r    Safely remove a package"
            echo "  sync,    -s    Sync portage"
            echo "  update,  -u    Update @world"
            echo "  upgrade, -U    Update @system"
            ;;
    esac
}
