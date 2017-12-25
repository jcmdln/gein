# /etc/profile.d/azryn.sh

azryn() {
    case $1 in
        cleanup|-c)
            sudo emerge -avuDN --quiet-build @world && \
            sudo eclean --deep distfiles && \
            sudo revdep-rebuild
            ;;

        install|-i)
            sudo emerge -av --quiet-build ${@:2} && \
            sudo revdep-rebuild
            ;;

        reconfig|-R)
            echo "azryn: WARNING: This will overwrite the following scripts:"
            echo "/etc/portage/repos.conf/gentoo.conf"
            echo "/etc/portage/make.conf"
            echo "/etc/portage/package.accept_keywords"
            echo "/etc/portage/package.env"
            echo "/etc/portage/package.license"
            echo "/etc/portage/package.use"
            echo "/etc/profile"
            echo "/etc/profile.d/alias.sh"
            echo "/etc/profile.d/azryn.sh"
            echo "/etc/profile.d/environment.sh"
            echo "/etc/Xresources"
            echo "/etc/emacs/default.el"
            echo "/etc/i3/config"
            echo "/etc/sudoers"
            echo "/etc/tmux.conf"
            echo "/etc/vimrc"
            echo "/etc/xinitrc"
            read -ep "Proceed with replacing configurations? [Y/N]: " Proceed
            if echo $Proceed | grep -iq "^n"; then exit; fi

            BaseUrl="https://raw.githubusercontent.com/Azryn/AzrynOS/master"

            echo "azryn: Replacing portage configuration files..."
            wget -q $BaseUrl/etc/portage/repos.conf/gentoo.conf \
                 -O /etc/portage/repos.conf/gentoo.conf
            wget -q $BaseUrl/etc/portage/make.conf -O /etc/portage/make.conf
            wget -q $BaseUrl/etc/portage/package.accept_keywords \
                 -O /etc/portage/package.accept_keywords
            wget -q $BaseUrl/etc/portage/package.env -O /etc/portage/package.env
            wget -q $BaseUrl/etc/portage/package.license \
                 -O /etc/portage/package.license
            wget -q $BaseUrl/etc/portage/package.use -O /etc/portage/package.use

            echo "azryn: Replacing profile configuration files..."
            wget -q $BaseUrl/etc/profile -O /etc/profile
            wget -q $BaseUrl/etc/profile.d/alias.sh -O /etc/profile.d/alias.sh
            wget -q $BaseUrl/etc/profile.d/azryn.sh -O /etc/profile.d/azryn.sh
            wget -q $BaseUrl/etc/profile.d/environment.sh \
                 -O /etc/profile.d/environment.sh

            echo "azryn: Replacing userland configuration files..."
            wget -q $BaseUrl/etc/Xresources       -O /etc/Xresources
            wget -q $BaseUrl/etc/emacs/default.el -O /etc/emacs/default.el
            wget -q $BaseUrl/etc/i3/config        -O /etc/i3/config
            wget -q $BaseUrl/etc/sudoers          -O /etc/sudoers
            wget -q $BaseUrl/etc/tmux.conf        -O /etc/tmux.conf
            wget -q $BaseUrl/etc/vimrc            -O /etc/vimrc
            wget -q $BaseUrl/etc/xinitrc          -O /etc/xinitrc
            
            unset BaseUrl
            ;;

        remove|-r)
            sudo emerge -avc --quiet-build ${@:2} && \
            sudo revdep-rebuild
            ;;

        sync|-s)
            sudo emerge -v --sync
            ;;

        update|-u)
            sudo emerge -avuDU --keep-going --with-bdeps=y @world && \
            sudo revdep-rebuild
            ;;

        upgrade|-U)
            sudo emerge -avuDN --quiet-build @system && \
            sudo revdep-rebuild
            ;;

        *)
            echo "Available options:"
            echo "  cleanup,  -c    Remove unneeded packages"
            echo "  install,  -i    Install a package"
            echo "  reconfig, -R    Get latest configuration files"
            echo "  remove,   -r    Safely remove a package"
            echo "  sync,     -s    Sync portage"
            echo "  update,   -u    Update @world without rebuild"
            echo "  upgrade,  -U    Update @system and @world with rebuild"
            ;;
    esac
}
