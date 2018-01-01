# /etc/profile.d/azryn.sh

azryn() {
    case $1 in
        cleanup|-c)
            sudo emerge -avuDN --quiet-build @world && \
            sudo eclean --deep distfiles && \
            sudo revdep-rebuild -q
            ;;

        install|-i)
            sudo emerge -av --quiet-build ${@:2}
            ;;

        reconfig|-R)
            BaseUrl="https://raw.githubusercontent.com/Azryn/AzrynOS/master"
            Files="
                /etc/portage/repos.conf/gentoo.conf
                /etc/portage/make.conf
                /etc/portage/package.accept_keywords
                /etc/portage/package.env
                /etc/portage/package.license
                /etc/portage/package.use
                /etc/profile
                /etc/profile.d/alias.sh
                /etc/profile.d/azryn.sh
                /etc/profile.d/environment.sh
                /etc/Xresources
                /etc/emacs/default.el
                /etc/i3/config
                /etc/sudoers
                /etc/tmux.conf
                /etc/vimrc
                /etc/xinitrc
            "

            echo "azryn: WARNING: This will overwrite the following scripts:"
            for cfg in $Files; do echo $cfg; done
            read -ep "Proceed with replacing configurations? [Y/N]: " Proceed
            if echo $Proceed | grep -iq "^n"; then exit; fi

            MOPTS=$(grep MAKEOPTS /etc/portage/make.conf | sed 's/.*MAKEOPTS=//')
            VIDEO=$(grep VIDEO_CARDS /etc/portage/make.conf | sed 's/.*VIDEO_CARDS=//')

            for cfg in $Files; do
                sudo wget -q $BaseUrl/$cfg -O $cfg
            done

            sudo sed -i "s/MAKEOPTS=.*/MAKEOPTS=$MOPTS/g;
                    s/VIDEO_CARDS=.*/VIDEO_CARDS=$VIDEO/g" \
                        /etc/portage/make.conf

            unset BaseUrl mopts vcards
            ;;

        remove|-r)
            sudo emerge -avc --quiet-build ${@:2} && \
            sudo revdep-rebuild -q
            ;;

        sync|-s)
            sudo emerge -v --sync
            ;;

        update|-u)
            sudo emerge -avuDU \
                 --keep-going --with-bdeps=y --quiet-build @world && \
            sudo revdep-rebuild -q
            ;;

        upgrade|-U)
            sudo emerge -avuDN --quiet-build @system && \
            sudo revdep-rebuild -q
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
