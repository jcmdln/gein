# /etc/profile.d/azryn.sh

azryn() {
    if [ $EUID -ne 0 ]; then
        echo "azryn: You must run this script with elevated privileges"
        exit
    fi

    case $1 in
        sync|-s)
            emerge -v --sync
            ;;

        install|-i)
            emerge -av --quiet-build ${@:2}
            ;;

        remove|-r)
            emerge -avc --quiet-build ${@:2} && \
            revdep-rebuild -q
            ;;

        update|-u)
            emerge -avuDU --keep-going --with-bdeps=y --quiet-build @world && \
            revdep-rebuild -q
            ;;

        upgrade|-U)
            emerge -avuDN --quiet-build @system && \
            revdep-rebuild -q
            ;;

        clean|-c)
            emerge -avuDN --quiet-build @world && \
            eclean --deep distfiles && \
            revdep-rebuild -q
            ;;

        purge|-p)
            azryn -r $(qlist -CI ${@:2})
            revdep-rebuild -q
            ;;

        reconfig|-R)
            Source="https://raw.githubusercontent.com/Azryn/AzrynOS/master"
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

            MakeOpts=`grep MAKEOPTS /etc/portage/make.conf|sed 's/.*MAKEOPTS=//'`
            VideoCards=`grep VIDEO_CARDS /etc/portage/make.conf|sed 's/.*VIDEO_CARDS=//'`

            for cfg in $Files; do
                wget -q $BaseUrl/$cfg -O $cfg
            done

            sed -i "s/MAKEOPTS=.*/MAKEOPTS=$MakeOpts/g;
                    s/VIDEO_CARDS=.*/VIDEO_CARDS=$VideoCards/g" \
                        /etc/portage/make.conf

            unset Source MakeOpts VideoCards
            ;;

        *)
            echo "azryn: Available options:"
            echo "  -c, clean      Remove unneeded packages"
            echo "  -i, install    Install a package"
            echo "  -p, purge      Remove unneeded packages"
            echo "  -r, remove     Safely remove a package"
            echo "  -s, sync       Sync portage"
            echo "  -u, update     Update @world without rebuild"
            echo "  -U, upgrade    Update @system and @world with rebuild"
            echo "  -C config      Get latest configuration files"
            ;;
    esac
}
