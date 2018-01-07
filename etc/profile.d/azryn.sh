# /etc/profile.d/azryn.sh

azryn() {
    if [ $EUID -ne 0 ]; then
        if [ -e $(command -v sudo) ]; then
            SU="sudo"
        else
            SU="su -c"
        fi
    fi

    case $1 in
        sync|-s)
            $SU emerge -v --sync
            ;;

        install|-i)
            $SU emerge -av --quiet-build ${@:2}
            ;;

        remove|-r)
            $SU emerge -avc --quiet-build ${@:2} && \
            $SU revdep-rebuild -q
            ;;

        update|-u)
            $SU emerge -avuDU --keep-going --with-bdeps=y --quiet-build @world && \
            $SU revdep-rebuild -q
            ;;

        upgrade|-U)
            $SU emerge -avuDN --quiet-build @system && \
            $SU revdep-rebuild -q
            ;;

        clean|-c)
            $SU emerge -avuDN --quiet-build @world && \
            $SU eclean --deep distfiles && \
            $SU revdep-rebuild -q
            ;;

        purge|-p)
            $SU azryn -r $(qlist -CI ${@:2})
            $SU revdep-rebuild -q
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
                $SU wget -q $Source/$cfg -O $cfg
            done

            $SU sed -i "s/MAKEOPTS=.*/MAKEOPTS=$MakeOpts/g;
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
