# /etc/profile.d/gein.sh

gein() {
    if [ $EUID -ne 0 ]; then
        echo "gein: Current user has insufficient permissions"
        if [ -e $(command -v sudo) ]; then
            echo "gein: Found 'sudo' to proceed"
            SU="sudo"
        else
            echo "gein: Fallback to 'su' to proceed"
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

        -c|configs)
            Source="https://raw.githubusercontent.com/jcmdln/gein/master"
            Files="
                /etc/portage/repos.conf/gentoo.conf
                /etc/portage/make.conf
                /etc/portage/package.accept_keywords
                /etc/portage/package.env
                /etc/portage/package.license
                /etc/portage/package.use
                /etc/profile
                /etc/profile.d/alias.sh
                /etc/profile.d/gein.sh
                /etc/profile.d/environment.sh
                /etc/Xresources
                /etc/emacs/default.el
                /etc/i3/config
                /etc/sudoers
                /etc/tmux.conf
                /etc/vimrc
                /etc/xinitrc
            "

            echo "gein: WARNING: This will overwrite the following scripts:"
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
            echo "gein: Available options:"
            echo "  -s, sync       Sync Portage"
            echo "  -i, install    Install a package"
            echo "  -r, remove     Safely remove a package"
            echo "  -p, purge      Remove unneeded packages"
            echo "  -c, clean      Remove unneeded packages"
            echo "  -u, update     Update @world without rebuild"
            echo "  -U, upgrade    Update @system and @world with rebuild"
            echo "  -c, config     Get latest configuration files"
            ;;
    esac
}
