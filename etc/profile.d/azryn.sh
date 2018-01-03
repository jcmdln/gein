# /etc/profile.d/azryn.sh

azryn() {
    case $1 in
        sync|-s)
            sudo emerge -v --sync
            ;;

        install|-i)
            sudo emerge -av --quiet-build ${@:2}
            ;;

        remove|-r)
            sudo emerge -avc --quiet-build ${@:2} && \
            sudo revdep-rebuild -q
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

        clean|-c)
            sudo emerge -avuDN --quiet-build @world && \
            sudo eclean --deep distfiles && \
            sudo revdep-rebuild -q
            ;;

        purge|-p)
            azryn -r $(qlist -CI ${@:2})
            sudo revdep-rebuild -q
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
                sudo wget -q $BaseUrl/$cfg -O $cfg
            done

            sudo sed -i "s/MAKEOPTS=.*/MAKEOPTS=$MakeOpts/g;
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
