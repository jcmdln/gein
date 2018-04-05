# /etc/profile.d/defaults.sh

## User Directories
if [ "$(whoami)" != 'root' ]; then
    Folders="Documents Downloads Music Pictures Projects Videos"

    for Folder in $Folders; do
	[ -d "$HOME"/"$Folder" ] ||
	    mkdir "$HOME"/"$Folder"
    done

    unset Folders
fi
