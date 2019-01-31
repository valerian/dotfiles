dotfilesRelPath=`realpath --relative-to=$HOME $(dirname "$0")`
echo Installing from $HOME/$dotfilesRelPath to $HOME
echo "This will install ~/.zshrc, ~/.gitconfig, ~/.emacs.d"
echo "(if files/folders already exist, backups will be made)"
echo

while true; do
    read -p "Do you wish to proceed? " yn
    case $yn in
        [Yy]* ) break;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
done

mv $HOME/.zshrc $HOME/.zshrc.dotfiles.old 2> /dev/null
ln -s $dotfilesRelPath/zsh/.zshrc $HOME/.zshrc
echo $HOME/.zshrc '<=' $HOME/$dotfilesRelPath/zsh/.zshrc

mv $HOME/.gitconfig $HOME/.gitconfig.dotfiles.old 2> /dev/null
ln -s $dotfilesRelPath/git/.gitconfig $HOME/.gitconfig
echo $HOME/.gitconfig '<=' $HOME/$dotfilesRelPath/git/.gitconfig

mv $HOME/.emacs.d $HOME/.emacs.d.dotfiles.old 2> /dev/null
mkdir $HOME/.emacs.d
ln -s ../$dotfilesRelPath/emacs/init.el $HOME/.emacs.d/init.el
ln -s ../$dotfilesRelPath/emacs/init.d $HOME/.emacs.d/init.d
ln -s ../$dotfilesRelPath/emacs/libraries $HOME/.emacs.d/libraries
echo $HOME/.emacs.d '<=' $HOME/$dotfilesRelPath/emacs
