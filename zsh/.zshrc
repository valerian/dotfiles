# key bindings
bindkey "e[3~" delete-char
bindkey "^[[1;5D" backward-word
bindkey "^[[1;5C" forward-word
# ----
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[3~" delete-char
bindkey "\e[2~" quoted-insert
bindkey "\e[5C" forward-word
bindkey "\eOc" emacs-forward-word
bindkey "\e[5D" backward-word
bindkey "\eOd" emacs-backward-word
bindkey "\ee[C" forward-word
bindkey "\ee[D" backward-word
# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
# for non RH/Debian xterm, can't hurt for RH/DEbian xterm
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line

# Set up the prompt

#autoload -Uz promptinit
#promptinit
#prompt adam1

autoload -U colors && colors
if [ $USER = 'root' ]
then
    PS1="%{$fg[white]%}%{$bg[red]%}%n%{$fg[black]%}@%{$fg[black]%}%m %{$fg[white]%}%~ %{$reset_color%}%\> "
else
    PS1="%{$fg[red]%}%n%{$reset_color%}@%{$fg[cyan]%}%m %{$fg[yellow]%}%~ %{$reset_color%}%\> "
fi


setopt histignorealldups sharehistory

# pushes the old directory onto the stack
setopt AUTO_PUSHD

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
# The following lines were added by compinstall

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _ignored _match _correct _approximate _prefix
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' ignore-parents parent pwd .. directory
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' match-original both
zstyle ':completion:*' max-errors 4
zstyle ':completion:*' menu select=1
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' word true

zstyle ':completion:*:directory-stack' list-colors '=(#b) #([0-9]#)*( *)==95=38;5;12'

zstyle :compinstall filename ~/.zshrc

autoload -Uz compinit
compinit

if [ -d "$HOME/bin" ]
then
    PATH="$HOME/bin:$PATH"
fi

# aliases
alias zshrc='emacs ~/.zshrc'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias gni='grep -rni'
alias gn='grep -rn'

alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -A'
alias lla='ls -lA'
alias llh='ls -lh'
alias llah='ls -lah'
alias l='ls -CF'
alias space='du -h --max-depth=1 | sort -hr'
alias j=jobs
alias fgfg=fg
alias e='emacs'
alias psg='ps ax | grep '

alias ga='git add'
alias gp='git push'
alias gpt='git push --tags'
alias gl='git lg'
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'
alias gm='git commit -m'
alias gma='git commit -am'
alias gb='git branch'
alias gc='git checkout'
alias gpu='git pull'
alias gcl='git clone'

alias sudo='sudo -i '
alias sudou='command sudo '

# copy with a progress bar.
alias cpv="rsync -poghb --backup-dir=/tmp/rsync -e /dev/null --progress --"

function command-exists { command -v $1 >/dev/null 2>&1 }

# live auto refreshing server (need npm install live-server)
if [ `command -v live-server 2>/dev/null` ]
then
    alias serve="live-server --host=0.0.0.0"
fi

# Cygwin only
if [ `uname -s | fgrep -i cygwin` ]
then
    alias ccd='cd $(cygpath -u "$1" 2>/dev/null)'

    if [ $DISPLAY ] && [ `command -v xset 2>/dev/null` ]
    then
        # higher keyboard speed in X environment
        xset r rate 150 100 >/dev/null 2>&1
    fi
fi

# disable annoying Ctrl-S flow freeze
stty -ixon

# toggle sudo with Ctrl-S
sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    if [[ $BUFFER == sudo\ * ]]; then
        LBUFFER="${LBUFFER#sudo }"
    else
        LBUFFER="sudo $LBUFFER"
    fi
}
zle -N sudo-command-line
bindkey "^s" sudo-command-line

# npm start or run
function npr
{
    if [ $# -lt 1 ]
    then
        npm start
    else
        npm run $*
    fi
}

# Recursive case insensitive file/folder search
function ff
{
    if [ $# -lt 1 ]
    then
        echo "usage: ff PATTERN [PATH]"
    elif [ $# -eq 1 ]
    then
        find . -iname "*$1*"
    else
        find $2 -iname "*$1*"
    fi
}

# Recursive Sed
function rsed
{
    if [ $# -lt 1 ]
    then
        echo "Recursive Sed usage: rsed SCRIPT [PATH]"
    elif [ $# -eq 1 ]
    then
        find ./ -type f -exec sed -i ${(q)1} {} +
    else
        find ${(qq)2} -type f -exec sed -i ${(q)1} {} +
    fi
}

# Recursive Search and Replace
function rsr
{
    if [ $# -lt 2 ]
    then
        echo "Recursive Search and Replace usage: rsr SEARCH REPLACE [PATH]"
    elif [ $# -ge 2 ]
    then
        searchScript=s/$(echo ${(q)1} | sed 's/\//\\\//g')/$(echo ${(q)2} | sed 's/\//\\\//g')/g
        if [ $# -eq 2 ]
        then
            find ./ -type f -exec sed -i $searchScript {} +
        else
            find ${(qq)3} -type f -exec sed -i $searchScript {} +
        fi
    fi
}

# Draws mandelbrot fractal
function mandelbrot {
   local lines columns colour a b p q i pnew
   ((columns=COLUMNS-1, lines=LINES-1, colour=0))
   for ((b=-1.5; b<=1.5; b+=3.0/lines)) do
       for ((a=-2.0; a<=1; a+=3.0/columns)) do
           for ((p=0.0, q=0.0, i=0; p*p+q*q < 4 && i < 32; i++)) do
               ((pnew=p*p-q*q+a, q=2*p*q+b, p=pnew))
           done
           ((colour=(i/4)%8))
            echo -n "\\e[4${colour}m "
        done
        echo
    done
}


# trust me, my term supports 256 colors :)
case "$TERM" in
    xterm*) TERM=xterm-256color
esac 

if [ -f ~/.zshrc_local ]
then
   source ~/.zshrc_local
fi
