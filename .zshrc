#liasr Luke's config for the Zoomer Shell

# Enable colors and change prompt:
autoload -U colors && colors
# PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%c%{$fg[red]%}]%{$reset_color%}$%b "

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# Make auto complete case insensitive:
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

# Load aliases and shortcuts if existent.
[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"

# Load zsh-syntax-highlighting; should be last.
# source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source /home/nobel/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# Git Prompt
source ~/.zsh/zsh-git-prompt/zshrc.sh

# Set prompt 
# PS1='%B %m %~%b $(git_super_status) %# '
# PS1=' %{$fg[light yellow]%}%~%b $(git_super_status) %# '
PS1=' %2d $(git_super_status) %# '

# Source bin for binaries
export PATH=~/.local/bin:$PATH

# Source java directory 
export PATH=~/Java/jdk1.8.0_231/bin:$PATH

# Source Rider
export PATH=~/Rider/"JetBrains Rider-2019.3"/bin:$PATH

# Source tfs cli tool
export PATH=~/Tfs/TEE-CLC-14.134.0:$PATH

# Source customs script directoy in dotfiles
export PATH=~/dotfiles/scripts:$PATH

# Source Postman 
export PATH=~/Postman/Postman:$PATH

# Source Clipmenu
export PATH=~/SuckLess/clipmenu:$PATH

# Source Datagrip
export PATH=~/Datagrip/DataGrip-2019.3.2/bin:$PATH

# Source netcoredbg
export PATH=~/NetCoreDbg/netcoredbg/bin:$PATH

# Save history accross sessions
setopt inc_append_history
setopt share_history

# Save directory history 
# setopt AUTO_PUSHD

# Set default editor to vanilla vim
# export EDITOR='program'

export TERM=xterm-256color

export TF_DIFF_COMMAND='kdiff3 %1 %2'

export FZF_DEFAULT_COMMAND='fd --type f -E "*bin*" -E "*obj*" -E "*dll*"'

export FZF_ALT_C_COMMAND='fd --type d -d 1 -E "*obj*" -E "*bin*"'

export FZF_CTRL_T_COMMAND='fd --type f -E "*bin*" -E "*obj*" -E "*dll*"'

# if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "&TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
# 	exec tmux
# fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/nobel/.sdkman"
[[ -s "/home/nobel/.sdkman/bin/sdkman-init.sh" ]] && source "/home/nobel/.sdkman/bin/sdkman-init.sh"
