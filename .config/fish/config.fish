
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/nobel/anaconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

export TERM=xterm-256color
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

source ~/.config/aliasrc
set -gx EDITOR nvim
conda activate CS6475
