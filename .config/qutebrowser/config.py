# Load existing settings made via :set
config.load_autoconfig()

config.bind("<Ctrl+n>", "completion-item-focus --history next", mode="command")
config.bind("<Ctrl+p>", "completion-item-focus --history prev", mode="command")

config.bind("<Ctrl+j>", "tab-next")
config.bind("<Ctrl+k>", "tab-prev")

config.unbind("<Ctrl+w>")
config.bind('<Ctrl-w>', 'rl-backward-kill-word', mode="command")

config.unbind("J")
config.unbind("K")

# config.bind(" ,M hint links spawn mpv {hint-url}
