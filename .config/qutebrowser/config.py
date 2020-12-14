# Load existing settings made via :set
config.load_autoconfig()

config.bind("<Ctrl+n>", "completion-item-focus --history next", mode="command")
config.bind("<Ctrl+p>", "completion-item-focus --history prev", mode="command")

config.bind("<Ctrl+j>", "tab-next")
config.bind("<Ctrl+k>", "tab-prev")

config.unbind("J")
config.unbind("K")
