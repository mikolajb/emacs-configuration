((magit-blame "-w" "-M3" "-C3")
 (magit-commit)
 (magit-fetch "--prune" "--tags")
 (magit-rebase "--update-refs" "--autosquash" "--autostash" "--interactive")
 (magit-tag "--annotate" "--sign"))
