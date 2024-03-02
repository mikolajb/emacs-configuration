((magit-commit "--signoff")
 (magit-fetch "--prune" "--tags")
 (magit-rebase "--update-refs" "--autostash" "--interactive")
 (magit-tag "--annotate" "--sign"))
