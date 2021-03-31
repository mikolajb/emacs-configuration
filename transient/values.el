((magit-commit "--signoff")
 (magit-fetch "--prune" "--tags")
 (magit-tag "--annotate" "--sign"))
