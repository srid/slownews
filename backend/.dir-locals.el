((nil . (
  (dante-target . "backend")
  (eval . (setq dante-project-root (file-name-directory (directory-file-name (locate-dominating-file buffer-file-name dir-locals-file)))))
  (dante-repl-command-line . ("nix-shell" "--run" (concat "cabal new-repl " dante-target " --builddir=dist/dante")))
)))
