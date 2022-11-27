;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode . ((eval . (progn
                            (defvar-local /haskell-language-server-exec nil)
                            (setq-local flycheck-haskell-hlint-executable
                                        "/home/gallo/.xmonad/env/bin/hlint"
                                        flycheck-haskell-ghc-executable
                                        "/home/gallo/.xmonad/env/bin/ghc"
                                        /haskell-language-server-exec
                                        "/home/gallo/.xmonad/env/bin/haskell-language-server"
                                        eglot-server-programs
                                        `((haskell-mode
                                           . (,/haskell-language-server-exec "--lsp"))))))
                  (mode . flycheck))))
