((nix-mode . ((eval . (setq-local eglot-server-programs
                                  `((nix-mode . ("nixd")) . ,eglot-server-programs))))))
