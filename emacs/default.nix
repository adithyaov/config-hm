{ pkgs }:

{
  home.file.".emacs.d".recursive = true;
  home.file.".emacs.d".source = ./.emacs.d;

  programs.emacs.enable = true;
  programs.emacs.extraPackages = epkgs: with epkgs;
    [ ahk-mode
      column-enforce-mode
      eshell-git-prompt
      csharp-mode
      haskell-mode
      highlight-indent-guides
      solarized-theme
      impatient-mode
      ivy
      swiper
      counsel
      magit
      projectile
      counsel-projectile
      multiple-cursors
      avy
      hydra
      rust-mode
      highlight-function-calls
      org-tree-slide
      org
      expand-region
      ox-reveal
      s
      dash
      forge
      doom-themes
      ivy-posframe
      doom-modeline
      nix-mode
      exwm
      ace-window
      vue-mode
      elm-mode
      proced-narrow
      yaml-mode
      lsp-mode
      lsp-haskell
      lsp-ui
      fira-code-mode
      nix-buffer
      direnv
      org-static-blog
      helm
      helm-rg
      helm-projectile
      helm-exwm
      use-package
      leaf
    ];
}
