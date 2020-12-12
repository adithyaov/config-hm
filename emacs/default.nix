{ pkgs }:

{
  home.file.".emacs.d".recursive = true;
  home.file.".emacs.d".source = ./.emacs.d;

  programs.emacs.enable = true;
  programs.emacs.overrides = self: super: {
    org-static-blog = super.melpaBuild rec {
      pname = "org-static-blog";
      version = "1.4.0";
      src = builtins.fetchGit {
        url = "https://github.com/adithyaov/org-static-blog.git";
        ref = "master";
      };
      # XXX Is the recipe's :fetcher and :branch even used?
      recipe = pkgs.writeText "recipe" ''
        (org-static-blog
        :repo "adithyaov/org-static-blog"
        :fetcher github)
      '';
    };
    helm-org-static-blog = super.melpaBuild rec {
      pname = "helm-org-static-blog";
      version = "0.0.0";
      src = builtins.fetchGit {
        url = "https://github.com/adithyaov/helm-org-static-blog.git";
        ref = "master";
      };
      # XXX Is the recipe's :fetcher and :branch even used?
      recipe = pkgs.writeText "recipe" ''
        (helm-org-static-blog
        :repo "adithyaov/helm-org-static-blog"
        :fetcher github)
      '';
    };
  };
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
      helm-org-static-blog
      helm
      helm-rg
      helm-projectile
      helm-exwm
      use-package
      leaf
      helm-hoogle
      bufler
      helm-bufler
      helm-ag
    ];
}
