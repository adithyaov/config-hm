with (import <nixpkgs> {});
runCommand "emacsFonts"
  { src = builtins.fetchGit {
      url = "https://github.com/domtronn/all-the-icons.el.git";
      rev = "d363bb3e73909be013fcf35e1458bb654ec5bbaa";
    };
  }
  ''
mkdir -p $out/share/fonts
cp $src/fonts/*.ttf $out/share/fonts/
''
