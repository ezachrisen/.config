{
  description = "default shell configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixunstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, nixunstable, flake-utils, emacs-overlay }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        oldpkgs = nixpkgs.legacyPackages.${system};
	unstablepkgs = import nixunstable { inherit system; };
        pkgs = import nixpkgs {
          config = {
            allowUnfree = true;
          };

          inherit system; # required to pass system to the builder

          overlays = [
		        (import (builtins.fetchTarball {
		          url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
			  sha256 = "0ydzggbrgcbidg7ivnxgmg8p8jgd3d5jx8lbywggndqbzk59ms4i";
		        }))
		      ];
        };

         devtools = {
            staticcheck = oldpkgs.buildGoModule {
              name = "staticcheck";
              src = oldpkgs.fetchFromGitHub {
                owner = "dominikh";
                repo = "go-tools";
                rev = "2023.1.2";
                sha256 = "sha256-Xnylkv0n3FExQ4e4pmD6DAUqGtud80wHHoVY56UXfOU";
              };
              doCheck = false;
              subPackages = [ "cmd/staticcheck" ];
              vendorSha256 = "sha256-o9UtS6AMgRYuAkOWdktG2Kr3QDBDQTOGSlya69K2br8";
	    };
            pkgsite = oldpkgs.buildGoModule {
              name = "pgsite";
              src = oldpkgs.fetchFromGitHub {
                owner = "golang";
                repo = "pkgsite";
                rev = "0a30e374544fc794cc1769dd04254f5be9b62c68";
                sha256 = "sha256-yZGaldBQVS6xpS4OOOmW4m6vN9TbdGGUMADY5Wq1RyU";
              };
              doCheck = false;
              subPackages = [ "cmd/pkgsite" ];
              vendorSha256 = "sha256-qqAUs1TWHEDMfWhi71GEaSkXKmbFpGGzzv5G6XTRG04=";
            };
         };
      in
        {
          devShell = oldpkgs.mkShell {
            buildInputs = with pkgs; [
              pkgs.emacsGit
#              unstablepkgs.go_1_20
              unstablepkgs.neovim
              unstablepkgs.gopls
  #            unstablepkgs.buf
  #            unstablepkgs.protoc-gen-go
  #            unstablepkgs.protoc-gen-go-grpc
              unstablepkgs.fzf
  #            unstablepkgs.protoc-gen-grpc-web
              pkgs.pandoc
              pkgs.ripgrep
              pkgs.docker
              pkgs.bat
              pkgs.cmake
              pkgs.clang
              pkgs.ninja
              pkgs.pkg-config
              pkgs.ispell
    #         pkgs.nodejs-19_x
              pkgs.xdg-utils
   #     pkgs.nodePackages.typescript
   #            pkgs.nodePackages.npm
          #    unstablepkgs.protobuf3_19
              unstablepkgs.golangci-lint
         #     unstablepkgs.protoc-gen-validate
              unstablepkgs.gotools
       #      unstablepkgs.protoc-gen-grpc-web
              devtools.staticcheck
              devtools.pkgsite
            ];

            shellHook = ''
              echo "Welcome to Nix shell"
              source ./git-prompt.sh
              emacs () {
              # Added to .bashrc to set the TERM info for Emacs
                  if test -f "$HOME/.terminfo/x/xterm-emacs-leg" && ( test "$LC_TERMINAL" == "iTerm2"  || test "$COLORTERM" == "truecolor" )
                  then
                      TERM=xterm-emacs-leg command emacs "$@"
                  else
                      command emacs "$@"
                  fi
              }
              alias e='emacs'
              ./emacs_terminfo.sh
              force_color_prompt=yes
              export COLORTERM=truecolor
              export GIT_PS1_SHOWDIRTYSTATE=true
              export GIT_PS1_SHOWCOLORHINTS=true 
              RED="$(tput setaf 1)"
              RESET="$(tput sgr0)"
              PS1='\[\033[01;32m\]\h\[\033[00m\] \W$(__git_ps1 " ''${RED}(%s)''${RESET}") $ '
              alias vi='nvim'
            '';
          };
        }
    );
}


  # {
  #     inputs = {
  # 	nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";
  # 	emacs-overlay.url = "github:nix-community/emacs-overlay";
  # 	flake-utils.url = "github:numtide/flake-utils";
  #     };

  #     outputs = { self, nixpkgs, emacs-overlay,flake-utils, ... }:
  #        flake-utils.lib.eachDefaultSystem (system:
  #           let
  # 	    pkgs =  import nixpkgs {
  #                config = {
  #                 allowUnfree = true;
  #                };
  #                 overlays = [
  # 		 (import (builtins.fetchTarball {
  # 		   url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
  # 		 }))
  # 	       ];
  #            };
  # 	   oldpkgs = nixpkgs.legacyPackages.${system};
  #          in {
  #            shell =
  #                oldpkgs.mkShell {
  # 		buildInputs = [
  #                   pkgs.emacsGit
  #                 ];
  # 	       };
  #          }
  #        );
  #      }
  # #{
  # #  inputs = {
  # #	nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
  # #        nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  # #emacs-overlay.url = "github:nix-community/emacs-overlay";
  # #  };
  # #
  # #  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, ...  }:
  # #    let
  # # pkgs = nixpkgs.legacyPackages.x86_64-linux;
  # #          nixpkgs.overlays = [
  # #            (inputs.emacs-overlay.overlay)
  # #            (inputs.neovim-nightly-overlay.overlay)
  # #          ];
  # #    in {
  # #
  # #      devShell.x86_64-linux =
  # #        pkgs.mkShell {
  # #	  buildInputs = [
  # #nixpkgs.overlays.emacs.emacsGit
  # #	  ];
  # #	};
  # #   };
  # #}
