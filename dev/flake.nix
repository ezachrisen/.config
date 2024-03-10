{
  description = "default shell configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixunstable2.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixunstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    goplspkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, nixunstable, nixunstable2, flake-utils, emacs-overlay, goplspkgs }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        oldpkgs = nixpkgs.legacyPackages.${system};
	      unstablepkgs2 = import nixunstable2 { inherit system; };
	      unstablepkgs = import nixunstable { inherit system; };
	      gopls = import goplspkgs { inherit system; };
        pkgs = import nixpkgs {
          config = {
            allowUnfree = true;
        };

        inherit system; # required to pass system to the builder

#          overlays = [
#		        (import (builtins.fetchTarball {
#		          url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
 #                         sha256 = "0ydzggbrgcbidg7ivnxgmg8p8jgd3d5jx8lbywggndqbzk59ms4i";
#		        }))
#		      ];
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

          spannercli = oldpkgs.buildGoModule {
	          name = "spannercli";
            pname = "spannercli";
            src = oldpkgs.fetchFromGitHub {
              owner = "cloudspannerecosystem";
              repo = "spanner-cli";
              rev = "a80699f";
              sha256 = "sha256-Vz6vosf24rhMMOmwygNI/9thBDEukCG7Uuw81mm5E5c=";
            };
						subPackages = ["."];
            doCheck = false;
            vendorSha256 = "sha256-5CY2h+eP96QpP/KHUvNIoJ7ggZJbPzNafCS6RB7Q+pQ=";
	  };

          pkgsite = oldpkgs.buildGoModule {
            name = "pkgsite";
            src = oldpkgs.fetchFromGitHub {
              owner = "golang";
              repo = "pkgsite";
              rev = "ea43129276ed1c7e85557321bd2af0504fbc4a7f";
              sha256 = "sha256-AzDXy7axfPVXBm7pQQCwzrkq/QdROZNz0p2KUlgOAnA=";
            };
            doCheck = false;
            subPackages = [ "cmd/pkgsite" ];
            vendorSha256 = "sha256-dsBYeYYk6MonC8NttrbV2nHR1A2OU7iyeiVCj0O887A=";
          };
        };
      in
        {
          devShell = oldpkgs.mkShell {
            buildInputs = with pkgs; [
              unstablepkgs.emacs
              unstablepkgs.fzf

              # Go 
              unstablepkgs.go_1_22
              gopls.gopls

              # Go tools
              devtools.staticcheck
              devtools.pkgsite
							devtools.spannercli
              unstablepkgs.golangci-lint
              unstablepkgs.gotools

              # Protobuf 
              unstablepkgs.buf
              pkgs.protobuf3_19
              unstablepkgs.protoc-gen-go
              unstablepkgs.protoc-gen-go-grpc
              unstablepkgs.protoc-gen-validate
              unstablepkgs.protoc-gen-grpc-web
              

              # Node 
              #unstablepkgs.nodejs-19_x
              pkgs.nodePackages.typescript
              pkgs.nodePackages.npm

              # Misc 
              unstablepkgs2.neovim
              pkgs.xz
              pkgs.zip
              pkgs.unzip
              pkgs.pandoc
              pkgs.ripgrep
              pkgs.docker
              pkgs.bat
              pkgs.cmake
              pkgs.clang
              pkgs.ninja
              pkgs.mesa
              pkgs.pkg-config
              pkgs.ispell
              pkgs.nodejs_20
              pkgs.xdg-utils

            ];


            shellHook = ''
              echo "Welcome to Nix shell"
              source ./git-prompt.sh
              # emacs () {
              # # Added to .bashrc to set the TERM info for Emacs
              #     if test -f "$HOME/.terminfo/x/xterm-emacs-leg" && ( test "$LC_TERMINAL" == "iTerm2"  || test "$COLORTERM" == "truecolor" )
              #     then
              #         TERM=xterm-emacs-leg command emacs "$@"
              #     else
              #         command emacs "$@"
              #     fi
              # }
							#alias emacs='emacs -nw'
              alias e='emacs -nw'
              ./emacs_terminfo.sh
              force_color_prompt=yes
              export COLORTERM=truecolor
              export GIT_PS1_SHOWDIRTYSTATE=true
              export GIT_PS1_SHOWCOLORHINTS=true 
              RED="$(tput setaf 1)"
              RESET="$(tput sgr0)"
              PS1='\[\033[01;32m\]\h\[\033[00m\] \W$(__git_ps1 " ''${RED}(%s)''${RESET}") $ '
              alias vi='nvim'
						  export CHROME_EXECUTABLE=/snap/bin/chromium
              cd ~
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
