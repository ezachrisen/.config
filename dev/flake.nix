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
              sha256 ="10cm60581b7ma5r4451vdbyrs5rvwg44fd5qbjli0swqhgzzg8h0";              
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
         };
      in
        {
          devShell = oldpkgs.mkShell {
            buildInputs = with pkgs; [
	            pkgs.emacsGit
              unstablepkgs.go_1_20
              unstablepkgs.gopls
              unstablepkgs.buf
              unstablepkgs.protoc-gen-go
              unstablepkgs.protoc-gen-go-grpc
              unstablepkgs.protoc-gen-grpc-web
              pkgs.pandoc
              pkgs.ispell
              unstablepkgs.golangci-lint
              unstablepkgs.protoc-gen-validate
              unstablepkgs.gotools
              unstablepkgs.protoc-gen-grpc-web
              devtools.staticcheck
            ];
            
            shellHook = ''
              echo "Welcome to Nix shell"
              emacs () {
              # Added to .bashrc to set the TERM info for Emacs
              echo "running emacs func"
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
            '';

            MY_NAME = "cow";
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
