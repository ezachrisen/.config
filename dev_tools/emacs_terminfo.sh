#!/bin/bash

# To get 24 bit color in emacs, you need to set TERM to a terminal
# type that provides 24 bit color.
#
# For that, you probably don't need this script! If you're running
# emacs 27.1 or later, and if your system already has the terminal
# type `xterm-direct` installed (which you can check with `toe -a`), 
# then you can get 24 bit color by setting the environment
# variable `TERM=xterm-direct` before running emacs. This works with
# almoost all terminal emulators I have tested.
#
# So when do you want one of these custom terminal type definitions?
# You might want one if your terminal emulator does not work properly
# with `xterm-direct` (like blink.sh), if you are running an older
# version of emacs (26.1 or later), if you are on a system where
# `xterm-direct` is not already defined, or if you want a slightly
# more accurate 24 bit color where the first few color codes are treated 
# as normal 24 bit colors (certain dark blues) rather than 
# reserved as configurable palette colors.
# 
# Then, running this script adds two terminal types which provide support
# for emacs to use 24 bit color.
#
# Both of these terminal types start from the xterm256-color terminal
# type and add support for terminal operations which are used only by 
# emacs. They differ only in the format of the color code used:
#
# # xterm-emacs
#
# This uses the ISO 8613-6 format (e.g., "38:2::R:G:B"), which is
# recommended by emacs, and which works almost everywhere. But alas it
# does not work blink.sh, and presumably not in other terminal
# emulators based on hterm.
#
# xterm-emacs-leg
#
# This uses the legacy semicolon format for color escape codes (e.g.,
# "38;2;R;G;B"). In my testing, it works with all terminal
# emulators. This is also the format currently emitted by tmux
# and mosh. I would use this.
#

cd $(mktemp -d)
cat <<EOF > terminfo-24bit.src
xterm-emacs|xterm with 24-bit color for Emacs (ISO8613-6 format),
  use=xterm-256color,
  setb24=\E[48\:2\:\:%p1%{65536}%/%d\:%p1%{256}%/%{255}%&%d\:%p1%{255}%&%dm,
  setf24=\E[38\:2\:\:%p1%{65536}%/%d\:%p1%{256}%/%{255}%&%d\:%p1%{255}%&%dm,
xterm-emacs-leg|xterm with 24-bit color for Emacs (legacy format),
  use=xterm-256color,
  setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
  setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
EOF

tic -x -o ~/.terminfo terminfo-24bit.src
rm terminfo-24bit.src
