emacs () {
# Added to .bashrc to set the TERM info for Emacs
    if test -f "$HOME/.terminfo/x/xterm-emacs-leg" && ( test "$LC_TERMINAL" == "iTerm2"  || test "$COLORTERM" == "truecolor" )
    then
        TERM=xterm-emacs-leg command emacs "$@"
    else
        command emacs "$@"
    fi
}
