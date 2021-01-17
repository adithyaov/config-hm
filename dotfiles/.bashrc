# =========== REMAP: Capslock and Space ====================================

alias emacs-keys='
# Map an unused modifier keysym to the spacebars keycode and make it a
# control modifier. It needs to be an existing key so that emacs wont
# spazz out when you press it. Hyper_L is a good candidate.
spare_modifier="Hyper_L"
xmodmap -e "keycode 65 = $spare_modifier"
xmodmap -e "remove mod4 = $spare_modifier" # hyper_l is mod4 by default
xmodmap -e "add Control = $spare_modifier"

# Map space to an unused keycode (to keep it around for xcape to
# use).
xmodmap -e "keycode any = space"

# Finally use xcape to cause the space bar to generate a space when tapped.
xcape -e "$spare_modifier=space"

# Remap Capslock to escape
xmodmap -e "clear Lock"
xmodmap -e "keycode 66 = Escape NoSymbol Escape"

# Re - source nix profile
if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then source "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi
'

# $1 = Display
xmacs () {
    eval "Xephyr :$1 -ac -fullscreen -dpi 96 &"
    sleep 2
    eval "DISPLAY=:$1 emacs"
}
