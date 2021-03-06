# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
# shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
# shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
# if ! shopt -oq posix; then
#  if [ -f /usr/share/bash-completion/bash_completion ]; then
#    . /usr/share/bash-completion/bash_completion
#  elif [ -f /etc/bash_completion ]; then
#    . /etc/bash_completion
#  fi
# fi

# set nvim as default editor for shell programs/ranger/and more
VISUAL=nvim; export VISUAL
EDITOR=nvim; export EDITOR

######## START INFINALITY SETTINGS
# infinality settings (for full explanation, see /etc/fonts/infinality/ README
SET_XFT_SETTINGS=true

XFT_SETTINGS="
Xft.antialias:  1
Xft.autohint:   0
Xft.dpi:        96
Xft.hinting:    1
Xft.hintstyle:  hintfull
Xft.lcdfilter:  lcddefault
Xft.rgba:       rgb
" 

if [ "$SET_XFT_SETTINGS" = "true" ]; then
  echo "$XFT_SETTINGS" | xrdb -merge > /dev/null 2>&1
fi

export INFINALITY_FT_FILTER_PARAMS="11 22 38 22 11"

export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=25

export INFINALITY_FT_STEM_FITTING_STRENGTH=25

export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=40

export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=true

export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0

export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=10

export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100

export INFINALITY_FT_USE_VARIOUS_TWEAKS=true

export INFINALITY_FT_GAMMA_CORRECTION="0 100"

export INFINALITY_FT_BRIGHTNESS="0"

export INFINALITY_FT_CONTRAST="0"

export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH="0"

export INFINALITY_FT_FRINGE_FILTER_STRENGTH="0"

export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH="10"

export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH="25"

export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=true

USE_STYLE="OSX"

### WARNING - NEEDS WORK - ALSO LIABLE TO CRASH APPLICATIONS ###
################# OSX STYLE #################
if [ "$USE_STYLE" = "OSX" ]; then

export INFINALITY_FT_FILTER_PARAMS="03 32 38 32 03"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=25
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=0
export INFINALITY_FT_STEM_FITTING_STRENGTH=0
export INFINALITY_FT_GAMMA_CORRECTION="1000 80"
export INFINALITY_FT_BRIGHTNESS="10"
export INFINALITY_FT_CONTRAST="20"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=false
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=false
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=0
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=0
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=false
export INFINALITY_FT_GLOBAL_EMBOLDEN_X_VALUE=0
export INFINALITY_FT_GLOBAL_EMBOLDEN_Y_VALUE=8
export INFINALITY_FT_BOLD_EMBOLDEN_X_VALUE=0
export INFINALITY_FT_BOLD_EMBOLDEN_Y_VALUE=0

fi



################# IPAD STYLE #################
if [ "$USE_STYLE" = "IPAD" ]; then

export INFINALITY_FT_FILTER_PARAMS="00 00 100 00 00"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=100
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=0
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=0
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=0
export INFINALITY_FT_STEM_FITTING_STRENGTH=0
export INFINALITY_FT_GAMMA_CORRECTION="1000 80"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="0"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=false
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=false
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=0
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=0
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=false
export INFINALITY_FT_GLOBAL_EMBOLDEN_X_VALUE=0
export INFINALITY_FT_GLOBAL_EMBOLDEN_Y_VALUE=0
export INFINALITY_FT_BOLD_EMBOLDEN_X_VALUE=0
export INFINALITY_FT_BOLD_EMBOLDEN_Y_VALUE=0

fi



################# UBUNTU STYLE #################
if [ "$USE_STYLE" = "UBUNTU" ]; then

export INFINALITY_FT_FILTER_PARAMS="11 22 38 22 11"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=0
export INFINALITY_FT_STEM_FITTING_STRENGTH=0
export INFINALITY_FT_GAMMA_CORRECTION="1000 80"
export INFINALITY_FT_BRIGHTNESS="-10"
export INFINALITY_FT_CONTRAST="15"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=false
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=0
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=0
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=false

fi



################# LINUX STYLE #################
if [ "$USE_STYLE" = "LINUX" ]; then

export INFINALITY_FT_FILTER_PARAMS="06 25 44 25 06"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=0
export INFINALITY_FT_STEM_FITTING_STRENGTH=0
export INFINALITY_FT_GAMMA_CORRECTION="0 100"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="0"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=false
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=0
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=false

fi


################# WINDOWS XP STYLE LIGHT #################
if [ "$USE_STYLE" = "WINDOWSXPLIGHT" ]; then

export INFINALITY_FT_FILTER_PARAMS="06 25 44 25 06"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=100
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=65
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=15
export INFINALITY_FT_STEM_FITTING_STRENGTH=15
export INFINALITY_FT_GAMMA_CORRECTION="1000 120"
export INFINALITY_FT_BRIGHTNESS="20"
export INFINALITY_FT_CONTRAST="30"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=false
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=30
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=true

fi


################# WINDOWS 7 STYLE LIGHT #################
if [ "$USE_STYLE" = "WINDOWS7LIGHT" ]; then

export INFINALITY_FT_FILTER_PARAMS="20 25 38 25 05"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=100
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=100
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=0
export INFINALITY_FT_STEM_FITTING_STRENGTH=0
export INFINALITY_FT_GAMMA_CORRECTION="1000 160"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="20"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=false
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=30
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=true

fi


################# WINDOWS XP STYLE #################
if [ "$USE_STYLE" = "WINDOWSXP" ]; then

export INFINALITY_FT_FILTER_PARAMS="06 25 44 25 06"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=100
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=65
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=15
export INFINALITY_FT_STEM_FITTING_STRENGTH=15
export INFINALITY_FT_GAMMA_CORRECTION="1000 120"
export INFINALITY_FT_BRIGHTNESS="10"
export INFINALITY_FT_CONTRAST="20"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=false
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=30
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=true

fi


################# WINDOWS 7 STYLE #################
if [ "$USE_STYLE" = "WINDOWS7" ]; then

export INFINALITY_FT_FILTER_PARAMS="20 25 42 25 06"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=100
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=65
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=0
export INFINALITY_FT_STEM_FITTING_STRENGTH=0
export INFINALITY_FT_GAMMA_CORRECTION="1000 120"
export INFINALITY_FT_BRIGHTNESS="10"
export INFINALITY_FT_CONTRAST="20"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=false
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=0
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=true

fi


############### VANILLA STYLE ##############
if [ "$USE_STYLE" = "VANILLA" ]; then

export INFINALITY_FT_FILTER_PARAMS="06 25 38 25 06"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=0
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=0
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=0
export INFINALITY_FT_STEM_FITTING_STRENGTH=0
export INFINALITY_FT_GAMMA_CORRECTION="0 100"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="0"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=false
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=false
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=0
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=0
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=false

fi


############### CLASSIC INFINALITY STYLE ##############
if [ "$USE_STYLE" = "CLASSIC" ]; then

export INFINALITY_FT_FILTER_PARAMS="06 25 38 25 06"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=0
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=0
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=0
export INFINALITY_FT_STEM_FITTING_STRENGTH=0
export INFINALITY_FT_GAMMA_CORRECTION="0 100"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="0"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=true
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=0
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=false

fi


################# NUDGE STYLE #################
if [ "$USE_STYLE" = "NUDGE" ]; then

export INFINALITY_FT_FILTER_PARAMS="11 22 38 22 11"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=25
export INFINALITY_FT_STEM_FITTING_STRENGTH=15
export INFINALITY_FT_GAMMA_CORRECTION="0 100"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="0"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=true
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=30
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=false

fi


################# PUSH STYLE #################
if [ "$USE_STYLE" = "PUSH" ]; then

export INFINALITY_FT_FILTER_PARAMS="11 22 38 22 11"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=75
export INFINALITY_FT_STEM_FITTING_STRENGTH=50
export INFINALITY_FT_GAMMA_CORRECTION="0 100"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="0"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=true
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=30
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=true

fi


################# INFINALITY STYLE #################
if [ "$USE_STYLE" = "INFINALITY" ]; then

export INFINALITY_FT_FILTER_PARAMS="11 22 38 22 11"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=5
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=25
export INFINALITY_FT_STEM_FITTING_STRENGTH=25
export INFINALITY_FT_GAMMA_CORRECTION="0 100"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="0"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=true
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=40
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=true

fi


################# SHOVE STYLE #################
if [ "$USE_STYLE" = "SHOVE" ]; then

export INFINALITY_FT_FILTER_PARAMS="11 22 38 22 11"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=100
export INFINALITY_FT_STEM_FITTING_STRENGTH=100
export INFINALITY_FT_GAMMA_CORRECTION="0 100"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="0"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=true
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=0
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=true

fi


################# SHARPENED INFINALITY STYLE #################
if [ "$USE_STYLE" = "SHARPENED" ]; then

export INFINALITY_FT_FILTER_PARAMS="11 22 38 22 11"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=65
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=25
export INFINALITY_FT_STEM_FITTING_STRENGTH=25
export INFINALITY_FT_GAMMA_CORRECTION="0 100"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="0"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=true
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=40
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=true

fi


################# DISABLED STYLE #################
if [ "$USE_STYLE" = "DISABLED" ]; then

export INFINALITY_FT_FILTER_PARAMS=
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=
export INFINALITY_FT_STEM_FITTING_STRENGTH=
export INFINALITY_FT_GAMMA_CORRECTION="0 100"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="0"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=false
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=false
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=false

fi


################# CUSTOM STYLE #################
if [ "$USE_STYLE" = "CUSTOM" ]; then

export INFINALITY_FT_FILTER_PARAMS="11 22 38 22 11"
export INFINALITY_FT_GRAYSCALE_FILTER_STRENGTH=0
export INFINALITY_FT_FRINGE_FILTER_STRENGTH=0
export INFINALITY_FT_AUTOHINT_HORIZONTAL_STEM_DARKEN_STRENGTH=10
export INFINALITY_FT_AUTOHINT_VERTICAL_STEM_DARKEN_STRENGTH=25
export INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_CHROMEOS_STYLE_SHARPENING_STRENGTH=0
export INFINALITY_FT_STEM_ALIGNMENT_STRENGTH=100
export INFINALITY_FT_STEM_FITTING_STRENGTH=100
export INFINALITY_FT_GAMMA_CORRECTION="0 100"
export INFINALITY_FT_BRIGHTNESS="0"
export INFINALITY_FT_CONTRAST="0"
export INFINALITY_FT_USE_VARIOUS_TWEAKS=true
export INFINALITY_FT_AUTOHINT_INCREASE_GLYPH_HEIGHTS=true
export INFINALITY_FT_AUTOHINT_SNAP_STEM_HEIGHT=100
export INFINALITY_FT_STEM_SNAPPING_SLIDING_SCALE=0
export INFINALITY_FT_USE_KNOWN_SETTINGS_ON_SELECTED_FONTS=true

fi

######## END INFINALITY SETTINGS

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
