#!/usr/bin/env bash
##
# Run this script to install all dependencies and configurations.
#

# Fast failure

set -e

# Hei, I'm

hollow="artolevi"

# Colorful logging

function error() {
  echo -e "\033[0;31m$*\033[0m"
}

function intro() {
  echo -e "\033[0;34m$*\033[0m"
}

function log() {
  echo -e "$*"
}

function section() {
  echo -e "\033[0;34m┅[■]┅ $*\033[0m"
}

function a_theme() {
  local text="═══[ $2 ]═══ ${*:3}"
  local length="${#text}"
  echo
	echo '╔════════════════════════════════════════════════════════════════════════════╗'
	echo -ne "╠\033[$1m$text\033[0m "
  printf '═%.0s' $(seq 1 $((75 - length)))
  echo '╣'
	echo '╚════════════════════════════════════════════════════════════════════════════╝'
}

function optional_theme() {
  a_theme "1;32" "$1" "${*:2}"
}

function inactive_theme() {
  a_theme "1;37" "$1" "${*:2}"
}

# Get the OS info

KERNEL_NAME=$(uname -s | tr '[:upper:]' '[:lower:]')
KERNEL_RELEASE=$(uname -r | tr '[:upper:]' '[:lower:]')
OS_NAME="unknown"
OS_VERSION="unknown"
case $KERNEL_NAME in
  darwin)
    OS_NAME=macos
    OS_VERSION=$(sw_vers -productVersion)
    ;;
  linux)
    OS_NAME=linux
    ;;
  *)
    ;;
esac

# Setup USER

if [ -z "$USER" ]; then
  USER=$(whoami)
fi

# Setup PATH

mkdir -p "$HOME/.local/bin/"
if [ ! -d "$HOME/.config/" ]; then
    error "$HOME/.config is missing..."
    log "Creating $HOME/.config..."
    mkdir -p "$HOME/.config"
fi

export PATH=$HOME/.local/bin:$PATH
export XDG_CONFIG_HOME=${GITHUB_WORKSPACE:-${XDG_CONFIG_HOME:-$HOME/.config}}
export XDG_CONFIG_CACHE="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export PATH=$XDG_CONFIG_HOME/bin:$PATH
export DEVELOPER=$HOME/Developer${USER:hollow}/personal

env_https=""

if [ -d "$XDG_CONFIG_HOME" ] && [ ! -d "$XDG_CONFIG_HOME/.git" ]; then
    cd "$XDG_CONFIG_HOME" && {
        log "Download environment"
        git init
        # clone via HTTPS, as most likely SSH is not yet available or configured
        git remote add origin $env_https
        git fetch
        git reset --hard origin/master
  }
fi

# Greetings
log
log ⢸⠉⣹⠋⠉⢉⡟⢩⢋⠋⣽⡻⠭⢽⢉⠯⠭⠭⠭⢽⡍⢹⡍⠙⣯⠉⠉⠉⠉⠉⣿⢫⠉⠉⠉⢉⡟⠉⢿⢹⠉⢉⣉⢿⡝⡉⢩⢿⣻⢍⠉⠉⠩⢹⣟⡏⠉⠹⡉⢻⡍⡇
log ⢸⢠⢹⠀⠀⢸⠁⣼⠀⣼⡝⠀⠀⢸⠘⠀⠀⠀⠀⠈⢿⠀⡟⡄⠹⣣⠀⠀⠐⠀⢸⡘⡄⣤⠀⡼⠁⠀⢺⡘⠉⠀⠀⠀⠫⣪⣌⡌⢳⡻⣦⠀⠀⢃⡽⡼⡀⠀⢣⢸⠸⡇
log ⢸⡸⢸⠀⠀⣿⠀⣇⢠⡿⠀⠀⠀⠸⡇⠀⠀⠀⠀⠀⠘⢇⠸⠘⡀⠻⣇⠀⠀⠄⠀⡇⢣⢛⠀⡇⠀⠀⣸⠇⠀⠀⠀⠀⠀⠘⠄⢻⡀⠻⣻⣧⠀⠀⠃⢧⡇⠀⢸⢸⡇⡇
log ⢸⡇⢸⣠⠀⣿⢠⣿⡾⠁⠀⢀⡀⠤⢇⣀⣐⣀⠀⠤⢀⠈⠢⡡⡈⢦⡙⣷⡀⠀⠀⢿⠈⢻⣡⠁⠀⢀⠏⠀⠀⠀⢀⠀⠄⣀⣐⣀⣙⠢⡌⣻⣷⡀⢹⢸⡅⠀⢸⠸⡇⡇
log ⢸⡇⢸⣟⠀⢿⢸⡿⠀⣀⣶⣷⣾⡿⠿⣿⣿⣿⣿⣿⣶⣬⡀⠐⠰⣄⠙⠪⣻⣦⡀⠘⣧⠀⠙⠄⠀⠀⠀⠀⠀⣨⣴⣾⣿⠿⣿⣿⣿⣿⣿⣶⣯⣿⣼⢼⡇⠀⢸⡇⡇⠇
log ⢸⢧⠀⣿⡅⢸⣼⡷⣾⣿⡟⠋⣿⠓⢲⣿⣿⣿⡟⠙⣿⠛⢯⡳⡀⠈⠓⠄⡈⠚⠿⣧⣌⢧⠀⠀⠀⠀⠀⣠⣺⠟⢫⡿⠓⢺⣿⣿⣿⠏⠙⣏⠛⣿⣿⣾⡇⢀⡿⢠⠀⡇
log ⢸⢸⠀⢹⣷⡀⢿⡁⠀⠻⣇⠀⣇⠀⠘⣿⣿⡿⠁⠐⣉⡀⠀⠁⠀⠀⠀⠀⠀⠀⠀⠀⠉⠓⠳⠄⠀⠀⠀⠀⠋⠀⠘⡇⠀⠸⣿⣿⠟⠀⢈⣉⢠⡿⠁⣼⠁⣼⠃⣼⠀⡇
log ⢸⠸⣀⠈⣯⢳⡘⣇⠀⠀⠈⡂⣜⣆⡀⠀⠀⢀⣀⡴⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢽⣆⣀⠀⠀⠀⣀⣜⠕⡊⠀⣸⠇⣼⡟⢠⠏⠀⡇
log ⢸⠀⡟⠀⢸⡆⢹⡜⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⠋⣾⡏⡇⡎⡇⠀⡇
log ⢸⠀⢃⡆⠀⢿⡄⠑⢽⣄⠀⠀⠀⢀⠂⠠⢁⠈⠄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⠂⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠀⠄⡐⢀⠂⠀⠀⣠⣮⡟⢹⣯⣸⣱⠁⠀⡇
log ⠈⠉⠉⠉⠉⠉⠉⠉⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⠉⠉⠉⠉⠉⠉⠉⠁
log
intro "Humans are just human. They represent neither our creators nor our arbiters. They do not decide our fate.\nI am me, and you are you. We're both alone, yet we're connected.\nSo I will create a new layer for you Lain..."
log
log "Kernel name:      $KERNEL_NAME"
log "Kernel release:   $KERNEL_RELEASE"
log "Operating system: $OS_NAME"
log "OS version:       $OS_VERSION"
log "User:             $USER"
log "XDG_CONFIG_HOME:  $XDG_CONFIG_HOME"
log

# Helpers

section "Defining layers"

function layer() {
  key=$(echo "$1" | tr '[:upper:]' '[:lower:]')
  local layer_ref="layer_$key"
  local ignore_layer_ref="layer_ignore_$key"
  layer="${!layer_ref}"
  ignore_layer="${!ignore_layer_ref}"
  if [[ ("$ALL" = "true" || "$layer" = "true") && "$ignore_layer" = "" ]]; then
    optional_theme "$1" "${@:2}"
    return 0
  else
    inactive_theme "$1" "${@:2}"
    return 1
  fi
}

function install_layer() {
  [[ "$ACTION" == "install" ]]
  return
}

function upgrade_layer() {
  [[ "$ACTION" == "upgrade" ]]
  return
}

function test_layer() {
  [[ "$ACTION" == "test" ]]
  return
}

function linux_layer() {
  [[ "$KERNEL_NAME" == "linux" ]]
  return
}

function macos_layer() {
  [[ "$OS_NAME" == "macos" ]]
  return
}

function check() {
  command -v "$1" >/dev/null 2>&1
}

# Setup variables

section "Defining variables"

ALL="true"
ACTION=
case $1 in
  install|upgrade|test)
    ACTION=$1
    shift
    ;;
  *)
    if [ -z "$1" ]; then
      ACTION=install
    else
      error "action '$1' is not supported"
      log "supported actions are: install, upgrade, test"
      exit 1
    fi
    ;;
esac

POSITIONAL=()
while [[ $# -gt 0 ]]
do
  if [[ "$1" != "" ]]; then
    if [[ "$1" = -* ]]; then
      key=$(echo "${1#-}" | tr '[:upper:]' '[:lower:]')
      declare -r "layer_ignore_$key=true"
    else
      key=$(echo "$1" | tr '[:upper:]' '[:lower:]')
      declare -r "layer_$key=true"
      ALL="false"
    fi
  fi
  shift
done
set -- "${POSITIONAL[@]}" # restore positional parameters

if [[ "$INTERACTIVE" = "" ]]; then
  INTERACTIVE=true
fi

# Lock

LOCK_FILE=$XDG_CACHE_HOME/install/install.lock
if [ -f "$LOCK_FILE" ]; then
  error "
Yet another layer is being connected to you...

One must either disconnect or embrace the horrors of the unknown and
manually delete the $LOCK_FILE"
  exit 1
fi
mkdir -p "$(dirname "$LOCK_FILE")"
touch "$LOCK_FILE"

function unlock() {
  rm -rf "$LOCK_FILE"
}

trap unlock INT TERM EXIT

# Actual bootstrap

## Defaults
macos_layer && {
    layer "system" "apply default settings" && {
        cd "$XDG_CONFIG_HOME/scripts" && {
            /bin/bash osx_defaults.sh
        }
    }
}

## Linking Git
macos_layer && {
    layer "system" "linking git settings" && {
        cd "$XDG_CONFIG_HOME/git" && {
            ln -sfv gitconfig $HOME/.gitconfig
            ln -sfv gitconfig $HOME/.gitignore_global
        }
    }
}

## Homewbrew
export PATH=/opt/homebrew/bin:$PATH
macos_layer && {
  layer "system" "ensure homebrew installation" && {
    if check brew; then
      echo "Found brew executable at $(which brew)"
      echo "Nothing to do"
    else
      section "install brew"
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    fi
  }
}

install_layer && {
  layer "system" "install all libs & apps" && {
    cd "$XDG_CONFIG_HOME/homebrew" && {
        brew bundle
    }
  }
}

## Gnupg
layer "system" "Fix gnupg" && {
  # make sure that I am the owner
  chown -R "$(whoami)" ~/.gnupg/
  # correct permissions
  find ~/.gnupg -type f -exec chmod 600 {} \;
  find ~/.gnupg -type d -exec chmod 700 {} \;
}


true
