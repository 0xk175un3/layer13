# Ask for the administrator password upfront
sudo -v

# goodbye gatekeeper
sudo spctl --master-disable

###############################################################################
# General UI/UX                                                               #
###############################################################################
sudo scutil --set ComputerName ArtBook
sudo scutil --set HostName artbook
sudo scutil --set LocalHostName ArtBook
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string Artbook

###############################################################################
# Safari & WebKit                                                             #
###############################################################################
# Show the full URL in the address bar (note: this still hides the scheme)
defaults write com.apple.SafariTechnologyPreview ShowFullURLInSmartSearchField -bool true

# Prevent Safari from opening ‘safe’ files automatically after downloading
defaults write com.apple.SafariTechnologyPreview AutoOpenSafeDownloads -bool false

# Save screenshots into ~/Screenshots/ instead of desktop
mkdir -p ~/Screenshots/
sudo defaults write com.apple.screencapture location ~/Screenshots/
defaults write com.apple.screencapture location ~/Screenshots/

# Disable the “Are you sure you want to open this application?” dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false

# Disable press-and-hold for keys in favor of key repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Set a blazingly fast keyboard repeat rate
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 20

# Set the timezone; see `sudo systemsetup -listtimezones` for other values
systemsetup -settimezone "Europe/Helsinki" 2>/dev/null 1>&2

# set first day of the week to 'Monday', like normal people
defaults write -globalDomain AppleLocale -string "en_JP@calendar=iso8601";

# Finder: show hidden files by default
defaults write com.apple.finder AppleShowAllFiles -bool true

# Finder: show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Finder: show status bar
defaults write com.apple.finder ShowStatusBar -bool true

# Finder: show path bar
defaults write com.apple.finder ShowPathbar -bool true

# Display full POSIX path as Finder window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# Keep folders on top when sorting by name
defaults write com.apple.finder _FXSortFoldersFirst -bool true

# When performing a search, search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Enable spring loading for directories
defaults write NSGlobalDomain com.apple.springing.enabled -bool true

# Remove the spring loading delay for directories
defaults write NSGlobalDomain com.apple.springing.delay -float 0.2

# Avoid creating .DS_Store files on network or USB volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# autohide speed
defaults write com.apple.dock autohide-time-modifier -int 0;killall Dock

# never re-open apps
sudo chown root ~/Library/Preferences/ByHost/com.apple.loginwindow*
sudo chmod 000 ~/Library/Preferences/ByHost/com.apple.loginwindow*
defaults write com.apple.loginwindow TALLogoutSavesState -bool false
