source ~/.aliases
source ~/.env_vars

neofetch --ascii ~/dotfiles/aperture_ascii.txt

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/zsh_completion" ] && \. "$NVM_DIR/zsh_completion"  # This loads nvm bash_completion

eval "$(starship init zsh)"
# eval "$(direnv hook zsh)"


#  . /usr/local/opt/asdf/libexec/asdf.sh ## Use asdf package manager
# export PATH="/usr/local/sbin:$PATH"

export PATH="/Users/alex/.mix/escripts:$PATH" 

fortune -s

export PATH="/usr/local/sbin:$PATH"
export PATH="/Library/Frameworks/Python.framework/Versions/3.7/bin:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
