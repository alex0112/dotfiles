source ~/.aliases
source ~/.env_vars

neofetch # --ascii ~/dotfiles/aperture_ascii.txt

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


## Evals for various things
eval "$(starship init bash)"
# eval "$(direnv hook bash)"
eval "$(zoxide init bash)"

#  . /usr/local/opt/asdf/libexec/asdf.sh ## Use asdf package manager
# export PATH="/usr/local/sbin:$PATH"

#export PATH="/Users/alex/.mix/escripts:$PATH" 

fortune -s

export PATH="/usr/local/sbin:$PATH"
export PATH="/sbin:$PATH"
export PATH="$PATH:/usr/local/go/bin"
export PATH="$PATH:/home/kingsfoil/go/bin"
# export PATH="/Library/Frameworks/Python.framework/Versions/3.7/bin:$PATH"
# export PYENV_ROOT="$HOME/.pyenv"
# command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
#eval "$(pyenv init -)"

# Created by `pipx` on 2023-09-22 22:27:19
export PATH="$PATH:/home/kingsfoil/.local/bin"

export PATH="$PATH:/home/kingsfoil/sources/bin"

export PATH="/run/user/1000/fnm_multishells/618164_1700005610556/bin":$PATH
export FNM_LOGLEVEL="info"
export FNM_VERSION_FILE_STRATEGY="local"
export FNM_RESOLVE_ENGINES="false"
export FNM_DIR="/home/kingsfoil/.local/share/fnm"
export FNM_COREPACK_ENABLED="false"
export FNM_MULTISHELL_PATH="/run/user/1000/fnm_multishells/618164_1700005610556"
export FNM_ARCH="x64"
export FNM_NODE_DIST_MIRROR="https://nodejs.org/dist"

export HELIX_RUNTIME=~/sources/helix/runtime

. "$HOME/.asdf/asdf.sh"
. "$HOME/.asdf/completions/asdf.bash"
