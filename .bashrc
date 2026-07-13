source ~/.aliases
source ~/.env_vars

~/.config/neofetch/bmosay.sh -q -r ~/.config/neofetch/bmofetch.quotes && neofetch --config ~/.config/neofetch/bmofetch.conf --source ~/.config/neofetch/bmo.txt
#neofetch --config ~/.config/neofetch/bmofetch.conf --source ~/.config/neofetch/bmo.txt

echo "DAILY FORTUNE:  "
fortune -s

printf "\nmullvad status: $(mullvad status)\n\n"
#mullvad status; ## State the current connection status of mullvad

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
. "$HOME/.cargo/env"

# Set up fzf key bindings and fuzzy completion
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Use vivid to set LSCOLORs so we can read the output from lsd
export LS_COLORS="$(vivid generate ayu)"

# Get go binaries in the path
export PATH=$PATH:/usr/local/go/bin

# Keep the history around
shopt -s histappend

export EDITOR="$(which emacs) -nw"

# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

case ":$PATH:" in
    *:/home/kingsfoil/.juliaup/bin:*)
        ;;

    *)
        export PATH=/home/kingsfoil/.juliaup/bin${PATH:+:${PATH}}
        ;;
esac

# <<< juliaup initialize <<<
export PATH="/home/kingsfoil/.local/bin:$PATH"



