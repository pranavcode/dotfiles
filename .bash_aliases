# Bash Aliases

#-----------------------------
# General
#-----------------------------
alias ll='ls -alF --color'
alias la='ls -A --color'
alias l="ls -ltrahFG --color"
alias grep="grep --color"
alias mv="mv -i"
alias cp="cp -i"
alias rm="rm -i"
alias emx="emacs -nw"
alias screenoff="xset dpms force off"
alias poweroff="sudo shutdown -h now"

#-----------------------------
# Git
#-----------------------------
alias gpr="git pull --rebase"
alias gpush="git push"
alias gst="git status -sbu"
alias gd="git diff -U0"
alias gsh="git show"
alias gco="git checkout"
alias gcm="git commit -m"
alias ga="git add"
alias gr="git reset"

#-----------------------------
# Rails
#-----------------------------
alias be="bundle exec"
alias rs="be rails s"
alias rc="be rails c"

#-----------------------------
# Docker
#-----------------------------
alias dps="docker ps"
alias dimgs="docker images"
alias drun="docker run -t"
alias druni="docker run -t -i"
alias mine="./RubyMine7/bin/rubymine.sh"

#-----------------------------
# Kubernetes
#-----------------------------
alias k="kubectl"
alias kgp="kubectl get pods"
alias kgs="kubectl get services"
alias kgd="kubectl get deployments"
alias kgst="kubectl get secrets"
alias kcf="kubectl create -f"
alias krf="kubectl replace -f"
alias kru="kubectl rolling-update"
