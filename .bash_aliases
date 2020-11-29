# Bash Aliases

#-----------------------------
# General
#-----------------------------
alias ll='ls -alF -color'
alias la='ls -A -color'
alias l="ls -ltrahFG -color"
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

#--------------------------
# Kubernetes
#--------------------------
# courtesy mr. oriol fito
# use minikube kubectl context - gcloud pointing to replaypoker
kum() {
    gcloud config configurations activate default
    kubectl config use-context minikube
    echo "* Running pods:"
    kubectl get pods
}
# use minikube docker daemon
dum() {
    eval $(minikube docker-env)
}
# use replaypoker staging cluster
kurd() {
    gcloud config configurations activate default
    kubectl config use-context gke_replay-gaming_us-central1-c_staging
}
# use replaypoker prod cluster
kurp() {
    gcloud config configurations activate default
    kubectl config use-context gke_replay-gaming_us-central1-c_production
}

alias kgp='kubectl get pods'
alias kgps='kubectl get pods -o wide --show-all'
alias kgs='kubectl get svc'
alias kgd='kubectl get deployment'
alias kl='kubectl logs'
alias kgc='kubectl config get-contexts'
alias kuc='kubectl config use-context'
alias kgn='kubectl get namespaces'
