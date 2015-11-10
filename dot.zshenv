path=(
  ~/bin ~/local/bin bin ~/usr/local/bin /bin /sbin /usr/local/bin /usr/bin /usr/sbin /opt/local/bin /opt/local/sbin
)

# EDITOR
if [ -x "`which emacs`" ]; then
    export EDITOR=emacs
elif [ -x "`which vim`" ]; then
    export EDITOR=vim
fi

# PAGER
if which lv >& /dev/null ; then
  export PAGER="lv"
elif which less >& /dev/null ;  then
  export PAGER="less -RM --quiet -x2"
else
  export PAGER="more -x2"
fi

export HOSTNAME=`hostname`
export LC_COLLATE="ja_JP.UTF-8"
export WORDCHARS="*?_-.[]~=&!#$%^(){}<>"

# git access token
source ~/.brew_api_token

alias ll='ls -alF'
alias ls='ls -F'
alias ps='ps auxw'
alias tc='tar cvzf'
alias tx='tar xvzf'

# git & peco.
function git-hash(){
  git log --oneline --branches | peco | awk '{print $1}'
}
function git-changed-files(){
  git status --short | peco | awk '{print $2}'
}

alias -g gh='$(git-hash)'
alias -g gc='$(git-changed-files)'
