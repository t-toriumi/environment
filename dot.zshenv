path=(
  ~/bin ~/local/bin bin ~/usr/local/bin /bin /sbin /usr/local/bin /usr/bin /usr/sbin /opt/local/bin /opt/local/sbin
)

# VISUAL
if [ -x "`which vim`" ]; then
    export VISUAL=vim
else
    export VISUAL=vi
fi

# EDITOR
if [ -x "`which emacs`" ]; then
    export EDITOR="emacs -nw"
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

#alias ll='gls -alF'
#alias ls='gls -F'
alias ps='ps auxw'
alias tc='tar cvzf'
alias tx='tar xvzf'

# grep
alias -g gr='grep --color -n '
alias -g xg='|xargs grep --color -n '

# peco
alias sshp='_sshp'
alias jump='_jump'
alias look='less $(find . -type f -maxdepth 1 | peco)'

# ssh & peco
function _sshp(){
  __path=$(grep -w Host ~/.ssh/config | awk '{print $2}' | sort -r | peco --prompt "SSH to > ")
  if [ -n "$__path" ]; then
    ssh $__path
  fi
}

# ag & view
function _jump(){
  __path=$(ag $* | peco | awk -F: '{printf  $1 " +" $2}'| sed -e 's/\+$//')
  if [ -n "$__path" ]; then
    view $__path
  fi
}

# docker & peco
function _sshd(){
  __path=$(docker ps --format "{{.ID}}\t{{.Names}}" | peco --prompt "into > " | awk '{print$1}')
  if [ -n "$__path" ]; then
    docker exec -it $__path /bin/sh
  fi
}
function _logd(){
  __path=$(docker-compose config --service | peco --prompt "tail for >")
  if [ -n "$__path" ]; then
    docker-compose logs -f $__path
  fi
}

alias sshd='_sshd'
alias logd='_logd'

# git & peco.
function _git-hash(){
  git log --oneline --branches | peco | awk '{print $1}'
}
function _git-changed-files(){
  git status --short | peco | awk '{print $2}'
}

alias -g gith='$(_git-hash)'
alias -g gitc='$(_git-changed-files)'

# git diff view
function __vimd() {
  vimdiff $1 <(git show remotes/origin/test:$1)
}
alias vimd=__vimd

function __git_unmerges() {
  git l  $1..$(gbn | sed -e 's/\://')
}
alias git_unmerges=__git_unmerges

# git access token
if [ -e ~/.brew_api_token ]; then
    source ~/.brew_api_token
fi

# local setting
if [ -e ~/.zshlocal ]; then
    source ~/.zshlocal
fi
