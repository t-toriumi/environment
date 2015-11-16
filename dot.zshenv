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

alias ll='ls -alF'
alias ls='ls -F'
alias ps='ps auxw'
alias tc='tar cvzf'
alias tx='tar xvzf'

# grep
alias -g gr='grep --color -n '
alias -g xg='|xargs grep --color -n '

# peco
alias jump='_jump'
alias look='less $(find . -type f -maxdepth 1 | peco)'

# ag & view
function _jump(){
  __path=$(ag $* | peco | awk -F: '{printf  $1 " +" $2}'| sed -e 's/\+$//')
  if [ -n "$__path" ]; then
    view $__path
  fi
}

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

# git access token
if [ -e ~/.brew_api_token ]; then
    source ~/.brew_api_token
fi

# local setting
if [ -e ~/.zshlocal ]; then
    source ~/.zshlocal
fi
