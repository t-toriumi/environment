# UTF-8
export LANG=ja_JP.UTF-8

# COREを作らない
ulimit -c 0

# Emacs ライクな操作を有効にする（文字入力中に Ctrl-F,B でカーソル移動など）
bindkey -e

# autoloadされる関数を検索するパス
fpath=(~/.zfunc $fpath)
fpath=(~/.git_completion $fpath)
fpath=(~/.zsh.d/anyframe(N-/) $fpath)

# zsh plugin
autoload -Uz anyframe-init
anyframe-init

# 履歴の保存先と保存数
HISTFILE=$HOME/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

# 色の設定
local DEFAULT=$'%{^[[m%}'$
local RED=$'%{^[[1;31m%}'$
local GREEN=$'%{^[[1;32m%}'$
local YELLOW=$'%{^[[1;33m%}'$
local BLUE=$'%{^[[1;34m%}'$
local PURPLE=$'%{^[[1;35m%}'$
local LIGHT_BLUE=$'%{^[[1;36m%}'$
local WHITE=$'%{^[[1;37m%}'$

export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

if [ -f ~/.dircolors ]; then
    if type dircolors > /dev/null 2>&1; then
        eval $(dircolors ~/.dircolors)
    elif type gdircolors > /dev/null 2>&1; then
        eval $(gdircolors ~/.dircolors)
    fi
fi

# 自動補完を有効にする
# コマンドの引数やパス名を途中まで入力して <Tab> を押すといい感じに補完してくれる
autoload -U compinit; compinit -u

# 入力したコマンド名が間違っている場合には修正
setopt correct
setopt correct_all
# 入力したコマンドが存在せず、かつディレクトリ名と一致するなら、ディレクトリに cd する
setopt auto_cd
# cd した先のディレクトリをディレクトリスタックに追加する
# ディレクトリスタックとは今までに行ったディレクトリの履歴のこと
setopt auto_pushd
# pushd したとき、ディレクトリがすでにスタックに含まれていればスタックに追加しない
setopt pushd_ignore_dups
# 入力したコマンドがすでにコマンド履歴に含まれる場合、履歴から古いほうのコマンドを削除する
# コマンド履歴とは今まで入力したコマンドの一覧のことで、上下キーでたどれる
setopt hist_ignore_all_dups
# コマンドがスペースで始まる場合、コマンド履歴に追加しない
setopt hist_ignore_space
# 補完キー連打で順に補完候補を自動で補完
setopt auto_menu
# 拡張グロブで補完(~とか^とか。例えばless *.txt~memo.txt ならmemo.txt 以外の *.txt にマッチ)
setopt extended_glob
# Beepを鳴らさない
setopt no_beep
setopt no_list_beep
# 補完候補を詰めて表示
setopt list_packed
# このオプションが有効な状態で、ある変数に絶対パスのディレクトリを設定すると、即座にその変数の名前が
# ディレクトリの名前になり、すぐにプロンプトに設定している'%~'やcdコマンドの'~'での補完に反映されるようになる。
setopt auto_name_dirs
# 補完実行時にスラッシュが末尾に付いたとき、必要に応じてスラッシュを除去する
setopt auto_remove_slash
# 行の末尾がバッククォートでも無視する
setopt sun_keyboard_hack
# 補完候補一覧でファイルの種別を識別マーク表示(ls -F の記号)
setopt list_types
# 補完のときプロンプトの位置を変えない
setopt always_last_prompt
# 変数の単語分割を行う
setopt sh_word_split
# 履歴にタイムスタンプを追加する
setopt extended_history
# Ctrl-s, Ctrl-q によるフロー制御を無効にする
setopt no_flow_control
# 履歴を共有する
setopt share_history
# バックグラウンドジョブが終了したら(プロンプトの表示を待たずに)すぐに知らせる
setopt notify

# <Tab> でパス名の補完候補を表示したあと、
# 続けて <Tab> を押すと候補からパス名を選択できるようになる
# 候補を選ぶには <Tab> か Ctrl-N,B,F,P
zstyle ':completion:*:default' menu select=1
# キャッシュを利用する
zstyle ':completion:*' use-cache true
# 補完から除外するファイル
zstyle ':completion:*:*files' ignored-patterns '*?.o' '*?~' '*\#'
# ファイル補完候補に色を付ける
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# cd は親ディレクトリからカレントディレクトリを選択しないので表示させないようにする
zstyle ':completion:*:cd:*' ignore-parents parent pwd
# 大文字小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# 履歴表示
#function __history() {
#  history -E 1
#}
#alias his=__history
alias his=anyframe-widget-execute-history

# ディレクトリスタックに直接移動する
#function __dirs() {
#  if [ -z $1 ]; then
#    dirs -v | perl -pe 's/\t/: /g'
#  else
#    dirs -v | perl -pe 's/\t/: /g' | grep $1
#  fi
#  echo -n "select number: "
#  read newdir
#  [ expr $newdir + 0 > /dev/null 2>&1 ]
#  cd +"$newdir"
#}
#alias cdd=__dirs
alias cdd=anyframe-widget-cdr

# select history
function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | \
        eval $tac | \
        peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-select-history
bindkey '^r' peco-select-history

# cdr
autoload -Uz is-at-least
if is-at-least 4.3.11
then
  autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
  add-zsh-hook chpwd chpwd_recent_dirs
  zstyle ':chpwd:*'      recent-dirs-max 1000
  zstyle ':chpwd:*'      recent-dirs-default yes
  zstyle ':completion:*' recent-dirs-insert both
fi

# ブランチ名を表示する
function __git_branch() {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\:\1/'
}
alias gbn=__git_branch

# Emacsはbrew版をターミナルで利用する
alias emacs='/usr/local/Cellar/emacs-plus@27/27.2/bin/emacs -nw'
alias gtags='/usr/local/Cellar/global/6.6.4_1/bin/gtags'
#alias screen='/usr/local/Cellar/screen/4.6.2/bin/screen -U'

# 色の設定
if [ `uname` = "FreeBSD" ]; then
  alias ls='ls -GF'
  alias ll='ls -alGF'
elif [ `uname` = "Darwin" ]; then
  alias ls='gls -F --color'
  alias ll='gls -alF --color'
else
  alias ls='gls -F --color'
  alias ll='gls -alF --color'
fi

alias more='more -R'
alias less='less -R'
alias ag='ag -i --hidden --pager "less -R"' 

# プロンプト
setopt prompt_subst
if [ `whoami` = "root" ]; then
  PROMPT='[%F{red}%n@%m%F{default}]# '
else
  PROMPT='[%m$(gbn)]$ '
fi
RPROMPT='[%F{green}%~%f%F{default}]'

# スクリーン起動
if [ "$WINDOW" = "" ] ; then
  screen
fi

function preexec() {
  if [ $TERM_PROGRAM = 'iTerm.app' ]; then
    mycmd=(${(s: :)${1}})
    echo -ne "\ek$(hostname|awk 'BEGIN{FS="."}{print $1}'):$mycmd[1]\e\\"
  fi
}
function precmd() {
  if [ $TERM_PROGRAM = 'iTerm.app' ]; then
    echo -ne "\ek$(hostname|awk 'BEGIN{FS="."}{print $1}'):idle\e\\"
  fi
}
