. ~/.bash_alias

export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
export HISTCONTROL=ignoreboth
export EDITOR=vim
shopt -u histappend   # .bash_history追記モードは不要なのでOFFに
shopt -s checkwinsize

# alias
export EDITOR=vim
export GIT_EDITOR=vim
export IGNOREEOF=2 # 間違ってCtl+dしても3回は許してくれる
export FIGNORE=".bak:.o:~:.pyc" # 補完時に無視するsuffix
export HISTTIMEFORMAT='%y/%m/%d %H:%M:%S  ' 
export HISTCONTROL=ignoreboth
export HISTIGNORE=?:??:???:exit
export SCREENDIR=$HOME/.screendir
export HISTSIZE=10000000000000  # 履歴のMAX保存数を指定

# macports
export PATH=~/bin:/usr/local/bin:/usr/local/sbin:$PATH:~/bin/AWS-ElasticBeanstalk-CLI-2.0/eb/macosx/python2.7
PATH="$(brew --prefix coreutils)/libexec/gnubin:$PATH"
# AWS ElasticBeanstalk
export ELASTICBEANSTALK_URL=elasticbeanstalk.ap-northeast-1.amazonaws.com
export AWS_CREDENTIAL_FILE=~/.nobjas_amazon_credencial

# save history of screen
function share_history {  # 以下の内容を関数として定義
    history -a  # .bash_historyに前回コマンドを1行追記
    history -c  # 端末ローカルの履歴を一旦消去
    history -r  # .bash_historyから履歴を読み込み直す
}
export PROMPT_COMMAND='share_history'  # 上記関数をプロンプト毎に自動実施


# Homebrew bash-completion
if [ -f `brew --prefix`/etc/bash_completion ]; then
  . `brew --prefix`/etc/bash_completion
fi
NAME='HOME'
if [ -f $BASH_COMPLETION_DIR/git-completion.bash ]; then
    PS1='\[\e[0;37m\]${NAME}\[\e[0;37m\][\t]\[\e[0;37m\]: \[\e[1;37m\]\w\n\[\e[1;33m\]h:\! j:\j\[\e[0;36m\]$(__git_ps1) \[\e[0;34m\]\$\[\e[m\] '
else
    PS1='\[\e[0;37m\]${NAME}\[\e[0;37m\][\t]\[\e[0;37m\]: \[\e[1;37m\]\w\n\[\e[1;33m\]h:\! j:\j \[\e[0;34m\]\$\[\e[m\] '
fi

# keychain
unset SSH_AUTH_SOCK
HOST_NAME="nobjas-pc"
keychain ~/.ssh/id_rsa ~/.ssh/id_dsa --host $HOST_NAME
if [ -f ~/.keychain/$HOST_NAME-sh ]; then
    source ~/.keychain/$HOST_NAME-sh
fi

if [ "`echo $BASH_VERSION | cut -b 1`" -eq "4" ]; then
    # version 4
    shopt -s dirspell # 入力補完時にディレクトリ名のスペルミスを修正するdirspell
    shopt -s autocd   # ディレクトリ名を入力するだけでカレントディレクトリを変更できるautocd
    shopt -s globstar # サブディレクトリを再帰的にファイル検索するglobstar
    shopt -s cdspell  # cdするときディレクトリ名をよしなに修正する。
    export PROMPT_DIRTRIM=3 # PS1が短くなる
fi

VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
