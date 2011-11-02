export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
export HISTCONTROL=ignoreboth
export EDITOR=vim
shopt -s histappend
shopt -s checkwinsize

# alias
export EDITOR=/Applications/MacVim.app/Contents/MacOS/Vim
alias vi='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/MacOS/Vim "$@"'
alias vim='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/MacOS/Vim "$@"'
alias ls='ls -vG'
alias la='ls -a'
alias ll='ls -l'
#alias e='/Applications/Emacs.app/Contents/MacOS/Emacs'
#alias c='/Applications/CotEditor.app/Contents/MacOS/CotEditor'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias j='jobs'
#alias screen='screen -xRU -D -RR'

# export 
# export LANG=ja_JP.UTF-8

# macports
#export PATH=~/bin:/opt/local/bin:/opt/local/sbin:$PATH
#export MANPATH=/opt/local/share/man:/opt/local/man:$MANPATH
export PATH=$PATH:/Users/nobjas/bin:/Applications/android-sdk-mac_86/tools
export PYTHONPATH=/opt/local/lib/python2.5/site-packages

# save screen
export SCREENDIR=$HOME/.screendir
function share_history {  # 以下の内容を関数として定義
    history -a  # .bash_historyに前回コマンドを1行追記
    history -c  # 端末ローカルの履歴を一旦消去
    history -r  # .bash_historyから履歴を読み込み直す
}
PROMPT_COMMAND='share_history'  # 上記関数をプロンプト毎に自動実施
shopt -u histappend   # .bash_history追記モードは不要なのでOFFに
export HISTSIZE=10000000000000  # 履歴のMAX保存数を指定


# Homebrew bash-completion
if [ -f /usr/local/etc/bash_completion ]; then
    . /usr/local/etc/bash_completion
fi
if [ -f $BASH_COMPLETION_DIR/git-completion.bash ]; then
    PS1='\[\e[1;32m\]NOBJAS:\[\e[m\] \[\e[1;37m\]\w\[\e[m\]\n\[\e[1;33m\]h:\! \[\e[0;33m\] \[\e[1;31m\]j:\j$(__git_ps1) \$\[\e[m\] '
else
    PS1='\[\e[1;32m\]NOBJAS:\[\e[m\] \[\e[1;37m\]\w\[\e[m\]\n\[\e[1;33m\]h:\! \[\e[0;33m\] \[\e[1;31m\]j:\j \$\[\e[m\] '
fi

# keychain
unset SSH_AUTH_SOCK
HOST_NAME="nobjas-pc"
keychain ~/.ssh/id_rsa ~/.ssh/id_dsa --host $HOST_NAME
if [ -f ~/.keychain/$HOST_NAME-sh ]; then
    source ~/.keychain/$HOST_NAME-sh
fi
#export PERL5LIB=/Users/nobjas/perl5/lib/perl5:$PERL5LIB
#source /Users/nobjas/perl5/perlbrew/etc/bashrc


## for git 
alias g='git'
alias gst='git status'
alias gl='git pull'
alias gup='git fetch && git rebase'
alias gp='git push'
function gdv {
    git-diff -w "$@" | view -
}
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gco='git checkout'
alias gb='git branch'
alias gba='git branch -a'
alias gcount='git shortlog -sn'
alias gcp='git cherry-pick'
alias glg='git log --stat --max-count=5'
alias glgg='git log --graph --max-count=5'
alias gss='git status -s'
alias ga='git add'
alias gm='git merge'
alias gsr='git svn rebase'
alias gsd='git svn dcommit'


#
# Will return the current branch name
# Usage example: git pull origin $(current_branch)
#
function current_branch {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

# these aliases take advantage of the previous function
alias ggpull='git pull origin $(current_branch)'
alias ggpush='git push origin $(current_branch)'
alias ggpnp='git pull origin $(current_branch) && git push origin $(current_branch)'
