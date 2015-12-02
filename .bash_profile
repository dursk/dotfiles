export TERM="xterm-color"

alias ls="ls -G"
alias ll="ls -FGlAhp"
alias cp="cp -iv"
alias mv="mv -iv"
alias mkdir="mkdir -pv"
alias less="less -FSRXc"
alias ..="cd ../"
alias ...="cd ../../"
alias .3="cd ../../../"
alias .4="cd ../../../../"
alias .5="cd ../../../../../"
alias ~="cd ~"

# git aliases
alias ga="git add"
alias gs="git status"
alias gd="git diff"
alias gc="git commit"
alias gcl="git branch --merged | grep -v \* | xargs git branch -D "
alias gp="git pull"
alias gf="git fetch"
alias gl="git log"
alias gr="git rebase -i HEAD^^"
alias gr3="git rebase -i HEAD^^^"
alias gr4="git rebase -i HEAD^^^^"
alias gca="git checkout -- ."
alias gcf="git commit -am 'f'"
alias gcr="git commit -am 'f' && git rebase -i HEAD^^"
alias gcd="git checkout develop && git pull"
alias gpo="git push origin"
alias gb="git branch"
alias gcb="git checkout -b"

alias jpp="python -m json.tool"

# get current branch in git repo
function parse_git_branch() {
    BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
    if [ ! "${BRANCH}" == "" ]
    then
        STAT=`parse_git_dirty`
        echo "[${BRANCH}${STAT}]"
    else
        echo "--"
    fi
}

# get current status of git repo
function parse_git_dirty {
    status=`git status 2>&1 | tee`
    dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
    untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
    ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
    newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
    renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
    deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
    bits=''
    if [ "${renamed}" == "0" ]; then
        bits=">${bits}"
    fi
    if [ "${ahead}" == "0" ]; then
        bits="*${bits}"
    fi
    if [ "${newfile}" == "0" ]; then
        bits="+${bits}"
    fi
    if [ "${untracked}" == "0" ]; then
        bits="?${bits}"
    fi
    if [ "${deleted}" == "0" ]; then
        bits="x${bits}"
    fi
    if [ "${dirty}" == "0" ]; then
        bits="!${bits}"
    fi
    if [ ! "${bits}" == "" ]; then
        echo " ${bits}"
    else
        echo ""
    fi
}


export PS1="\[\e[33m\]\u\[\e[m\]@\[\e[32m\]\h \[\e[m\]on \[\e[1;31m\]\`parse_git_branch\`\n\[\e[m\]\w \\$ "

# Download git auto-completion script from:
# curl https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash -o ~/.git-completion.bash
if [ -f ~/.git-completion.bash ]; then
  . ~/.git-completion.bash
fi

alias emacs="/usr/local/Cellar/emacs/24.5/Emacs.app/Contents/MacOS/Emacs -nw"
EDITOR="/usr/local/Cellar/emacs/24.5/Emacs.app/Contents/MacOS/Emacs"

GOPATH=~/work
