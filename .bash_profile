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
alias gcm="git checkout master && git pull"
alias gpo="git push origin"
alias gb="git branch -vv"
alias gcb="git checkout -b"
alias gpom="git push origin master"
alias guc="git reset --soft HEAD^ && git reset HEAD ."
alias gwip="git commit -am 'WIP'"

alias jpp="python -m json.tool"

# docker workflow
alias dm=docker-machine
alias dc=docker-compose
alias dmq='eval "$(docker-machine env q)"'

start-docker () {
  docker-machine start q > /dev/null 2>&1;
  eval "$(docker-machine env q)";
}

start-docker;

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

# added by Anaconda2 2.4.1 installer
export PATH="~/anaconda/bin:$PATH"

# fancy autocompletion (for things like scp)
. /etc/bash_completion

# For use with https://github.com/stevemao/diff-so-fancy
git config --global core.pager "diff-so-fancy | less --tabs=1,5 -R"
git config --global color.diff-highlight.oldNormal "red bold"
git config --global color.diff-highlight.oldHighlight "red bold 52"
git config --global color.diff-highlight.newNormal "green bold"
git config --global color.diff-highlight.newHighlight "green bold 22"
