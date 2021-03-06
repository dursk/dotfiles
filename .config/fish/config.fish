set -g -x PATH /usr/local/bin $PATH
set -g -x fish_greeting ''

function ls
    command ls -G $argv
end
function ll
    command ls -FGlAhp $argv
end
function cp
    command cp -iv $argv
end
function mv
    command mv -iv $argv
end
function mkdir
    command mkdir -pv $argv
end
function less
    command less -FSRXc $argv
end
function ..
    cd ../
end
function ...
    cd ../../
end
function .3
    cd ../../../
end
function .4
    cd ../../../../
end
function .5
    cd ../../../../../
end
function g
    egrep $argv
end

# git aliases
function ga
    git add $argv
end
function gaa
    git add .
end
function gs
    git status $argv
end
function gd
    git diff $argv
end
function gc
    git commit $argv
end
function gco
    git checkout $argv
end
function gcam
    git commit -am $argv
end
function gpm
    git branch | grep -v "master" | xargs git branch -D
end
function gpd
    git branch | grep -v "develop" | xargs git branch -D
end
function gp
    git pull $argv
end
function gf
    git fetch $argv
end
function gl
    git log $argv
end
function gr
    git rebase -i HEAD^^ $argv
end
function gr3
    git rebase -i HEAD^^^ $argv
end
function gr4
    git rebase -i HEAD^^^^ $argv
end
function gca
    git checkout -- . $argv
end
function gcf
    git commit -am 'f' $argv
end
function gcr
    git commit -am 'fixup!'
    and git rebase -i --autosquash HEAD^^
end
function gcd
    git checkout develop
    and git pull
end
function gcm
    git checkout master
    and git pull
end
function gpo
    git push origin $argv
end
function gb
    git branch -vv $argv
end
function gcb
    git checkout -b $argv
end
function gpom
    git push origin master
end
function guc
    git reset --soft HEAD^
    and git reset HEAD .
end
function gwip
    git commit -am 'WIP'
end
function grm
    git rebase master
end

function jpp
    python -m json.tool $argv
end

eval (python -m virtualfish)

if set -q VIRTUAL_ENV
    echo -n -s (set_color -b blue white) "(" (basename "$VIRTUAL_ENV") ")" (set_color normal) " "
end

function dc
    docker-compose $argv
end

# status --is-interactive; and source (pyenv init -| psub)
# status --is-interactive; and source (pyenv virtualenv-init - | psub)


set -g fish_user_paths "/usr/local/opt/qt/bin" $fish_user_paths

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/matt/google-cloud-sdk/path.fish.inc' ]; . '/Users/matt/google-cloud-sdk/path.fish.inc'; end
set -g fish_user_paths "/usr/local/opt/node@10/bin" $fish_user_paths
