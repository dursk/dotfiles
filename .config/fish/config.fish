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

# git aliases
function ga
    git add $argv
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
function gcl
    git branch --merged | grep -v \* | xargs git branch -D
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
    git commit -am 'f'
    and git rebase -i HEAD^^
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

function jpp
    python -m json.tool $argv
end


function start-docker
    docker-machine start q > /dev/null 2>& 1
    eval (docker-machine env q)
end

start-docker

eval (python -m virtualfish)

if set -q VIRTUAL_ENV
    echo -n -s (set_color -b blue white) "(" (basename "$VIRTUAL_ENV") ")" (set_color normal) " "
end