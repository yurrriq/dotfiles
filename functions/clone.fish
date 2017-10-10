# -*- mode: fish; tab-width: 2; -*-

#################### INSTALL #####################
# ln -s (pwd)/clone.fish ~/.config/fish/functions/
##################################################

function clone
  function __update
    test -d $argv[1]; and cd $argv[1]; and git fetch --all; and git pull
  end

  function __usage
    echo "Usage: clone [username] [repository] [[destination]]"
  end

  set --local num_args (count $argv)

  if test $num_args -ge 2
    set --local user $argv[1]
    set --local repo $argv[2]

    if test $num_args -eq 2
      set dest ~/src/$user/$repo
    else if test $num_args -eq 3
      set dest $argv[3]/$user/$repo
    else
      __usage
    end

    echo $dest

    git clone git@github.com:$user/$repo.git $dest; or __update $dest; or __usage
  else
    __usage
  end
end
