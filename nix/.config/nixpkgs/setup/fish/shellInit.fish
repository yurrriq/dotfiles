for p in /run/current-system/sw/bin ~/bin
    if not contains $p $fish_user_paths
        set -g fish_user_paths $p $fish_user_paths
    end
end


function fish_title
  echo "$PWD | $_" | sed "s|$HOME|~|g"
end