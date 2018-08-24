# -*- mode: fish; tab-width: 2 -*-

function latest -d 'Print the latest tag (on GitHub) for a given user and repo.'
  # TODO: __usage

  set --local num_args (count $argv)

  if test $num_args -eq 2
    set --local user $argv[1]
    set --local repo $argv[2]
    http https://api.github.com/repos/$user/$repo/tags | jq '.[0].name'
  end
end
