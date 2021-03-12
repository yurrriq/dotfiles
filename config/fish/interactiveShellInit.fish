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
function latest -d 'Print the latest release (on GitHub) for a given user and repo.'
  # TODO: __usage

  set --local num_args (count $argv)

  if test $num_args -eq 2
    set --local user $argv[1]
    set --local repo $argv[2]
    http https://api.github.com/repos/$user/$repo/releases/latest | jq -r '.tag_name'
  end
end
command -sq aws; and command -sq jq; and \
function describe-cert -d 'List the domains for a given ACM certificate'
    test (count $argv) -ne 1; and return
    aws acm describe-certificate --certificate-arn $argv[1] |
    jq -r '.Certificate | .SubjectAlternativeNames[]'
end
command -sq fluidsynth; and function playmidi
    fluidsynth -i ~/lib/arachno-soundfont/Arachno\ SoundFont\ -\ Version\ 1.0.sf2 $argv
end
command -sq kitty; and function icat
    kitty +kitten icat $argv
end
command -sq kubectl; and begin
    # TODO: Add option to print server versions too.
    function k8s::versions
        printf "kubectl %s\n" (command kubectl version --client --short)
        printf "helm %s\n" (command helm version --client --short)
        command helmfile --version
        printf "kops %s\n" (command kops version)
    end

    function kcterm -d 'Terminate a Kubernetes node'
        test (count $argv) -ne 1; and return
        kubectl get node -o jsonpath='{.spec.providerID}' $argv[1] |
        cut -d'/' -f5 |
        xargs aws ec2 terminate-instances --instance-ids
    end
end
# FIXME: functions rvm >/dev/null 2>&1; and rvm default
set fish_greeting
command -sq task; and command -sq jq; and function tj \
  -d 'Open the Jira ticket associated with a Taskwarrior task'
  test (count $argv) -ne 1; and return
  open (task $argv[1] export | jq -r '.[0].jiraurl')
end
