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


function latest -d 'Print the latest tag (on GitHub) for a given user and repo.'
  # TODO: __usage

  set --local num_args (count $argv)

  if test $num_args -eq 2
    set --local user $argv[1]
    set --local repo $argv[2]
    http https://api.github.com/repos/$user/$repo/tags | jq '.[0].name'
  end
end


command -sq fluidsynth; and function playmidi
    fluidsynth -i ~/lib/arachno-soundfont/Arachno\ SoundFont\ -\ Version\ 1.0.sf2 $argv
end


command -sq kitty; and function icat
    kitty +kitten icat
end


command -sq kubectl; and function kcexec
    argparse -N 2 --name=kcexec 'r/replica=' -- $argv
    or return

    if not set -q _flag_replica
        set -l _flag_replica 0
    end

    kubectl exec -it (kubectl get pod -o jsonpath="{.items[$_flag_replica].metadata.name}" -l app.kubernetes.io/name=$argv[1]) -c $argv[1] $argv[2..-1]
end


command -sq kubectl; and begin
    function kcnodepods -d 'List all pods on a given node'
        argparse --name=kcnodepods \
        -N 1 -X 1 \
        'n/namespace=?' \
        'w/watch' \
        -- $argv
        or return

        set -l kubectl_flags

        if set -q _flag_namespace
            set kubectl_flags $kubectl_flags --namespace=$_flag_namespace
        else
            set kubectl_flags $kubectl_flags --all-namespaces
        end

        if set -q _flag_watch
            set kubectl_flags $kubectl_flags --watch
        end

        set kubectl_flags $kubectl_flags --field-selector=spec.nodeName=$argv[1]

        eval kubectl get pods $kubectl_flags
    end

    function cpo -d 'Get the name of the Cilium pod running on a given node'
        command kubectl get pods \
        --field-selector spec.nodeName=$argv[1] \
        --namespace kube-system \
        --output jsonpath='{.items[0].metadata.name}' \
        --selector k8s-app=cilium
    end

    function cpods -d 'Get the name of every Cilium pod'
        command kubectl get pods \
        --namespace kube-system \
        --output jsonpath='{range .items[*]}{@.metadata.name}{"\n"}' \
        --selector k8s-app=cilium
    end

    # TODO: Add option to print server versions too.
    function k8s::versions
        printf "kubectl %s\n" (command kubectl version --client --short)
        printf "helm %s\n" (command helm version --client --short)
        command helmfile --version
        printf "kops %s\n" (command kops version)
    end
end


# FIXME: functions rvm >/dev/null 2>&1; and rvm default


if string match -r '.*k8s-\d+$' "$buildInputs"
    set fish_greeting (k8senv)
else
    set fish_greeting
end


command -sq task; and command -sq jq; and function tj \
  -d 'Open the Jira ticket associated with a Taskwarrior task'
  test (count $argv) -ne 1; and return
  open (task $argv[1] export | jq -r '.[0].jiraurl')
end
