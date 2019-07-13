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
        argparse -N 1 -X 1 --name=kcnodepods 'n/namespace=?' -- $argv
        or return

        if not set -q _flag_namespace
            kubectl get pods --all-namespaces --field-selector=spec.nodeName=$argv[1]
        else
            kubectl get pods --namespace=$_flag_namespace --field-selector=spec.nodeName=$argv[1]
        end
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


function vpn-restart
    sudo launchctl stop me.ericb.openconnect
    and sudo launchctl start me.ericb.openconnect
end


# FIXME: functions rvm >/dev/null 2>&1; and rvm default


if string match -r '.*k8s-\d+$' "$buildInputs"
    set fish_greeting (k8senv)
else
    set fish_greeting
end
