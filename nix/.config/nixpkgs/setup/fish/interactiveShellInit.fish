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

    function k8senv
        kops version
        kubectl version --client --short
        helm version --client --short
    end
end


functions rvm >/dev/null 2>&1; and rvm default


set fish_greeting
