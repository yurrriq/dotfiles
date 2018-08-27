function pcat
    pygmentize -f terminal -g $argv
end


function hicat -d 'Hackish hicat clone via pygments'
    pcat $argv | less -cR
end


function kcexec
    argparse -N 2 --name=kcexec 'r/replica=' -- $argv
    or return

    if not set -q _flag_replica
        set -l _flag_replica 0
    end

    kubectl exec -it (kubectl get pod -o jsonpath="{.items[$_flag_replica].metadata.name}" -l app=$argv[1]) -c $argv[1] $argv[2..-1]
end


rvm default
