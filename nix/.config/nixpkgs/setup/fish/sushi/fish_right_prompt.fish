# ------------------------------------------------------------------- [ Colors ]

function orange
    set_color -o ee5819
end

function yellow
    set_color -o b58900
end

function red
    set_color -o d30102
end

function cyan
    set_color -o 2aa198
end

function white
    set_color -o fdf6e3
end

function dim
    set_color -o 4f4f4f
end

function off
    set_color -o normal
end


# ---------------------------------------------------------------------- [ Git ]

function git::is_repo
    test -d .git; or command git rev-parse --git-dir >/dev/null ^/dev/null
end

function git::branch_name
    git::is_repo; and begin
        command git symbolic-ref --short HEAD ^/dev/null;
        or command git show-ref --head -s --abbrev | head -n1 ^/dev/null
    end
end

function git::is_dirty
    git::is_repo; and not command git diff --no-ext-diff --quiet --exit-code
end

function git::is_staged
    git::is_repo; and begin
        not command git diff --cached --no-ext-diff --quiet --exit-code
    end
end

function git::is_stashed
    git::is_repo; and begin
        command git rev-parse --verify --quiet refs/stash >/dev/null
    end
end


# ---------------------------------------------------------------------- [ AWS ]

function aws::current_profile
    if set -q AWS_PROFILE
        printf "$AWS_PROFILE"
    else if set -q AWS_DEFAULT_PROFILE
        printf "$AWS_DEFAULT_PROFILE"
    end
end


# --------------------------------------------------------------- [ Kubernetes ]

function k8s::current_context
    command kubectl config current-context
end

function k8s::current_namespace
    command kubectl config view --minify -o jsonpath='{.contexts[0].context.namespace}'
end

function k8s::current_user
    command kubectl config view --minify -o jsonpath='{.users[0].name}' \
        | string replace "@sportradar.com" ""
end


# ------------------------------------------------------------- [ Right Prompt ]

function fish_right_prompt
    set -l cwd (basename (prompt_pwd))

    if git::is_repo
        set root_folder (command git rev-parse --show-toplevel ^/dev/null)
        set parent_root_folder (dirname $root_folder)
        set cwd (echo $PWD | sed -e "s|$parent_root_folder/||")
    end

    printf (yellow)"("(dim)$cwd(yellow)") "(off)

    set -l aws_profile (aws::current_profile)
    test -n "$aws_profile";
    and printf (dim)"{"(yellow)(aws::current_profile)(dim)"} "(off)

    # NOTE: Only on Darwin.
    # command ifconfig utun1 >/dev/null 2>&1;
    # and printf (dim)"|vpn| "(off)

    command -sq kubectl; and begin
        if [ "$USER" = (k8s::current_user) ]
            printf (dim)"["(off)
        else
            printf (red)"["(off)
        end

        printf (yellow)(k8s::current_context)(off)

        set -l k8s_namespace (k8s::current_namespace)
        test -n "$k8s_namespace";
        and printf (dim)"/"(yellow)"$k8s_namespace"(off)

        if [ "$USER" = (k8s::current_user) ]
            printf (dim)"] "(off)
        else
            printf (red)"] "(off)
        end
    end

    printf (dim)(date +%H(yellow):(dim)%M(yellow):(dim)%S)(off)
end
