#!/usr/bin/env bash

group=/Groups/nixbld
gid=502

# Create nixbld group
dscl . create $group

# Give it a group ID
dscl . create $group gid $gid

# Give nixbld group a full name
dscl . create $group RealName "Nix Builders"

# Read group info
dscl . -read $group

# Create build users
for i in $(seq 1 10); do
    user=/Users/nixbld$i
    uid="$((30000 + $i))"
    dscl . create $user
    dscl . create $user RealName "Nix build user $i"
    dscl . create $user PrimaryGroupID "$gid"
    dscl . create $user UserShell /usr/bin/false
    dscl . create $user NFSHomeDirectory /var/empty
    dscl . create $user UniqueID "$uid"
    dseditgroup -o edit -a nixbld$i -t user nixbld
    echo "created nixbld$i user with uid $uid"
done
