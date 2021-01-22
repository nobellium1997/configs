#!/bin/bash

# Adapter from glenn jackman on http://askubuntu.com/questions/180612/script-to-mute-an-application
# to depend directly on the name of the PulseAudio client
# rather than the application name (several instances of one application could
# run while only one is connected to PulseAudio)

# Possible further improvement: it could be useful to also mute all clients having
# the specified name. Here, only the first one is muted.

#!/bin/bash

main() {
    local action=mute
    while getopts :hu option; do
        case "$option" in
            h) usage 0 ;;
            u) action=unmute ;;
            ?) usage 1 "invalid option: -$OPTARG" ;;
        esac
    done
    shift $((OPTIND - 1))

    if [[ "$1" ]]; then
        $action "$1"
    else
        usage 1 "specify the name of a PulseAudio client"
    fi
}

usage() {
    [[ "$2" ]] && echo "error: $2"
    echo "usage: $0 [-h] [-u] appname"
    echo "where: -u = ummute application (default action is to mute)"
    exit $1
}

mute()   { adjust_muteness "$1" 1; }
unmute() { adjust_muteness "$1" 0; }

adjust_muteness() {
    local index=$(get_index "$1")
    if [[ -z "$index" ]]; then
        echo "error: no PulseAudio sink named $1 was found" >&2
    else
        [[ "$index" ]] && pacmd set-sink-input-mute "$index" $2 >/dev/null
    fi
}

get_index() {
    pacmd list-sink-inputs |
    awk -v name=$1 '
        $1 == "index:" {idx = $2}
        $1 == "application.name" && $3 == "\"" name "\"" {print idx; exit}
    '
}

main "$@"
