#! /bin/bash

set -euo pipefail

main() {
    for xi_file in "$@"; do
        dir=$(dirname "$xi_file")
        stripped=$(basename --suffix .xi "$xi_file")
        lexedsol="$dir/$stripped.lexedsol"
        ./xic --lex "$xi_file"
        echo "$stripped.lexed" "$lexedsol"
        diff "$stripped.lexed" "$lexedsol"
    done
}

main "$@"
