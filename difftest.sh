#! /bin/bash

set -euo pipefail

run_and_diff() {
    f="$1"

    nonce=TEMP
    dname=$(dirname $f)
    fname=$(basename $f)
    noext=$(basename $f .xi)
    pervasive_file="$dname/pervasives.xi"
    tempname="$dname/$nonce-$fname"
    tempname_noext="$dname/$nonce-$noext"

    # don't do anything for temp files or pervasives
    if [[ "$fname" == "$nonce"* || "$fname" == "pervasives.xi" ]] ; then
        return
    fi

    echo $fname

    cat "$pervasive_file" "$f" > "$tempname"

    flags=(
        "--tcdebug"
        "--basicir"
        "--ast-cfold"
        "--ir-cfold"
        "--lower"
        "--irgen"
    )
    extensions=(
        "typeddebug"
        "basicir"
        "astcfold"
        "ircfold"
        "lower"
        "ir"
    )
    evaluators=(
        "./xi"
        "./ir"
        "./ir"
        "./ir"
        "./ir"
        "./ir"
    )

    generated=()
    for ((i = 0; i < ${#extensions[@]}; ++i)); do
        generated[i] = "$tempname_noext.${extensions[$i]}"
    done

    outputfiles=()
    for ((i = 0; i < ${#generated[@]}; ++i)); do
        outputfiles[i] = "${generated[$i]}-out"
    done

    for flag in "${flags[@]}"; do
        cmd="./xic -libpath xilib $flag $tempname"
        echo "    $cmd"
        $cmd
    done

    for ((i = 0; i < "${generated[@]}"; ++i)); do
        cmd="${evaluators[$i]} ${generated[$i]} &> ${outputfiles[$i]}"
        echo "    $cmd"
        $cmd || true
    done
    return

    for ((i = 0; i < ${#outputfiles[@]}; ++i)); do
        for ((j = i + 1; j < ${#outputfiles[@]}; ++j)); do
            filea="${outputfiles[$i]}"
            fileb="${outputfiles[$j]}"
            if ! diff "$filea" "$fileb" > /dev/null; then
                echo "$filea and $fileb" differ
            fi
        done
    done
}

main() {
    if [[ $# == 0 ]]; then
        echo "usage: difftest test.xi..."
    fi

    for f in "$@"; do
        run_and_diff "$f"
    done
}

main "$@"
