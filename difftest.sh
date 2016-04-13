#! /bin/bash

set -euo pipefail

NORMAL="\e[0m"
red() {
    echo -e "\e[91m$1$NORMAL"
}
blue() {
    echo -e "\e[96m$1$NORMAL"
}

link_and_run() {
    filename="$1"
    binname="$(dirname $filename)/$(basename $filename .s)"
    runtime/runtime/linkxi.sh "$filename" -o "$binname"
    "$binname"
}

run_and_diff() {
    f="$1"

    nonce=TEMP
    dname=$(dirname $f)
    fname=$(basename $f)
    noext=$(basename $f .xi)
    pervasive_file="$dname/pervasives.xi"
    tempname="$dname/${nonce}_$fname"
    tempname_noext="$dname/${nonce}_$noext"

    # don't do anything for temp files or pervasives
    if [[ "$fname" == "$nonce"* || "$fname" == "pervasives.xi" ]] ; then
        return
    fi

    blue $fname

    cat "$pervasive_file" "$f" > "$tempname"

    flags=(
        "--tcdebug"
        "--ast-cfold"
        "--basicir"
        "--ir-acfold"
        "--ir-cfold"
        "--lower"
        "--irgen"
        ""
    )
    extensions=(
        "typeddebug"
        "astcfold"
        "basicir"
        "iracfold"
        "ircfold"
        "lower"
        "ir"
        "s"
    )
    evaluators=(
        "./xi"
        "./xi"
        "./ir"
        "./ir"
        "./ir"
        "./ir"
        "./ir"
        "link_and_run"
    )

    generated=()
    for ((i = 0; i < ${#extensions[@]}; ++i)); do
        generated[i]="$tempname_noext.${extensions[$i]}"
    done

    outputfiles=()
    for ((i = 0; i < ${#generated[@]}; ++i)); do
        outputfiles[i]="${generated[$i]}-out"
    done

    for flag in "${flags[@]}"; do
        cmd="./xic -libpath xilib $flag $tempname"
        echo "    $cmd"
        $cmd
    done

    for ((i = 0; i < "${#generated[@]}"; ++i)); do
        echo "    ${evaluators[$i]} ${generated[$i]} &> ${outputfiles[$i]}"
        "${evaluators[$i]}" "${generated[$i]}" &> "${outputfiles[$i]}" || true
    done

    for ((i = 0; i < ${#outputfiles[@]}; ++i)); do
        for ((j = i + 1; j < ${#outputfiles[@]}; ++j)); do
            filea="${outputfiles[$i]}"
            fileb="${outputfiles[$j]}"
            if ! diff "$filea" "$fileb" > /dev/null; then
                red "$filea and $fileb" differ
                exit 1
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
