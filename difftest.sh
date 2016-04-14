#! /bin/bash

set -euo pipefail

NORMAL="\e[0m"
red() {
    echo -e "\e[91m$1$NORMAL"
}
blue() {
    echo -e "\e[96m$1$NORMAL"
}

link_and_run_s() {
    filename="$1"
    binname="$(dirname $filename)/$(basename $filename .s)-s"
    runtime/runtime/linkxi.sh "$filename" -o "$binname"
    "$binname"
}

link_and_run_chomped() {
    filename="$1"
    asm="$(dirname $filename)/$(basename $filename .chomped).s"
    if [[ -e "$asm" ]]; then
        mv "$asm" yoloswagtmp
    fi
    cp "$filename" "$asm"
    binname="$(dirname $filename)/$(basename $filename .chomped)-chomped"
    runtime/runtime/linkxi.sh "$asm" -o "$binname"
    if [[ -e yoloswagtmp ]]; then
        mv yoloswagtmp "$asm"
    else
        rm "$asm"
    fi
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

        # "--tcdebug"
        # "--ast-cfold"
    flags=(
        "--basicir"
        "--ir-acfold"
        "--ir-cfold"
        "--lower"
        "--irgen"
        "--asmdebug"
        "--asmchomp --asmdebug"
    )
        # "typeddebug"
        # "astcfold"
    extensions=(
        "basicir"
        "iracfold"
        "ircfold"
        "lower"
        "ir"
        "s"
        "chomped"
    )
        # "./xi"
        # "./xi"
    evaluators=(
        "./ir"
        "./ir"
        "./ir"
        "./ir"
        "./ir"
        "link_and_run_s"
        "link_and_run_chomped"
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
