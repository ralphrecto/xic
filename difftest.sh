#! /bin/bash

set -euo pipefail

NORMAL="\e[0m"

red() {
    echo -e "\e[91m$1$NORMAL"
}

blue() {
    echo -e "\e[96m$1$NORMAL"
}

usage() {
    echo "Usage:"
    echo "  difftest test.xi..."
    echo ""
    echo "Options:"
    echo "  -h    Print this message"
    echo "  -n    Don't cat pervasives.xi"
}

link_and_run() {
    filename="$1"
    base="$(basename $filename)"
    ext="${base##*.}"
    binname="$(dirname $filename)/$(basename $filename $ext)-$ext"
    runtime/runtime/linkxi.sh "$filename" -o "$binname"
    "$binname"
}

run_and_diff() {
    f="$1"
    cat_pervasives="$2"

    nonce=TEMP
    dname=$(dirname $f)                     # foo/bar
    fname=$(basename $f)                    # file.xi
    noext=$(basename $f .xi)                # file
    pervasive_file="$dname/pervasives.xi"   # foo/bar/pervasives.xi
    tempname="$dname/${nonce}_$fname"       # foo/bar/TEMP_file.xi
    tempname_noext="$dname/${nonce}_$noext" # foo/bar/TEMP_file

    # don't do anything for temp files or pervasives
    if [[ "$fname" == "$nonce"* || "$fname" == "pervasives.xi" ]] ; then
        return
    fi

    blue $fname

    if "$cat_pervasives"; then
        cat "$pervasive_file" "$f" > "$tempname"
    else
        echo -n "" > "$tempname"
        echo "use conv" >> "$tempname"
        echo "use io"   >> "$tempname"
        echo ""         >> "$tempname"
        cat "$f"        >> "$tempname"
    fi

    should_run=()
    should_run+=(true) # typed
    should_run+=(true) # typed-acf
    should_run+=(true) # nolower
    should_run+=(true) # lower
    should_run+=(true) # ir
    should_run+=(true) # ir-cp
    should_run+=(true) # ir-pre
    should_run+=(true) # ir-opt
    should_run+=(true) # munch
    should_run+=(true) # chomp
    should_run+=(true) # s

    names=()
    names+=("typed")
    names+=("typed-acf")
    names+=("nolower")
    names+=("lower")
    names+=("ir")
    names+=("ir-cp")
    names+=("ir-pre")
    names+=("ir-opt")
    names+=("munch")
    names+=("chomp")
    names+=("s")

    flags=()
    flags+=("--tcdebug")
    flags+=("--tcdebug -Oacf")
    flags+=("--nolower -Oacf")
    flags+=("--lower   -Oacf")
    flags+=("--irgen")
    flags+=("--optir final -Ocp")
    flags+=("--optir final -Opre")
    flags+=("--optir final")
    flags+=("-O")
    flags+=("-Ois")
    flags+=("")

    generated=()
    generated+=("${tempname_noext}.typeddebug")
    generated+=("${tempname_noext}.typeddebug")
    generated+=("${tempname_noext}.nolower")
    generated+=("${tempname_noext}.lower")
    generated+=("${tempname_noext}.ir")
    generated+=("${tempname_noext}_final.ir")
    generated+=("${tempname_noext}_final.ir")
    generated+=("${tempname_noext}_final.ir")
    generated+=("${tempname_noext}.s")
    generated+=("${tempname_noext}.s")
    generated+=("${tempname_noext}.s")

    evaluators=()
    evaluators+=("./xi")
    evaluators+=("./xi")
    evaluators+=("./ir")
    evaluators+=("./ir")
    evaluators+=("./ir")
    evaluators+=("./ir")
    evaluators+=("./ir")
    evaluators+=("./ir")
    evaluators+=("link_and_run")
    evaluators+=("link_and_run")
    evaluators+=("link_and_run")

    # generate files and make them have unique names
    named=()
    for ((i = 0; i < ${#names[@]}; ++i)); do
        named[i]="$tempname_noext.${names[$i]}"
    done

    for ((i = 0; i < "${#flags[@]}"; ++i));do
        if "${should_run[$i]}"; then
            flag="${flags[$i]}"
            gen="${generated[$i]}"
            name="${named[$i]}"
            cmd="./xic -libpath xilib $flag $tempname"
            echo "    $cmd"
            $cmd
            if [[ $gen != $name ]]; then
                mv "$gen" "$name"
            fi
        fi
    done

    outputfiles=()
    for ((i = 0; i < ${#named[@]}; ++i)); do
        outputfiles[i]="${named[$i]}-out"
    done

    for ((i = 0; i < "${#named[@]}"; ++i)); do
        if "${should_run[$i]}"; then
            echo "    ${evaluators[$i]} ${named[$i]} &> ${outputfiles[$i]}"
            "${evaluators[$i]}" "${named[$i]}" &> "${outputfiles[$i]}" || true
        fi
    done

    for ((i = 0; i < ${#outputfiles[@]}; ++i)); do
        for ((j = i + 1; j < ${#outputfiles[@]}; ++j)); do
            if "${should_run[$i]}" && "${should_run[$j]}"; then
                filea="${outputfiles[$i]}"
                fileb="${outputfiles[$j]}"
                if ! diff "$filea" "$fileb" > /dev/null; then
                    red "$filea and $fileb" differ
                fi
            fi
        done
    done
}

main() {
    cat_pervasives=true

    while getopts "nh" opt; do
      case $opt in
        n)
          cat_pervasives=false
          ;;
        h)
          usage
          exit 0
          ;;
        \?)
          echo "Invalid option: -$OPTARG" >&2
          exit 1
          ;;
      esac
    done
    shift $((OPTIND-1))

    if [[ $# == 0 ]]; then
        usage
        exit 1
    fi

    for f in "$@"; do
        run_and_diff "$f" "$cat_pervasives"
    done
}

main "$@"
