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
    base="$(basename $filename)"
    ext="${base##*.}"
    binname="$(dirname $filename)/$(basename $filename $ext)-$ext"
    runtime/runtime/linkxi.sh "$filename" -o "$binname"
    "$binname"
}

run_and_diff() {
    f="$1"

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

    cat "$pervasive_file" "$f" > "$tempname"

    names=()
    names+=("typed")
    names+=("typed-acf")
    names+=("nolower")
    names+=("lower")
    names+=("ir")
    names+=("ir-cp")
    # names+=("ir-pre")
    # names+=("ir-opt")
    # names+=("munch")
    # names+=("chomp")
    # names+=("s")

    flags=()
    flags+=("--tcdebug")
    flags+=("--tcdebug -Oacf")
    flags+=("--nolower -Oacf")
    flags+=("--lower   -Oacf")
    flags+=("--irgen")
    flags+=("--optir final -Oacf -Oicf -Ocp")
    # flags+=("--optir final -Oacf -Oicf -Opre")
    # flags+=("--optir final")
    # flags+=("--asmdebug -Oacf -Oicf -Opre -Ocp")
    # flags+=("--asmdebug -Oacf -Oicf -Opre -Ocp -Ois")
    # flags+=("--asmdebug")

    generated=()
    generated+=("${tempname_noext}.typeddebug")
    generated+=("${tempname_noext}.typeddebug")
    generated+=("${tempname_noext}.nolower")
    generated+=("${tempname_noext}.lower")
    generated+=("${tempname_noext}.ir")
    generated+=("${tempname_noext}_final.ir")
    # generated+=("${tempname_noext}_final.ir")
    # generated+=("${tempname_noext}_final.ir")
    # generated+=("${tempname_noext}.s")
    # generated+=("${tempname_noext}.s")
    # generated+=("${tempname_noext}.s")

    evaluators=()
    evaluators+=("./xi")
    evaluators+=("./xi")
    evaluators+=("./ir")
    evaluators+=("./ir")
    evaluators+=("./ir")
    evaluators+=("./ir")
    # evaluators+=("./ir")
    # evaluators+=("./ir")
    # evaluators+=("link_and_run_s")
    # evaluators+=("link_and_run_s")
    # evaluators+=("link_and_run_s")

    # generate files and make them have unique names
    named=()
    for ((i = 0; i < ${#names[@]}; ++i)); do
        named[i]="$tempname_noext.${names[$i]}"
    done

    for ((i = 0; i < "${#flags[@]}"; ++i));do
        flag="${flags[$i]}"
        gen="${generated[$i]}"
        name="${named[$i]}"
        cmd="./xic -libpath xilib $flag $tempname"
        echo "    $cmd"
        $cmd
        if [[ $gen != $name ]]; then
            mv "$gen" "$name"
        fi
    done

    outputfiles=()
    for ((i = 0; i < ${#named[@]}; ++i)); do
        outputfiles[i]="${named[$i]}-out"
    done

    for ((i = 0; i < "${#named[@]}"; ++i)); do
        echo "    ${evaluators[$i]} ${named[$i]} &> ${outputfiles[$i]}"
        "${evaluators[$i]}" "${named[$i]}" &> "${outputfiles[$i]}" || true
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
