#! /bin/bash

# set -e
set -u
set -o pipefail

STATUS=0

die() {
    local frame=0
    while caller $frame; do
        ((frame++));
    done
    STATUS=1
}

assert_exists() {
    if [[ ! -e "$1" ]]; then
        echo "error: $1 does not exist"
        die
    fi
}

# test that we have a --help option that retuns 0
test_help() {
    ./xic --help &> /dev/null
}

# test that we produce the correct file extensions
test_file_extensions() {
    # check file extensions
    test1="xisrc/theirs/test1"
    rm -f "$test1.lexed"
    ./xic -libpath xilib --lex "$test1.xi"
    assert_exists "$test1.lexed"

    rm -f "$test1.parsed"
    ./xic -libpath xilib --parse "$test1.xi"
    assert_exists "$test1.parsed"

    rm -f "$test1.typed"
    ./xic -libpath xilib --typecheck "$test1.xi"
    assert_exists "$test1.typed"

    rm -f "$test1.ir"
    ./xic -libpath xilib --irgen "$test1.xi"
    assert_exists "$test1.ir"

    rm -f "$test1.s"
    ./xic -libpath xilib "$test1.xi"
    assert_exists "$test1.s"
}

# test -sourcepath option from same directory as compiler
test_sourcepath_cwd() {
    # trivial sourcepath is cwd, relative filename
    rm -f "xisrc/theirs/test1.lexed"
    ./xic --lex -sourcepath . xisrc/theirs/test1.xi
    assert_exists "xisrc/theirs/test1.lexed"

    # simple sourcepath is cwd, relative filename
    rm -f "xisrc/theirs/test1.lexed"
    ./xic --lex -sourcepath ../compilers xisrc/theirs/test1.xi
    assert_exists "xisrc/theirs/test1.lexed"

    # complex sourcepath is cwd, relative filename
    rm -f "xisrc/theirs/test1.lexed"
    ./xic --lex -sourcepath ../compilers/src/../. xisrc/theirs/test1.xi
    assert_exists "xisrc/theirs/test1.lexed"

    # complex sourcepath is cwd, relative filename
    rm -f "xisrc/theirs/test1.lexed"
    ./xic --lex -sourcepath "$PWD" xisrc/theirs/test1.xi
    assert_exists "xisrc/theirs/test1.lexed"

    # simple sourcepath is not cwd, relative filename
    mkdir -p "theirs"
    rm -f "theirs/test1.lexed"
    ./xic --lex -sourcepath xisrc theirs/test1.xi
    assert_exists "theirs/test1.lexed"

    # trivial sourcepath is cwd, absolute filename
    rm -f "xisrc/theirs/test1.lexed"
    ./xic --lex -sourcepath . "$PWD/xisrc/theirs/test1.xi"
    assert_exists "xisrc/theirs/test1.lexed"

    # simple sourcepath is cwd, absolute filename
    rm -f "xisrc/theirs/test1.lexed"
    ./xic --lex -sourcepath src/.. "$PWD/xisrc/theirs/test1.xi"
    assert_exists "xisrc/theirs/test1.lexed"

    # complex sourcepath is not cwd, absolute filename
    rm -f "xisrc/theirs/test1.lexed"
    ./xic --lex -sourcepath xisrc/theirs/../ "$PWD/xisrc/theirs/test1.xi"
    assert_exists "xisrc/theirs/test1.lexed"

    # complex sourcepath is not cwd, absolute filename
    rm -f "xisrc/theirs/test1.lexed"
    ./xic --lex -sourcepath "$PWD/xisrc/theirs/../" "$PWD/xisrc/theirs/test1.xi"
    assert_exists "xisrc/theirs/test1.lexed"
}

# test -sourcepath option from directory other than compiler
test_sourcepath_otherdir() {
    mkdir -p tempdir
    cd tempdir

    # trivial sourcepath is cwd, relative filename
    rm -f "../xisrc/theirs/test1.lexed"
    ../xic --lex -sourcepath . ../xisrc/theirs/test1.xi
    assert_exists "../xisrc/theirs/test1.lexed"

    # simple sourcepath is cwd, relative filename
    rm -f "../xisrc/theirs/test1.lexed"
    ../xic --lex -sourcepath ../tempdir/ ../xisrc/theirs/test1.xi
    assert_exists "../xisrc/theirs/test1.lexed"

    # complex sourcepath is cwd, relative filename
    rm -f "../xisrc/theirs/test1.lexed"
    ../xic --lex -sourcepath ../src/../././tempdir ../xisrc/theirs/test1.xi
    assert_exists "../xisrc/theirs/test1.lexed"

    # complex sourcepath is cwd, relative filename
    rm -f "../xisrc/theirs/test1.lexed"
    ../xic --lex -sourcepath "$PWD" ../xisrc/theirs/test1.xi
    assert_exists "../xisrc/theirs/test1.lexed"

    # simple sourcepath is not cwd, relative filename
    mkdir -p "theirs"
     rm -f "theirs/test1.lexed"
    ../xic --lex -sourcepath ../xisrc theirs/test1.xi
     assert_exists "theirs/test1.lexed"

    # trivial sourcepath is cwd, absolute filename
    rm -f "../xisrc/theirs/test1.lexed"
    ../xic --lex -sourcepath . "$PWD/../xisrc/theirs/test1.xi"
    assert_exists "../xisrc/theirs/test1.lexed"

    # simple sourcepath is cwd, absolute filename
    rm -f "../xisrc/theirs/test1.lexed"
    ../xic --lex -sourcepath src/.. "$PWD/../xisrc/theirs/test1.xi"
    assert_exists "../xisrc/theirs/test1.lexed"

    # complex sourcepath is not cwd, absolute filename
    rm -f "../xisrc/theirs/test1.lexed"
    ../xic --lex -sourcepath xisrc/theirs/../ "$PWD/../xisrc/theirs/test1.xi"
    assert_exists "../xisrc/theirs/test1.lexed"

    # complex sourcepath is not cwd, absolute filename
    rm -f "../xisrc/theirs/test1.lexed"
    ../xic --lex -sourcepath "$PWD/xisrc/theirs/../" "$PWD/../xisrc/theirs/test1.xi"
    assert_exists "../xisrc/theirs/test1.lexed"

    cd ..
}

main() {
    test_help
    test_file_extensions
    test_sourcepath_cwd
    test_sourcepath_otherdir
    exit $STATUS
}

main "$@"
