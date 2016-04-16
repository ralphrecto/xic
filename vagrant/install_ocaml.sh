sudo apt-get install aspcud m4 unzip
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - \
    | sh -s /usr/local/bin 4.02.3
eval `opam config env`
opam init
opam install core async ounit ocamlgraph bisect bisect_ppx
