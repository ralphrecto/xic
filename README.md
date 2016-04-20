# Xi Compiler #
A compiler for the Xi programming language.

## Getting Started ##
Before you get started, you may want to be running in the VM described below.
Once you have that set up, read on!

This repository comes with a spiffy [Makefile](Makefile) that can build code,
run tests, generate documentation and more. All the libraries you need to build
and run code are also packaged with this repository, so `make` should "just
work" right out of the box.

| **Command**    | **Action**                                                       |
| -------------- | ---------------------------------------------------------------- |
| `make clean`   | Removes all the garbage generated by compilation.                |
| `make src`     | Compiles all [`src`](src/mjw297) and [`test`](test/mjw297) code. |
| `make test`    | Executes all JUnit tests in [`test`](test/mjw297).               |
| `make doc`     | Generates Javadoc documentation into `doc`.                      |
| `make publish` | Publishes OCaml code coverage [online][github_javadoc].          |
| `make`         | Executes all of the above commands.                              |

After you run `make src`, you can execute our compiler's main executable:
[`xic`](xic), which is actually just a dirt simple shell script that invokes
the Java runtime on our compiler's compiled `.class` files.

## OCaml dependencies ##
If you don't want to use the VM, you can also download all the dependencies
yourself.
```bash
opam install core async ounit ocamlgraph bisect bisect_ppx
```

## Flags ##
- `--help`
- `--lex`: `a/b/foo.xi --> a/b/foo.lexed`
- `--parse`: `a/b/foo.xi --> a/b/foo.parsed`
- `--typecheck`: `a/b/foo.xi --> a/b/foo.typed`
- `--irgen`: `a/b/foo.xi --> a/b/foo.ir`
- `-sourcepath <path> [default: dir where xic run]`
    - `-sourcepath a/b  c/d/foo.xi: a/b/c/d/foo.xi -->  c/d/foo._`
    - `-sourcepath a/b /c/d/foo.xi: a/b/c/d/foo.xi --> /c/d/foo._`
- `-libpath <path> [default: dir where xic run]`
- `-D <path> [default: dir where xic run]`
    - `-D a/b/  c/d/foo.xi: c/d/foo.xi --> a/b/c/d/foo.xi`
    - `-D a/b/ /c/d/foo.xi: c/d/foo.xi --> /c/d/foo.xi`
    - `-sourcepath 0/1/ -D a/b/ /c/d/foo.xi: 0/1/c/d/foo.xi --> a/b/c/d/foo.xi`
- `-O`

## x86-64 ##
|     |      |      |              |
| --- | ---- | ---- | ------------ |
| rax |      | RET1 |              |
| rbx |      |      | callee-saved |
| rcx | ARG4 |      |              |
| rdx | ARG3 | RET2 |              |
| rsi | ARG2 |      |              |
| rdi | ARG1 |      |              |
| rbp |      |      | callee-saved |
| rsp |      |      |              |
| r8  | ARG5 |      |              |
| r9  | ARG6 |      |              |
| r10 |      |      |              |
| r11 |      |      |              |
| r12 |      |      | callee-saved |
| r13 |      |      | callee-saved |
| r14 |      |      | callee-saved |
| r15 |      |      | callee-saved |

## Virtual Machine ##
A virtual machine, managed by [vagrant][], can be found in the
[`vagrant`](vagrant) directory. The virtual machine comes with a standard
installation of Java and miscellaneous Java tools to make building the Xi
compiler consistent and easy. In this section, we show you how to get started
with the VM.

First, make sure you have a recent version of vagrant and VirtualBox installed,
then navigate to the `vagrant` directory and run the following command:

```bash
vagrant up
```

This will launch the virtual machine. Note that running `vagrant up` may run
for quite a while the first time you run it; vagrant has to install the virtual
machine image on to your machine. After the virtual machine is up and running,
you can connect to it with the following command:

```bash
vagrant ssh
```

Once you've ssh'ed into the virtual machine, run the following commands to
install Java 8, [Eclipse Mars][eclipse_mars], [eclim][],
[ghp-import](https://github.com/davisp/ghp-import), and OCaml.

```bash
# make sure to run this in the virtual machine
bash /vagrant/install_java8.sh
source /vagrant/install_ocaml.sh
```
The installation script sets up a bunch of environment variables in the
`~/.bash_path` file in the virutal machine. To make sure these variables are
exported, add the following to your `~/.bashrc` in the virtual machine:

```bash
# again, you should be in the virtual machine
# Path
if [ -f ~/.bash_path ]; then
    . ~/.bash_path
fi
```

After this, run `source ~/.bashrc` (or start a new terminal) and you should be
good to go! Try running `java -version`; you should get the following output:

```bash
> java -version
java version "1.8.0_71"
Java(TM) SE Runtime Environment (build 1.8.0_71-b15)
Java HotSpot(TM) 64-Bit Server VM (build 25.71-b15, mixed mode)
```

One last thing, if you don't want to install Eclipse or eclim or ghp-import,
just comment out the appropriate lines in the `install_java8.sh` script before
you run it.

## Resources ##
- [CS 4120 Website](http://www.cs.cornell.edu/courses/cs4120/2016sp/)
- [Xi Language Specification](http://www.cs.cornell.edu/courses/cs4120/2016sp/project/language.pdf)
- [Overview Documentation Requirements](http://www.cs.cornell.edu/courses/cs4120/2016sp/hw/overview-requirements.html)
- [Programming Assignment 1](http://www.cs.cornell.edu/courses/cs4120/2016sp/pa/pa1/pa1.pdf)
- [Online Javadoc Documentation][github_javadoc]

[eclim]:          http://eclim.org/
[eclipse_mars]:   https://eclipse.org/mars/
[github_javadoc]: http://ralphrecto.github.io/compilers/
[vagrant]:        https://www.vagrantup.com/
