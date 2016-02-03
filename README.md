# Xi Compiler #
A compiler for the Xi programming language.

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

Once you've ssh'ed into the virtual machine, run the following command to
install Java 8, [Eclipse Mars][eclipse_mars], and [eclim][].

```bash
# make sure to run this in the virtual machine
bash /vagrant/install_java8.sh
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

One last thing, if you don't want to install Eclipse or eclim, just comment out
the appropriate lines in the `install_java8.sh` script before you run it.

## Resources ##
- [CS 4120 Website](http://www.cs.cornell.edu/courses/cs4120/2016sp/)
- [Xi Language Specification](http://www.cs.cornell.edu/courses/cs4120/2016sp/project/language.pdf)
- [Overview Documentation Requirements](http://www.cs.cornell.edu/courses/cs4120/2016sp/hw/overview-requirements.html)
- [Programming Assignment 1](http://www.cs.cornell.edu/courses/cs4120/2016sp/pa/pa1/pa1.pdf)

[eclim]:        http://eclim.org/
[eclipse_mars]: https://eclipse.org/mars/
[vagrant]:      https://www.vagrantup.com/
