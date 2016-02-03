#! /bin/sh

set -euo pipefail

readonly JAVA_VERSION=71
readonly JAVA_B=15

# dependencies
sudo apt-get install -y libswt-gtk-3-java

# jdk (http://stackoverflow.com/a/10959815/3187068)
wget --no-check-certificate --no-cookies --header "Cookie: oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/8u$JAVA_VERSION-b$JAVA_B/jdk-8u$JAVA_VERSION-linux-x64.tar.gz"
tar -xzvf jdk-8u$JAVA_VERSION-linux-x64.tar.gz
echo 'export JAVA_HOME="$HOME/jdk1.8.0_'$JAVA_VERSION'"' >> ~/.bash_path
echo 'export PATH="$JAVA_HOME/bin:$PATH"'                >> ~/.bash_path

# eclipse
wget 'http://eclipse.mirror.rafal.ca/technology/epp/downloads/release/mars/1/eclipse-java-mars-1-linux-gtk-x86_64.tar.gz'
tar -xzvf 'eclipse-java-mars-1-linux-gtk-x86_64.tar.gz'
echo 'export ECLIPSE_HOME="$HOME/eclipse"' >> ~/.bash_path
echo 'export PATH="$ECLIPSE_HOME:$PATH"'   >> ~/.bash_path

# eclim
wget 'http://downloads.sourceforge.net/project/eclim/eclim/2.5.0/eclim_2.5.0.jar?r=&ts=1453915367&use_mirror=vorboss' -O eclim_2.5.0.jar
JAVA_HOME="$HOME/jdk1.8.0_$JAVA_VERSION" PATH="$JAVA_HOME/bin:$PATH" jdk1.8.0_$JAVA_VERSION/bin/java -Dvim.files=/home/vagrant/.vim -Declipse.home=/home/vagrant/eclipse -jar eclim_2.5.0.jar install

# # maven
# wget 'http://mirrors.advancedhosters.com/apache/maven/maven-3/3.3.3/binaries/apache-maven-3.3.3-bin.tar.gz'
# tar -xzvf apache-maven-3.3.3-bin.tar.gz
# echo 'export MAVEN_HOME="$HOME/apache-maven-3.3.3"' >> ~/.bash_path
# echo 'export PATH="$MAVEN_HOME/bin:$PATH"'          >> ~/.bash_path
