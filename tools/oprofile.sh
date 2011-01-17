!/bin/sh 
set -x 

sudo opcontrol --setup --vmlinux=/usr/src/linux/vmlinux
sudo opcontrol --init 
sudo opcontrol --event=CPU_CLK_UNHALTED:100000:0x00:0:1 --event=L2_LINES_IN:2000:0:0:1 
sudo opcontrol --reset 
sudo opcontrol --start --separate=thread,library 

./Combinatorrent "$@"

sudo opcontrol --dump 
sudo opcontrol --shutdown 
