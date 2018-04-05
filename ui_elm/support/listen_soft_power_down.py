#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Simple script to listen for a request to shutdown.

This script is used by the SOAP rPi to send a shutdown signal when the server
is shutdown.  The flow of events is:
    
    1. UI Stop button is pressed or hardware soft power down is pressed
    2. Command sent to server to stop
    3. UDP packet from server with stop string is broadcast to the rPi address.
    
The server will use 9823 to broadcast to 9822 and will handle what address 
to send the packet to.  

This has been tested and verified that:
    
    1. The strings below are written to syslog
    2. The system shuts down.
    
Warning: system will **not** shut down if someone is logged on via ftp or ssh.
The `-i` switch is provided to allow shutdown despite inhibitors.

@author: mrichardson
"""

import socket
import os
import syslog

port = 9822

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

s.bind(("", port))

out = "Opening port {} for listening for soft power down"

syslog.syslog(syslog.LOG_INFO, out.format(port))

data, addr = s.recvfrom(1024)


if (data.decode('ascii') == "stop"):
    syslog.syslog(syslog.LOG_INFO, "Soft power down called.  Powering off system.")
    os.system("systemctl poweroff -i")
    