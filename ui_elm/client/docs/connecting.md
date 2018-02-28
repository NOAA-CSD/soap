## Connecting to the Server

In order to connect to the instrument, make sure that both:

* instrument IP
* port

are set correctly.  These settings can currently be found on the page
labeled `Configuration`.  In the lab, the IP address is set to
10.172.240.107.  When the webservice is published, the port is 8080.
When debugging (i.e. we are running from the LabVIEW environment), this
port is 8001.

The static page for the web service can be accessed via the expected IP
and port at

```
http://[ip-address]:[port]//soap
```

This brings up the web page for access and control of the instrument.

## Accessing the instrument off-site

The instrument may be accessed off-site using openSSH via the command:

```
ssh -L 8080:10.172.240.107:8080 gate.al.noaa.gov
```

With these settings, the user will then set the IP Address field on the
`Configuration` page to localhost (127.0.0.1) and the appropriate port.