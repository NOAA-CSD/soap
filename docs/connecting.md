# Connecting to the Controller

At present time, the controller is located at 10.172.240.107.  This address is prone to changing based on the subnet it resides on.  However, the port for communication with a *published* web service will *always* be 8080 and the web service is located in `soap` (when debugging, the web service cab be found at port 8001).  In order to connect to the client page (for viewing data and control), point the browser to

> http://10.172.240.107:8080/soap

