# gCRD-PAS software requirements, specifications, and desires

The current plan for the gCRD-PAS instrument will need four separate pieces of software:
Main DAQ running on the cRIO
A status display system running on the beaglebone with small LCD display (7”)
A windows-based UI for the cRIO
Separate software running on a windows-based computer, alignment, status, and manual operation.

Features of the overall system:
* Execute automated scripted on startup.
* The window-based user interface should be able to be attached and detached without affecting the operation of the instrument

## Main cRIO DAQ

### Processing CRD and PAS signals, modulate lasers

This is relatively well defined by existing instrumentation. Although there are a few details that are different in this instrument. 
First, the CRD laser is q-switched with a saturated absorber that leads to a few usec. of timing jitter on the beginning of the ringdown. The ringdown data must be aligned before coadding.  The maximum ringdown time for this instrument will be ~100 us (based on mirror reflectivity,11 ppm losses, and a 35 cm cavity, Pressure = 0 mb).  The minimum ringdown time will be 1 us (limited by the DAQ sampling speed. I’m also planning on incorporating a 4 MHz low pass filter on the PMT signal which will also limit the minimum observable ringdown time. The extinction for a 1 us ringdown is ~ 3000Mm-1 for a 35 cm cavity.  This will be the upper range of the extinction measurement. )  This timing jitter issue has already been addressed in the current incarnation of the instrument but it needs to be tested for short ringdown times.
Second, rather than fitting every ringdown trace I would like to co-add approx. 100 ringdowns and fit the average. I would be nice if these parameters were set in the ini. File.  The CRD laser modulation will be 1 kHz.


For the PAS software I would like it to be largely similar to the current AOP software, of course with a few changes.  First. I would like to be able to turn on the speaker separately for each PAS cell. Second, I would like to save all of the microphone and PD data. Perhaps average to 5 kHz sampling if speed or memory is an issue.


### Control Four heaters/Fans
		
		PAS heaters: To save space, we’ve decided to use software to  control the heaters for the PAS cells using software instead of a stand-alone temperature controller. A digital output from the cRIO will control an AC relay which will drive a ~ 30 W AC heater attached to the PAS cell. The expected default setpoint will be 30 deg. C. The temperature will be measured with a thermocouple attached to the PAS cell.  The controler type will be a standard PID controller with the PID parameters, default set point, and ON/OFF to be set in the ini. file.

CRD heater: This is just a copy of the PAS heaters. I’m not sure that it is needed, but I would like to have it implemented in the electronics and software.  This would be used if the CRD alignment was not stable as the cabin temperature fluctuated, possible during preflight.

	Box fans: Fans are needed to cool box; however I would like to avoid an unnecessary acoustic noise from the fans, so the plan is to use large, adjustable speed fans and run them at low speed

3) Control valves.
One hanbay valve
 solenoid valves
AUX analog output and digital outputs

4) Collecting ancillary data:
	Communication with one vaisala RH probe RS 485
	Communication with 2 honeywell pressure transducers
	Communication with 4 alicat flow controllers
	Temperatures
	State of front panel buttons, setpoints, and switches
	Aux analog input and digital inputs
	
5) Initialization file for the cRIO:
		CRD laser modulation frequency: usually 1kHz
		Sampling period for each ringdown: 1000 us
		Number of ringdowns to co-add =  100

		PAS #1 heater, default set point, PID parameters
		PAS #2 heater, default set point, PID parameters

		CRD heater, default set point, PID parameters

		Box fans,default set point, PID parameters

		Speaker amplitude, center frequency and bandwidth, fit bandwidth

		Flow controller addresses, default setpoints
		
Save data
	Save data in a text format. Data should always be saved when the instrument is running. The user should not be able to turn it off.
	
	Five separate files should be saved regularly:
Time series data at maybe 2Hz. (Matt: What is the best rate to save data. 2-5 Hz would be nice. What is possible?
Copy of the ini. File currently in use
Copy of the Automation Script  currently in use
CRD Ringdown traces: one random and one co-added to 1 sec. (both channels)
PAS microphone signal and PD signal averaged to 5 kHz. (both channels)

We need have a warning when drive space is getting low.	

	How large should the harddrive be? I’ve seen 512 GB SSD and we could use a regular external hard drive to go a bit bigger, say 2 TB.
		
Automation Script for instrument operation
	Actions
sample/wait
Toggle Hanbay valve
Open/close solenoid valve #1
Open/close solenoid valve #2
PAS speaker ON/OFF
Set flow in each  flow controller
Set aux digital output
Set aux analog output
Set flag valve - just a value to be save along with raw data for data reduction purposes
Goto instruction XX:  this would be used to run a sample/calibration routine indefinitely.
Set AUX outputs


Real time calculation of extinction and absorption
Communicate with Front panel status display

Control status indicators and physical front panel switches 
	Front panel lamp - AC power - no connection with software
	Front Panel LED #1 - Lasers ON
	Front Panel LED #2 - software heartbeat
	Front Panel LED #3 - Error
	Front Panel LED#4 - mode indicator/ auto/idle
	Front panel button  - initiate soft shutdown
	Front panel switch - auto/idle ( maybe this should be a button.) ( if depressed during start up the instrument will start in idle mode
	
	
 Windows based UI
Controls
	Flow controllers, setpoints
	Heater, PID parameters, setpoints
	Hanbay valves
	Solenoid valves
	PAS speakers
Load script , execute script, (any editing of the script can be done offline in a text editor.)

	     b) Numeric displays
			CRD time constants
			Temperatures
			Heater ON/OFF percentage
			Flows
			PAS modulation frequency
			
			
	
	     c)  graphical displays
			1) ringdown plot + times series of tau and peak intensity
			2) PAS time series + acoustic spectrum		
			3) adjustable time series plot (2x)
				a) ability to select any two data traces
				b) ability to select the length to time displayed


Beaglebone status display
	This will display one graph with traces of extinction and absorption for a setable amount of time (5 min, 1 hour, 5 hours).   Numeric display of current extinction and absorption as well.

Alignment software
	I expect this will be a separate piece of software written in labview that communicates with the PAS lasers, CRD and PAS cameras, motorized mirror mounts, and the motorized rotation stages. This needs to run on a windows based computer as the driver for these are only avaliable in windows.  This software will not be critical to the instrument operation during sampling but will monitor the laser power and temperature, and occasionally save images of the CRD and PAS modes.  The main use will be during the alignment where it will be used to adjust the motorized mirrors and rotations stage and view the camera images.
	

