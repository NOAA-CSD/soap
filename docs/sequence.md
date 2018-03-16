## Sequence configuration

The sequence file that is used by the system is called `sequence.xml` and is located on the server under the folder `cfg`.  The file itself is an xml file and has a markup structure similar to an HTML file - all tags need to be opened and closed (with the `<tag> </tag>` syntax).  The xml file uses the head:

```
<?xml version="1.0" encoding="utf-8"?>
```

This is ignored but keep it for future use.  The sequence itself is enclosed with the `ozone` tag.  This tag can be changed to anything as it is ignored but should be retained for future use.

## Sequence tags

The sequence tags that are used are as follows:

* `o3-valve` - **boolean** setting the state of the O3 solenoid valve; `TRUE` is open and `FALSE` is closed.
* `o2-valve` - **boolean** setting the state of the O2 solenoid valve; `TRUE` is open and `FALSE` is closed.
* `filter` - the naming of this step is archaic - this valve is not an on off valve in the case of SOAP but rather an **integer** indicating the position of the sample flow; 0 is for the first cells while 1 is for the second.
* `wait` - **integer** indicating the number of seconds before the next step should execute.
* `uv-lamp` - **boolean** indicating the power state of the UV lamp; `TRUE` is on and `FALSE` is off.
* `speaker` - this is a two-value, comma separated list with the first entry being an **integer** indicating the PAS cell affected and the other a **boolean** indicating the speaker state.  The cell value can be 0 or 1 while `TRUE` indicates the speaker is on.
* `o3-flow` - misnamed; should be `o2-flow` but is what it is.  This is a *float* indicating the desired O2 flow rate.

## An example

In the example below, the system is set up to first start by ensuring that the calibration system is off - valves are closed, the UV lamp is off and there is no O2 flow.  The system switches between the two cells for 60 seconds each, allowing the PAS to retrieve the resonant frequency on the cells that are not pulling sample.  After this, the O3 sampled in each cell for 60 s.  Finally, the O3 system is turned off and the sample flow runs through each cell for 5 minutes, first through cell 1 then through cell 2.

```
<?xml version="1.0" encoding="utf-8"?>
<ozone>
  <o3-flow>0</o3-flow>
	<o3-valve>FALSE</o3-valve>
	<o2-valve>FALSE</o2-valve>
	<uv-lamp>FALSE</uv-lamp>
	<filter>0</filter>
	<speaker>0,FALSE</speaker>
	<speaker>1,TRUE</speaker>
	<wait>20</wait>
	<speaker>1,FALSE</speaker>
	<wait>40</wait>
	<filter>1</filter>
	<speaker>0,TRUE</speaker>
	<wait>20</wait>
	<speaker>0,FALSE</speaker>
	<wait>40</wait>
	<uv-lamp>TRUE</uv-lamp>
	<o3-flow>50</o3-flow>
	<o3-valve>TRUE</o3-valve>
	<o2-valve>TRUE</o2-valve>
	<wait>60</wait>
	<filter>0</filter>
	<wait>60</wait>
	<uv-lamp>FALSE</uv-lamp>
	<o3-flow>0</o3-flow>
	<o3-valve>FALSE</o3-valve>
	<o2-valve>FALSE</o2-valve>
	<filter>0</filter>
	<speaker>0,FALSE</speaker>
	<speaker>1,TRUE</speaker>
	<wait>20</wait>
	<speaker>1,FALSE</speaker>
	<wait>280</wait>
	<filter>1</filter>
	<speaker>0,TRUE</speaker>
	<wait>20</wait>
	<speaker>0,FALSE</speaker>
	<wait>280</wait>
</ozone>
```

## The user interface

The sequence state may be controlled via the `Automation` page.  This page is currently under construction, but contains two controls of importance.  The first control is a toggle with the caption `Run Sequence`.  This control has two states - run and pause.  When the state is switched to run (`TRUE`) the sequence will resume from where it was paused.  If it has not run before, it will run from the start of the sequence defined by `sequence.xml`. 

If the state is set to pause (`FALSE`), the sequence will stop where it currently is.  If the sequence is resumed while it was in the middle of a `wait` state, then the wait will be evaluated relative to where it *first* started.  This means that if the time has been exceeded, it will immediately proceed to the next step..

The other control is the button `Reset`.  When pressed, the sequence, regardless of where it is, will be *reloaded*. This means that if the sequence was edited, it will load the *edited* sequence.  If the current position of the `Run Sequence` toggle is `TRUE`, then the sequence will start immediately.  If the state is paused, then the sequence will start from the beginning when the toggle is reset to `TRUE`.
