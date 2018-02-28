## System Automation

Automation is controlled via the user interface via the two
buttons:

* `Run Sequence`
* `RESET SEQUENCE`

The former is a toggle.  Toggling this button will change the current run
state of the sequence on the server.  Toggling the button to active will
cause the sequence to run.  Toggling the button to off will **pause** the
sequence.  This means that the sequence will **resume** operations where
from the last step.

The second button will reload the sequence and resume the sequence from
beginning when the state of the sequence is set to run.  This button may
be pressed when the sequence is paused thus setting the sequence
to start from the initial step when operation of the sequence is resumed.