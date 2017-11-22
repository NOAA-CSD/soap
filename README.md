# SOAP

## Device Communication

### RS485

All devices communicating via RS485 are connected in a ring network to `COM2` and communicate at 19.2 8-N-1.  Currently, there are three devices that will be connected - a Vaisala hygrometer and two PPTs.  The addresses for these are as follows:

| Device | Address | Description |
| ------ | ------- | ----------- |
| PPT0 | 01 |  |
| PPT1 | Not assigned | | 
| Vaisala | 02 | |

The user manual for the PPT may be found [here](https://aerospace.honeywell.com/en/~/media/aerospace/files/user-manual/precisionpressuretransducerpptpptr-usermanual.pdf).

The user manual for the Vaisala probe may be found [here](https://www.vaisala.com/sites/default/files/documents/HMP60%20and%20HMP110%20Series%20User%27s%20Guide%20in%20English.pdf).

#### Communication

To retrieve data from the Vaisala probe, the command `Send [address]<cr>` is sent where `[adress]` is the address in the table above.  This returns a string that looks like 

```
T=\s\s19.81\s'C\sRH=\s\s19.31\s%RH\sTd=\s\s-3.76\s'C\s\r
```

This returns three values - temperature, relative humidity, and dewpoint temperature.

The PPT retrieves data through the command `[address]P1<cr>`; here `[address]` is defined in the above table.  Devices with an adress smaller than 10 *require a 0 to proceed the command for data*.  Data that is returned from the device will look like:

```
\n#01CP=\s826.4\r
```

The `CP` here indicates a temperature compensated pressure.
