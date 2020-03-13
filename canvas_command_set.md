# SYSTEM ET CANVAS COMMAND SET


## ZOOMING
Zoom to fit:
```
zoom fit
```

Zoom by:
```
zoom level 3
```


Zoom on point:
```
zoom center 10 10 # x/y
```

```
zoom center 10 10 2 # x/y scale
```

To zoom at the current cursor position keep CTRL pressed and press + or -.

## CURSOR
The drawing cursor can be positioned inside the canvas on left mouse button click.
After a click in the canvas the cursor can be moved via the cursor keys (right, left, up, down).
To position the cursor via command line:
```
position cursor absolute 25 30 # x/y
```
```
position cursor relative 5 0 # x/y
```

The cursor position always snaps to the nearest grid point.


## SHOW

To open a module for editing run this command. NOTE: This command is available in the 
schematic editor only.
```
show module LED-driver
```

If the number of a sheet is given, then the sheet will be opened right away:
```
show module LED-driver 2
```

To open a sheet for editing run this command:
```
show sheet 2
```

The show command also locates the unit of a device. If the requested unit
exist, then the drawing pans so that the unit appears at the center.

### In Schematc
If a device has only one unit:
```
show device R1
```

If a device has more than one unit:
```
show device IC1 IO-BANK2
```

### In Board
todo
