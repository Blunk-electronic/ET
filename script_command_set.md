# SYSTEM ET SCRIPT PROCESSOR COMMAND SET

The two letters "CS" indicate a "construction site" where things are not finished yet or intended for the future.

## BASICS
Scripting is an important feature of advanced CAE tools. It allows:
- repeating and automating of tedious or error prone tasks.
- software testing without graphical user interfaces
- generating schematics, layouts and device models via third party software
- The acronym 'CS' indicates a construction site where things are not complete or intended for the future.

The script file must exist in the project directory.
A script in general is just a list of things to do by the CAE system.
CS: link to example script
This list is a plain ASCII file. Comments start with #. 
The script commands have a domain-verb-noun structure.
Their meaning should be obvious in most cases. Additional explanation is provided in comments
if required. The domain-verb-noun structure is: DOMAIN MODULE VERB NOUN PARAMETERS

The domain tells where the operation is to take place. Domains are:
- project
- schematic
- board (or layout)
- symbol, package, device (CS)

The measurement system is METRIC. All dimensions are in millimeters (mm).
All angles and rotations are in degrees (1/360) and in mathematical sense.
Positive rotation (or angle) is counterclockwise. Negative rotation is clockwise.

## THE GUI INTERNAL COMMAND LINE
Both GUIs for schematic and board editor provide a command line where you can enter
commands. Every script command (as explained in the follwing) can be executed and tested
separately via the command line of the schematic or board editor.
Inside the graphical user iterface you must omit domain and module name.


The concept is most easily to understand by examples. The module to be designed or edited is named "led_driver":



## MODULES
In the project domain:
```
project create module led_driver
```
```
project create module templates/filter
```
```
project save module led_driver
```
```
project delete module led_driver
```

In the schematic domain:
```
schematic save module [led_driver]
```
```
schematic delete module [led_driver]
```


## EXIT
The exit or quit command does not require a verb.
```
schematic led_driver exit
```
```
schematic led_driver quit
```
```
board led_driver exit
```
```
board led_driver quit
```


## DRAWING GRID
The drawing grid is to be set for each module. It's default is 1mm in both x and y direction:
<!--```
CS schematic led_driver set grid 2 # x/y 2/2
```-->
```
schematic led_driver set grid 2 2 # x/y 2/2
```
```
board led_driver set grid 0.5 0.5 # x/y 0.5/0.5
```
```
schematic led_driver display grid [on/off]
```




## ZOOMING
Zoom to fit:
```
schematic led_driver zoom fit
```

Zoom by:
```
schematic led_driver zoom level 3
```


Zoom to point:
```
schematic led_driver zoom center 10 10 # x/y
```

```
schematic led_driver zoom center 10 10 2 # x/y level
```

To zoom at the current cursor position keep CTRL pressed and press + or -.




## CURSOR
The drawing cursor can be positioned inside the canvas on left mouse button click.
After a click in the canvas the cursor can be moved via the cursor keys (right, left, up, down).
To position the cursor via command line:
```
schematic led_driver position cursor absolute 25 30 # x/y
```
```
schematic led_driver position cursor relative 5 0 # x/y
```

The cursor position always snaps to the nearest grid point.




## SHOW

To open a module for editing run this command. 
NOTE: This command is available in the 
schematic editor only.
```
schematic led_driver show module LED-driver
```

If the number of a sheet is given, then the sheet will be opened right away:
```
schematic led_driver show module LED-driver 2
```

To open a sheet for editing run this command:
```
schematic led_driver show sheet 2
```

The show command also locates the unit of a device. If the requested unit
exist, then the drawing pans so that the unit appears at the center.

### In Schematc
If a device has only one unit:
```
schematic led_driver show device R1
```

If a device has more than one unit:
```
schematic led_driver show device IC1 IO-BANK2
```

### In Board
CS: todo


## DISPLAY

### In Schematc

The parameter "on/off" may be left off. The default is "on":
```
schematic led_driver display nets [on/off]
```
```
schematic led_driver display ports [on/off]
```
```
schematic led_driver display names [on/off]
```
```
schematic led_driver display values [on/off]
```
```
schematic led_driver display purposes [on/off]
```


### In Board
Board outline or dimensions:
```
board led_driver display outline [on/off]
```

Silkscreen:
```
board led_driver display silkscreen top [on/off]
```
```
board led_driver display silkscreen bottom [on/off]
```

Assembly documentation:
```
board led_driver display assy top [on/off]
```
```
board led_driver display assy bottom [on/off]
```

Keepout:
```
board led_driver display keepout top [on/off]
```
```
board led_driver display keepout bottom [on/off]
```

Solder stop mask:
```
board led_driver display stop top [on/off]
```
```
board led_driver display stop bottom [on/off]
```


Stencil, solder cream mask or solder paste:
```
board led_driver display stencil top [on/off]
```
```
board led_driver display stencil bottom [on/off]
```

Origins of devices:
The origin is a small cross usually in the center of a package.
For the assembly machines this is the placement position.
```
board led_driver display origins top [on/off]
```
```
board led_driver display origins bottom [on/off]
```


Conductor or signal layers (usually copper):
```
board led_driver display conductors 1 [on/off]
```
```
board led_driver display conductors 17 [on/off]
```

Vias:
```
board led_driver display vias [on/off]
```

Via restrict:
```
board led_driver display restrict via 3 [on/off]
```

Route restrict:
```
board led_driver display restrict route 5 [on/off]
```


<!--```
CS board led_driver display conductors all/none
```-->
<!--```
CS board led_driver display vias all/none
```-->





## DEVICES
There are two kinds of devices: Those which have an abstract representation (via a symbol)
in the schematic - we call them "electrical devices". The second type of devices are those
which appear in the layout only. They are used to model things like fiducials or mounting holes.
We refer to them as "non-electrical devices".

### ELECTRICAL DEVICES AND UNITS
Most devices are of this type. Unless otherwise noted we just call them "devices" in this section.
An electrical device is something like a resistor (consisting of a schematic symbol and a package drawing).
A single resistor contains only one symbol:

#### ADDING
```
schematic led_driver add device $HOME/git/BEL/ET_component_library/devices/passive/resistors/R.dev 1 110 10 0 S_0805 # sheet 1 x y rotation [variant]
```

A quad-NAND IC like the famous 7400 offers four symbols (named A..D):

This command places the first available unit at sheet 1 x/y 250/150 with rotation 90 degrees.
The last argument (D) is the package variant.
```
schematic led_driver add device $HOME/git/BEL/ET_component_library/devices/active/logic/7400.dev 1 250 150 90 D # sheet 1 x y rotation variant
```

This command invokes from IC1 the unit P and places it at sheet 4 at x/y 60/90 with zero rotation:
```
schematic led_driver invoke unit IC1 P 4 60 90 0 # device unit sheet x y rotation
```

#### COPYING

```
schematic led_driver copy device R1 2 210 100 0
```

#### RENAMING

```
schematic led_driver rename device R1 R9
```

```
schematic led_driver renumber devices 100
```


#### SETTING VALUE, PURPOSE, PARTCODE, PACKAGE VARIANT
```
schematic led_driver set value R1 2k2
```
```
schematic led_driver set purpose R1 "brightnesss_control"
```
```
schematic led_driver set partcode R1 R_PAC_S_0805_VAL_100R
```
```
schematic led_driver set variant IC1 D
```

#### DELETING
The whole device can be deleted by:
```
schematic led_driver delete device R1
```
To delete a certain unit the unit name must also be provided:
```
schematic led_driver delete unit R1 1
```

```
schematic led_driver delete device IC1 C
```

#### MOVING
##### in the schematic
Moving a unit disconnects it from net old segments and places it at the given position.
If the units ports end up where a net segment is, they become connected with the net.
```
schematic led_driver move unit R1 1 absolute 2 210 100 # R1 unit sheet x y
```
```
schematic led_driver move unit R1 1 relative -1 2 4 # IC1 unit B sheet dx dy
```

Absolute rotation (restores positions of placeholders as specified in the symbol model):
```
schematic led_driver rotate unit R1 1 absolute -270
```

Relative rotation:
```
schematic led_driver rotate unit R1 1 relative 90
```

##### in the board
Inside the drawing frame the board has a position. This anchor point
is usually the lower left corner of the board. For circular boards it is the center.
However, this point is the board origin, where all other position refer to.
The command to move the board requires x/y coordinates relative to the 
lower left corner of the drawing frame:
```
board led_driver move board absolute 20 50
```
```
board led_driver move board relative -5 10
```

Moving a device:
```
board led_driver move device R1 absolute 140 45.2
```
```
board led_driver move device R1 relative 4.5 0
```

Rotating a device:
```
board led_driver rotate device R1 absolute 70
```
```
board led_driver rotate device R1 relative -4
```
NOTE: For devices placed on the bottom side of the board the
rotation is negated.

By default a package model in the library is drawn as if it were placed on
the top side of the board.
Flipping a device to top or bottom means to mirror it along its Y-axis:
```
board led_driver flip device R1 top
```
```
board led_driver flip device R1 bottom
```


#### DRAGGING
Dragging a unit drags the connected net segments along.
If the units ports end up where a net segment is, they become connected with the net.
Dragging is not possible across sheets.

```
schematic led_driver drag unit R1 1 absolute 220 100 # R1 unit x y
```

```
schematic led_driver drag unit R1 1 relative 10 0 # IC1 unit B x y
```

#### MOVING PLACEHOLDERS FOR NAME, VALUE AND PURPOSE
The movement takes place relative to the unit origin.
```
schematic led_driver move name R1 1 absolute 10 15 # R1 unit x y
```
```
schematic led_driver move name R1 1 relative 1 -4 # R1 unit 1 dx dy
```
```
schematic led_driver move value R1 1 relative 1 -4 # R1 unit 1 dx dy
```
```
schematic led_driver move purpose R1 1 relative 1 -4 # R1 unit 1 dx dy
```

Rotating placeholders is always about their anchor point:
```
schematic led_driver rotate name R1 1 0
```
```
schematic led_driver rotate value R1 1 90
```
```
schematic led_driver rotate purpose R1 1 0
```

#### SETTING VALUE, PURPOSE, PARTCODE
```
schematic led_driver set value R1 2k2
```
```
schematic led_driver set purpose R1 "brightnesss_control"
```
```
schematic led_driver set partcode R1 R_PAC_S_0805_VAL_100R
```

### NON-ELECTRICAL DEVICES
These devices have no representation in the schematic and consist just of a package.
In constrast to electrical devices (see above) the prefix must be provided.

#### ADDING
For example in order to place a fiducial run:

```
board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac FD 5 5 0 top # prefix x y [rotation] [face]
```

To place a mounting hole run:
```
board led_driver add device $HOME/git/BEL/ET_component_library/packages/holes/hole_4.pac H 15 20 # prefix x y
```

#### MOVING
```
board led_driver move device FD1 relative 10 0
```

```
board led_driver move device FD1 absolute 95 75
```

#### ROTATING
```
board led_driver rotate device FD1 relative 10
```

```
board led_driver rotate device FD1 absolute 45
```

#### RENAMING
```
board led_driver rename device FD1 FD3
```

#### DELETING
```
board led_driver delete device FD3
```




## ASSEMBLY VARIANTS
```
schematic led_driver create variant low_cost
```

```
schematic led_driver describe variant low_cost "This is the low budget version."
```

```
schematic led_driver delete variant low_cost
```

```
schematic led_driver mount device low_cost R1 270R R_PAC_S_0805_VAL_270R
```

```
schematic led_driver mount device low_cost R1 270R R_PAC_S_0805_VAL_270R brightnesss_control
```

If a device is not to be mounted in a certain variant:
```
schematic led_driver unmount device low_cost R2
```

If a device is to be removed from the assembly variant:
```
schematic led_driver remove device low_cost R2
```

### VARIANTS OF SUBMODULES
If a submodule is to be instantiated with a certain assembly variant:
```
schematic led_driver mount submodule low_cost FLT1 fixed_frequency # variant parent, submod instance, submod variant
```

If a submodule is to be removed from the assembly variant:
```
schematic led_driver remove submodule low_cost FLT1
```

## NETS, NET SEGMENTS, STRANDS, LABELS

### DRAWING
```
schematic led_driver draw net reset_n 1 200 140 10 40 # net segment on sheet 1 from 230/150 to 10/40
```
Note: Creating a stub in a sloping net segment is not possible !

### JUNCTION
A junction can be placed in a net segment with this command:
```
schematic led_driver place junction 1 230 100 # sheet x y
```
Note: Junctions can not be placed in a sloping net segment !
<!--CS:
```
schematic led_driver delete junction 1 230 100 # sheet x y
```-->

### SIMPLE LABELS
A simple net label requires the sheet, x and y of the related net. 
There is no need to specify the net name. x/y must be a point that is run over by the net.
The next arguments are the relative distance from the position and rotation.
The rotation can either be 0 or 90 degree so that the label can be read from 
the front or from the right. Other angles are always "snapped" to 0 or 90 degree.
```
schematic led_driver place label 1 120 100 0 5 90 # sheet 1 120/100 relative 0/5  angle 90
```

A tag label indicates the signal direction and the next sheet where the net appears again.
The label can only be attached to a stub of a net. A stub is a net segment that has a dead
end. The segment must run in either horizonal or vertical direction. 
A tag label can not be attached to a sloped net segment.

### TAG LABELS
```
schematic led_driver place label 1 120 100 output # sheet 1 120/100 direction
```

### DELETING LABELS
```
schematic led_driver delete label 1 120 105 # sheet 1 x 120 y 105
```

### SCOPE
```
schematic templates/filter set scope GND global
```

```
schematic templates/adc set scope AGND local
```

### BOARD LAYER STACK
A layer is a compound of conductor and dielectric. The bottom conductor layer is always there 
and has the highest number. There is no command to add the bottom conductor layer.
Note that after adding or deleting a layer the layers are renumbered.
The top layer or further inner layers can be added via this command:
```
board led_driver add layer 0.12 0.2 # conductor thickness 0.12, dielectric thickness 0.2
```
All layers except the bottom layer can be deleted this way:
```
board led_driver delete layer 3
```


## ROUTING TRACKS
### FREETRACKS
Laying out:
```
board led_driver route freetrack 1 line 0.25 10 10 16 13 # layer 1, line, width 0.25, from 10/10 to 16/13
```
```
board led_driver route freetrack 1 arc 0.25 50 50 50 0 50 100 cw # layer 1, arc, width 0.25, center 50/50, from 50/0 to 50/100, clockwise
```

<!--Changing width. CS: Not implemented yet.
```
board led_driver width freetrack 1 12 10 1.2 # layer 1, segment in layer 1, crossing 12/10, new width 1.2
```-->


Ripping up:
```
board led_driver ripup freetrack 1 12 10 2 # layer 1, crossing point 12/10, accuracy 2
```


### NETS
#### Routing
Laying out tracks takes place in the layout:
```
board led_driver route net reset_n 1 line 0.25 10 10 16 13 # net reset_n, layer 1, line, width 0.25, line from 10/10 to 16/13
```
```
board led_driver route net reset_n 1 arc 0.25 50 50 50 0 50 100 ccw # net reset_n, layer 1, arc, width 0.25, center 50/50, from 50/0 to 50/100, counterclockwise
```

Route track from IC1 pad H7 in direction 45 degrees with length of 50mm:
```
board led_driver route net reset_n 1 line 0.25 IC1 H7 direction 45 50
```

<!--Route track from IC1 pad H7 in direction 45 degrees to the 5th grid line along x axis.
This operation depends on the drawing grid size. CS: Not implemented yet:
```
board led_driver route net reset_n 1 line 0.25 IC1 H7 direction 45 x 5
```-->

Route track from IC1 pad H7 to point 35/40:
```
board led_driver route net reset_n 1 line 0.25 IC1 H7 to 35 40
```

<!--Route track from IC1 pad H7 in x direction to the 5th grid line in along x axis:
This operation depends on the drawing grid size.  CS: Not implemented yet:
```
board led_driver route net reset_n 1 line 0.25 IC1 H7 to x 5
```-->

<!--Changing width. CS: Not implemented yet.
```
board led_driver width net reset_n 1 12 10 1.2 # layer 1, segment in layer 1, crossing 12/10, new width 1.2
```-->

#### Polygons / Fill Zones
```
board led_driver route net reset_n 1 poly 0.25 line 0 0 100 0 line 100 0 100 100 
arc 50 100 100 100 0 100 ccw line 0 100 0 0
```


##### Via Restring
Set the inner/outer resting width of vias.
```
board demo set via restring inner 0.22
```
```
board demo set via restring outer 0.2
```
Use the minimum resting width of vias as defined in design rules (default).
```
board demo set via restring inner dru
```
```
board demo set via restring outer dru
```

##### Via Drill Size
Set the drill size of vias.
```
board demo set via drill 0.6
```
Use the minimum drill size of vias as defined in design rules (default).
```
board demo set via drill dru
```

##### Placing Vias
Place a through-via in net RESET_N at x/y 10/14. 
```
board led_driver place via RESET_N 10 14
```
Place a blind-via in net RESET_N at x/y 10/14 drilled from
top to layer 3:
```
board led_driver place via RESET_N 10 14 blind top 3
```
Place a blind-via in net RESET_N at x/y 10/14 drilled from
bottom to layer 14:
```
board led_driver place via RESET_N 10 14 blind bottom 14
```
Place a buried-via at x/y 10/14 going from layer 5 to 8:
```
board led_driver place via RESET_N 10 14 buried 5 8
```



Ripping up:
```
board led_driver ripup net reset_n 1 12 10 2 # net reset_n, layer 1, crossing point 12/10, accuracy 2
```
<!--```
board led_driver ripup net reset_n 1 # net reset_n, all in layer 1
```
```
board led_driver ripup net reset_n # everything in net reset_n
```-->
<!--```
board led_driver ripup net # everything in all nets
```-->

<!--CS board led_driver ripup net reset_n line last
CS board led_driver ripup net reset_n arc last-->


<!--```
board led_driver move via 10 10 5-9 relative 1 0
```
```
board led_driver move via 10 10 5-9 absolute 11 10
```-->

<!--```
board led_driver delete via 10 10 5-9
```-->

To place a text in a conductor layer:
```
place text conductor 1 0.15 1 14 115 0 "SOME TEXT" # layer 1 (top), line width 0.15, size 1, x/y 14/115, rotation 0
```


#### ROUTE RESTRICT
Route restrict is a means to define areas in signal layers in the board where routing is not allowed.
In the examples below the term '[1,3,5-9]' addresses signal layers 1,3 and 5 through 9. 
NOTE: Within the term do not use whitespace characters.
```
board led_driver draw route_restrict [1,3,5-9] line 10 10 60 10  # line, from 10/10 to 60/10
```
```
board led_driver draw route_restrict [1,3,5-9] arc 50 50 0 50 100 0 cw # arc, center 50/50 from 50/0 to 50/100, clockwise
```
```
board led_driver draw route_restrict [1,3,5-9] circle 50 50 40 # circle, center 50/50 radius 40
```
```
board led_driver draw route_restrict [1,3,5-9] circle solid 50 50 40 # circle, solid, center 50/50 radius 40
```
```
board led_driver delete route_restrict 40 50 1 # crossing 40/50, accuracy 1
```

#### ROUTE RESTRICT
Via restrict is a means to define areas in signal layers in the board where vias are not allowed.
The commands are similar to the route restrict commands (see above) but the keyword
'route_restrict' is to be replaced by 'via_restrict'.

### DRAGGING
Dragging a net segment requires the net name, sheet and point of attack.
Dragging requires further-on an argument for relative/absolute movement followed by the x and y position.
If movement is relative, x and y are handled as difference.
In the example below the net motor_on is attacked on sheet 1 at x/y 120/100. The point 120/100
is close to start point. So the start point will be dragged by dx/dy of 0/10.
The decision whether the start or end point (or both) will be dragged depends on the point of attack.
If the start or end point is tied to any port, no dragging will be performed.
Dragging is not possible across sheets.
```
schematic led_driver drag segment motor_on 1 110 100 relative 0 -10 # point of attack 110 100 by dx dy
```
```
schematic led_driver drag segment motor_on 1 110 89 relative 0 10 # point of attack 120 100 by dx dy
```
```
schematic led_driver drag segment motor_on 1 100 80 absolute 120 90 # point of attack 100 80 to 120 90
```

### NAMING AND RENAMING
```
schematic led_driver rename net motor_on motor_on_n # on all sheets
```
```
schematic led_driver rename net motor_on motor_on_n 2 # all strands on sheet 2
```
```
schematic led_driver rename net motor_on motor_on_n 3 100 100 # only on sheet 3, only the strand starting at point 100/100
```

### DELETING

```
schematic led_driver delete segment motor_on 1 120 100 # segment on sheet 1 crossing point 120 100
```
```
schematic led_driver delete net motor_on_n # on all sheets
```
```
schematic led_driver delete net motor_on_n 2 # all strands on sheet 2
```
```
schematic led_driver delete net motor_on_n 3 100 100 # only on sheet 3, only the strand starting at point 100/100
```

## NETCHANGERS
A netchanger is a virtual device intended to
- tie two nets while maintaining the individual net names.
- provide a well defined point of interconnection between a module and its submodules.
- define the position in the layout where two nets are connected with each other.
- define the direction of net name inheritance (related to agile hardware development)

### PLACING
```
schematic led_driver add netchanger 1 90 100 0 # sheet 1 x/y 90/100 rotation 0
```

### MOVING
Moving a netchanger disconnects it from net old segments and places it at the given position.
If the netchanger ports end up where a net segment is, they become connected with the net.
```
schematic led_driver move netchanger 2 absolute 7 25 100
```
```
schematic led_driver move netchanger 2 relative 0 0 10
```
```
schematic led_driver rotate netchanger 2 relative 180
```

### DRAGGING
Dragging a netchanger drags the connected net segments along.
If the netchanger ports end up where a net segment is, they become connected with the net.
Dragging is not possible across sheets.
```
schematic led_driver drag netchanger 2 absolute 220 250
```
```
schematic led_driver drag netchanger 12 relative 20 -10
```

### DELETING
```
schematic led_driver delete netchanger 1
```

## SUBMODULES
Submodules are always instantiated from a generic module. On instantiation the submodule
is given an instance name. To edit a submodule the generic module (the file with extension *.mod)
must be edited.

### ADDING
```
schematic led_driver add submodule templates/filter.mod FLT1 1 300 90 30 30 # pos 300/90 size 30/30
```
```
schematic templates/filter add submodule templates/bypass.mod BY1 1 300 90 30 30
```

### RENAMING
```
schematic led_driver rename submodule FLT1 FILTER_1
```

### COPYING
```
schematic led_driver copy submodule FLT1 FLT2 1 300 90 # position sheet/x/y
```

### MOVING
As with units, moving a submodule disconnects it from attached nets.
The arguments for the move operation are sheet, x and y (see moving units):
```
schematic led_driver move submodule FLT1 absolute 4 210 100
```
```
schematic led_driver move submodule FLT1 relative 2 10 5
```

```
board led_driver move submodule FLT1 relative 10 -34.3 # dx/dy
```
```
board led_driver move submodule FLT1 absolute 34 90.5 # x/y
```

### ASSIGNING NAME OF GENERIC MODULE
```
schematic led_driver set submodule_file FLT1 templates/notch_filter.mod
```


### DRAGGING
Dragging a submodule drags the connected nets segments along.
If a submodule port ends up where a net segment is, it becomes connected with the net.
Dragging is not possible across sheets.
```
schematic led_driver drag submodule FLT1 absolute 210 100
```
```
schematic led_driver drag submodule FLT1 relative 10 5
```

### DELETING
```
schematic led_driver delete submodule FLT1
```

## SUBMODULE PORTS
The port of a submodule is named after a net which must exist inside the submodule.
The net becomes visible to the outside world if it is connected with a netchanger.
If a net inside the submodule is not connected with a netchanger, then it can not
be seen from outside.
To place a port of a submodule the module instance, the exported net, the port position
and the port direction must be given. See the follwing example command where
FLT1 is the instance, RF_OUT is the port name, 0/10 is the position and slave is the
direction.
The direction has nothing to do with energy or information flow. It can either be 
"master" or "slave". The direction is relevant for
netlist generation. A master port enforces its name onto the connected net whereas a
slave port propagates the name of the connected net into the submodule.

### ADDING
```
schematic led_driver add port FLT1 RF_OUT 0 10 slave 
```

### DELETING
```
schematic led_driver delete port FLT1 RF_OUT
```

### MOVING
Moving a port disconnects it from net old segments and places it at the given position.
If the port ends up where a net segment is, it becomes connected with the net.
```
schematic led_driver move port FLT1 RF_OUT absolute 110 260
```

```
schematic led_driver move port FLT1 RF_OUT relative 10 -2
```

### DRAGGING
Dragging a port drags the connected nets segments along.
If the port ends up where a net segment is, it becomes connected with the net.
```
schematic led_driver drag port FLT1 RF_OUT absolute 110 260
```

```
schematic led_driver drag port FLT1 RF_OUT relative 10 -2
```

## DATABASE INTEGRITY CHECK AND ERC
```
schematic led_driver check integrity
```

## MISC

### NESTED SCRIPTS
A script can be launched from inside a script. The domain does not matter 
as only the content of the called script is relevant.
The called script must exist in the project directory:
```
schematic led_driver execute script rename_nets.scr
```
```
board led_driver execute script rename_nets.scr
```

### SUBMODULE TREE
```
schematic led_driver build submodules_tree
```

## CAM DATA
The data required for manufacturing is always exported in the subdirectory "export/CAM".
There we have further separation between BOM, netlists and pick & place data.
On exporting these information ALL assembly variants are taken into account.
So you get BOM, netlists and p&p for each assembly variant defined. <br/>

Bill of material (BOM):
```
schematic led_driver make bom
```

Netlists:
```
schematic led_driver make netlists
```

Pick and place:
```
board led_driver make pnp
```

## BOARD OUTLINE
Board outlines (or contours) are milled in the PCB factory. For this reason there is
no line width parameter.
```
board led_driver draw outline line 0 0 160 0 # from 0/0 to 160/0
```
```
board led_driver draw outline arc 50 50 50 0 50 100 cw # center 50/50 from 50/0 to 50/100, clockwise
```
```
board led_driver draw outline circle 50 50 50 # center 50/50 radius 50
```
```
board led_driver delete outline 40 50 1 # crossing 40/50, accuracy 1
```

To place a text in the outline layer:
```
board demo place text outline 0.2 5 54 1 0 "SOME TEXT" # line width 0.2, size 5, x/y 54/1, rotation zero
```

## SILK SCREEN
This is (mostly) the white ink that the PCB house applies onto the board. 
NOTE: Whatever you place or draw in here affects the PCB manufacturing.
```
place text silkscreen top 0.15 1 14 115 0 "SOME TEXT" # top, line width 0.15, size 1, x/y 14/115, rotation 0
```
```
board led_driver draw silkscreen top line 2.5 0 0 160 0 # top, line, width 2.5mm, from 0/0 to 160/0
```
```
board led_driver draw silkscreen top arc 2.5 50 50 0 50 100 ccw # top, arc, width 2.5mm, center 50/50 from 50/0 to 50/100, counterclockwise
```
```
board led_driver draw silkscreen top circle 2.5 50 50 40 # top, circle, width 2.5mm, center 50/50 radius 40
```
Fill styles are: SOLID, HATCHED, CUTOUT.
```
board led_driver draw silkscreen top circle solid 50 50 40 # top, circle, solid, center 50/50 radius 40
```
<!--```
board led_driver draw silkscreen top circle cutout 50 50 40 # top, circle, cutout, center 50/50 radius 40
```-->
```
board led_driver draw silkscreen top circle hatched 50 50 40 0.5 1 # top, circle, hatched, center 50/50 radius 40, hatching line width 0.5, spacing 1
```
```
board led_driver delete silkscreen top 40 50 1 # crossing 40/50, accuracy 1
```

## ASSEMBLY DOCUMENTATION
Objects in the assembly documentation layer are drawn and deleted the like those in silkscreen (see above).
The difference is the 4th keyword 'assy'. An example to draw a line:
```
board led_driver draw assy top line 2.5 0 0 160 0 # top, line, width 2.5mm, from 0/0 to 160/0
```

## KEEPOUT
Objects in the keepout layers are drawn and deleted the like those in silkscreen (see above).
The difference is the 4th keyword 'keepout'. An example to draw a line:
```
board led_driver draw keepout top line 0 0 160 0 # top, line, from 0/0 to 160/0
```
Circles can either be filled or not. The fill style is always solid:
```
board led_driver draw keepout top circle filled 50 50 40 # circle, filled, center 50/50 radius 40
```
```
board led_driver draw keepout top circle 50 50 40 # circle, not filled, center 50/50 radius 40
```


## SOLDER STOP MASK
The solder stop mask defines the areas where the conductor plane (mostly copper) is exposed,
means where no stop laquer is applied.
NOTE: Whatever you draw here affects the PCB manufacturing.
Objects in the solder stop mask are drawn and deleted the like those in silkscreen (see above).
The difference is the 4th keyword 'stop'. An example to draw a line:
```
board led_driver draw stop top line 2.5 0 0 160 0 # top, line, width 2.5mm, from 0/0 to 160/0
```

## STENCIL
The stencil defines areas where solder cream is to be applied. These are mostly pads of SMT
devices. Solder cream is NOT applied to pads of THT devices. Solder cream can be applied only
where the solder stop mask is open (see above).
NOTE: Whatever you draw here affects the PCB manufacturing.
Objects in the stencil are drawn and deleted the like those in silkscreen (see above).
The difference is the 4th keyword 'stencil'. An example to draw a line:
```
board led_driver draw stencil top line 2.5 0 0 160 0 # top, line, width 2.5mm, from 0/0 to 160/0
```
