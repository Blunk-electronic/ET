# SYSTEM ET SCRIPT PROCESSOR COMMAND SET

## BASICS
Scripting is an important feature of advanced CAE tools. It allows:
- repeating and automating of tedious or error prone tasks.
- software testing without graphical user interfaces
- generating schematics, layouts and device models via third party software

A script in general is just a list of things to do by the CAE system. This list
is a plain ASCII file. Comments start with #. 
The two letters "CS" indicate a "construction site" where things are not finished yet or intended for the future.
The script commands have a domain-verb-noun structure.
Their meaning should be obvious in most cases. Additional explanation is provided in comments
if required. The domain-verb-noun structure is: DOMAIN MODULE VERB NOUN PARAMETER[S]

The domain tells where the operation is to take place. Domains are:
- project
- schematic
- board (or layout)
- symbol, package, device (CS)

The measurement system is METRIC. All dimensions are in millimeters (mm).
All angles and rotations are in degrees (1/360).

The concept is most easily to understand by examples. The module to be designed or edited is named "led_driver":

## MODULES
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

## DEVICES AND UNITS
A device is something like a resistor (consisting of a schematic symbol and a package drawing).
A single resistor contains only one symbol:

### ADDING
```
schematic led_driver add device $HOME/git/BEL/ET_component_library/devices/passive/resistors/R.dev 1 110 10 0 S_0805 # sheet 1 x y rotation [variant]
```

A quad-NAND IC like the famous 7400 offers four symbols (named A..D):

This command places the first available unit at sheet 1 x/y 250/150 with rotation 90 degrees. The last optional argument (D) is the package variant.
```
schematic led_driver add device $HOME/git/BEL/ET_component_library/devices/active/logic/7400.dev A 1 250 150 90 D # sheet 1 x y rotation [variant]
```

This command invokes from IC1 the unit P and places it at sheet 4 at x/y 60/90 with zero rotation:
```
schematic led_driver invoke unit IC1 P 4 60 90 0 # device unit sheet x y rotation
```

### COPYING

```
schematic led_driver copy device R1 2 210 100 0
```

### RENAMING

```
schematic led_driver rename device R1 R9
```

```
schematic led_driver renumber devices 100
```


### SETTING VALUE, PURPOSE, PARTCODE
```
schematic led_driver set value R1 2k2
```
```
schematic led_driver set purpose R1 "brightnesss_control"
```
```
schematic led_driver set partcode R1 R_PAC_S_0805_VAL_100R
```

### DELETING

```
schematic led_driver delete device R1
```

```
schematic led_driver delete unit R1 1
```

```
schematic led_driver delete device IC1 C
```

### MOVING
#### in the schematic
Moving a unit disconnects it from net old segments and places it at the given position.
If the units ports end up where a net segment is, they become connected with the net.
```
schematic led_driver move unit R1 1 absolute 2 210 100 # R1 unit sheet x y
```
```
schematic led_driver move unit R1 1 relative -1 2 4 # IC1 unit B sheet dx dy
```

```
schematic led_driver rotate unit R1 1 absolute -270
```
```
schematic led_driver rotate unit R1 1 relative 90
```

#### in the board
```
board led_driver move device R1 absolute 140 45.2
```
```
board led_driver move device R1 relative 4.5 0
```

```
board led_driver rotate device R1 absolute 70
```
```
board led_driver rotate device R1 relative -4
```

```
board led_driver flip device R1 top
```
```
board led_driver flip device R1 bottom
```


### DRAGGING
Dragging a unit drags the connected net segments along.
If the units ports end up where a net segment is, they become connected with the net.
Dragging is not possible across sheets.

```
schematic led_driver drag unit R1 1 absolute 220 100 # R1 unit x y
```

```
schematic led_driver drag unit R1 1 relative 10 0 # IC1 unit B x y
```

### MOVING PLACEHOLDERS FOR NAME, VALUE AND PURPOSE

```
schematic led_driver move name R1 1 absolute 2 210 100 # R1 unit sheet x y
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
```
schematic led_driver rotate name R1 1 0
```
```
schematic led_driver rotate value R1 1 90
```
```
schematic led_driver rotate purpose R1 1 0
```

### SETTING VALUE, PURPOSE, PARTCODE
```
schematic led_driver set value R1 2k2
```
```
schematic led_driver set purpose R1 "brightnesss_control"
```
```
schematic led_driver set partcode R1 R_PAC_S_0805_VAL_100R
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

### JUNCTION
```
schematic led_driver place junction 1 230 100 # sheet x y
```
<!--CS:
```
schematic led_driver delete junction 1 230 100 # sheet x y
```-->

### SIMPLE LABELS
```
schematic led_driver place label 1 120 100 0 5 90 # sheet 1 120/100 relative 0/5  angle 90
```

### TAG LABELS
```
schematic led_driver place label 1 120 100 0 -5 90 output # sheet 1 120/100 relative 0/-5 angle 90 direction
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

## ROUTING TRACKS
### FREETRACKS
Laying out:
```
board led_driver route freetrack 1 line 0.25 10 10 16 13 # layer 1, line, width 0.25, from 10/10 to 16/13
```
```
board led_driver route freetrack 1 arc 0.25 50 50 50 0 50 100 # layer 1, arc, width 0.25, center 50/50, from 50/0 to 50/100
```

Ripping up:
```
board led_driver ripup freetrack 1 12 10 2 # layer 1, crossing point 12/10, accuracy 2
```


### NETS
Laying out:
```
board led_driver route net reset_n 1 line 0.25 10 10 16 13 # net reset_n, layer 1, line, width 0.25, line from 10/10 to 16/13
```
```
board led_driver route net reset_n 1 arc 0.25 50 50 50 0 50 100 # net reset_n, layer 1, arc, width 0.25, center 50/50, from 50/0 to 50/100
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
board led_driver place via 10 10 0.25 5-9 reset_n # pos. 10/10, diameter 0.25, layers 5-9, net_name reset_n
```-->
<!--```
board led_driver move via 10 10 5-9 relative 1 0
```
```
board led_driver move via 10 10 5-9 absolute 11 10
```-->

<!--```
board led_driver delete via 10 10 5-9
```-->


### DRAGGING
Dragging a net segment requires the net name, sheet and point of attack.
Dragging requires further-on an argument for relative/absolute movement followed by the x and y position.
If movement is relative, x and y are handled as difference.
In the example below the net motor_on is attacked on sheet 1 at x/y 120/100. The point 120/100
is close to start point. So the start point will be dragged by dx/dy of 0/10.
The decision whether the start or end point (or both) will be dragged depends on the point of attack.
If the start or end point is tied to any port, no dragging will be performed.
Absolute dragging with the center of the segment is not supported.
Dragging is not possible across sheets.
```
schematic led_driver drag segment motor_on 1 110 100 relative 0 -10 # point of attack 110 100 by dx dy
```
```
schematic led_driver drag segment motor_on 1 110 89 relative 0 10 # point of attack 120 100 by dx dy
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
board led_driver draw outline arc 50 50 50 0 50 100 # center 50/50 from 50/0 to 50/100
```
```
board led_driver draw outline circle 50 50 50 # center 50/50 radius 50
```
```
board led_driver delete outline 40 50 1 # crossing 40/50, accuracy 1
```

