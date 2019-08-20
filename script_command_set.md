# SYSTEM ET SCRIPT COMMAND SET

## BASICS
Scripting is an important feature of advanced CAE tools. It allows:
- repeating and automating of tedious or error prone tasks.
- software testing without graphical user interfaces
- generating schematics, layouts and device models via third party software

A script in general is just a list of things to do by the CAE system. This list
is a plain ASCII file. Comments start with #. The script commands have a verb-noun structure.
Their meaning should be obvious in most cases. Additional explanation is provided in comments
if required. The verb-noun structure is: DOMAIN MODULE VERB NOUN PARAMETER[S]

The two letters "CS" indicate a "construction site" where things are not finished yet or intended for the future.


The concept is easy to understand by examples. The module to be designed or edited is named "led_driver":

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

## NETS, NET SEGMENTS, STRANDS, LABELS

### DRAWING
```
schematic led_driver draw net reset_n 1 200 140 10 40 # net segment on sheet 1 from 230/150 to 10/40
```

### JUNCTION
```
schematic led_driver place junction 1 230 100 # sheet x y
```
CS:
```
schematic led_driver delete junction 1 230 100 # sheet x y
```

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
is given an instance name. 

### ADDING
```
schematic led_driver add submodule templates/filter.mod FLT1 1 300 90 30 30 # pos 300/90 size 30/30
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
