.. _schematic:

.. include:: placeholders.rst

****************
Schematic Editor
****************



+++++++++++++++++
Devices and Units
+++++++++++++++++


#. Add Device

	This example command adds a new electrical device to the module.
	The destination is sheet 1 on 100/140 with rotation 0 degree.
	The package variant is D:

	.. code-block::

		add device $HOME/git/BEL/ET_component_library/devices/active/logic/7400_ext.dev 1 100 140 0 D


	.. code-block::

		add device $HOME/git/BEL/ET_component_library/devices/active/logic/7400_ext.dev 1 100 140 0 D


	|VNS| a d





#. Copy Device

	When a device is copied, then all the properties of the original
	device are also copied. Units are not copied. Instead the first
	available unit is added to the schematic (similar to when a new device
	is added).
	This example command creates a copy of IC1 and places the first
	available unit on sheet 2 210/100 with zero rotation:

	.. code-block::

		schematic led_driver copy device IC1 2 210 100 0

	|VNS| c d


	
#. Showing and finding devices and units:

	- To show a device:
	
	|VNS| h d

	
	- To find the first unit of device, use this command:

	.. code-block::

		show device R1

	|VNS| |NI|
	
		
	- To find the first unit of a device on the current sheet:
		
	.. code-block::

		show device IC1 IO-BANK2 .

	|VNS| |NI|
	
		
	- To find an explicit unit of a device:
		
	.. code-block::

		show device IC1 IO-BANK2
		
	|VNS| |NI|






	
#. Rename a Device

	.. code-block::

		rename device R1 R9
	
	|VNS| n d
	






#. Delete a Device

	A whole device (incl. all its units) can be deleted
	via this simple command:

	.. code-block::

		delete device IC12

	|VNS| del d

	



#. Delete a Unit

	.. code-block::

		delete unit IC1 C

	|VNS| del u


	



#. Drag a Unit

	Dragging a unit means to move it about the sheet
	along with the connected net segments.
	If the ports of the unit end up where 
	a net segment is, then they are connected with the net.

	This example command drags the unit C of IC1 to the
	absolute position 100/140:

	.. code-block::

		drag unit IC1 C absolute 100 140

	This example command drags the unit C of IC1 by the
	10/4:

	.. code-block::

		drag unit IC1 C relative 10 4


	|VNS| d u





#. Fetch a Unit

	If a device is already used in the schematic and further units
	of it are to be placed, then the fetch command should be used.
	The example fetches from IC1 the unit C and places it on sheet 1
	at x/y 70/100 with a rotation of -90 degrees:

	.. code-block::

		fetch unit IC1 C 1 70 100 -90

	|VNS| f u

	While the unit is being moved it can be rotated by pressing
	the r-key.

	



#. Move a Unit

	Moving a unit disconnects it from net segments and places it 
	at the given position. If the ports of the unit end up where 
	a net segment is, then they are connected with the net.

	.. code-block::

		move unit IC1 [A]

	A unit can be moved relatively from one sheet to 
	another and by some distance dx and dy:

	.. code-block::

		schematic led_driver move unit R1 1 relative -1 2 4


	A unit can be moved absolutely to a sheet and to
	a position x/y:

	.. code-block::

		schematic led_driver move unit R1 1 absolute 2 210 100

	|VNS| m u



	

#. Rotate a Unit

	.. code-block::

		rotate unit IC1 C absolute 90

	.. code-block::

		rotate unit IC1 C relative -90


	|VNS| r u





#. Set package variant

	.. code-block::

		set variant IC1 [S_0805]

	|VNS| s a
	
	


#. Set value

	.. code-block::

		set value R1 [100R]
		
	|VNS| s v

	


#. Set partcode

	.. code-block::

		set partcode R1 [R_PAC_S_0805_VAL_100R}

	|VNS| s p

	


#. Set purpose

	.. code-block::

		set purpose R1 [brightness]

	|VNS| s p





	
Placeholders for Name, Value, Partcode and Purpose
++++++++++++++++++++++++++++++++++++++++++++++++++

	
#. Move name

	.. code-block::

		move name IC1 [A]

	|VNS| m n
		

#. Move value

	.. code-block::

		move value IC1 [A]

	|VNS| m v
		

#. Move purpose

	.. code-block::

		move purpose RN1 [A]

	|VNS| m p
	
	
#. Rotate name

	.. code-block::

		rotate name IC1 [A]

	|VNS| o n
		

#. Rotate value

	.. code-block::

		rotate value IC1 [A]

	|VNS| o v
		

#. Rotate purpose

	.. code-block::

		rotate purpose RN1 [A]

	|VNS| o p
	


	
+++++++++++++++++++
Nets and Net Labels
+++++++++++++++++++

#. Drawing Net Segments

	|VNS| d n

	change bend style: left mouse click or key b

	The equivalent to this shortcut is the command

	.. code-block::

		draw net


	
	To specify the net name type this command:

	.. code-block::

		draw net [RESET_N]


	To draw a segment of a net on a given sheet starting
	at a certain point and ending at a certain point:
		
	Example: Draw a segment of net GND on sheet 1 starting
	at x/y 30/100 and ending at 50/100:
		
	.. code-block::

		draw net GND  1  30 100  50 100

	* A net segment is rejected if

	#. it causes a loop inside a strand,
	#. it causes a connection between two different nets.




#. Showing and Finding Nets

	- To show a net:

	|VNS| h n
	
	- To find a net use this command:
	
	.. code-block::

		show net RESET_N

	|VNS| |NI|
	

	
	
	
#. Renaming a Net

	- A single particular strand on the current sheet:
	
	|VNS| n s


	- All strands on the current sheet:
	
	|VNS| n n


	- A specific strand on a specific sheet:

	.. code-block::

		rename net RESET_N RST_N 2 50 90



	- All strands on a specific sheet:

	.. code-block::

		rename net RESET_N RST_N 5

	
	- All strands on all sheets:

	.. code-block::

		rename net RESET_N RST_N
	
	|VNS| n N


	
	
#. Drag net segment

	When a net segment is being dragged, then other connected segments are
	dragged along. If the ends of the net are dragged to a port of a
	unit, netchanger or submodule, then a connection between port and
	net segment is established.
	
	A segment which is connected with a port can not be dragged away 
	anymore.

	If the segment meets another net, then these two nets will NOT be connected.

	This example command searches for a net segment
	on sheet 1 that crosses the position 80/100 in a zone of
	2mm radius. It drags the attacked end of the segment by 10/0mm.
	
	If more than one segment has been found in the given zone,
	then the first of them will be selected.


	.. code-block::

		drag segment 1 80 100 2 relative 10 0
	
	|VNS| g s

	
	
	
#. Delete net segment

	This example command searches for a net segment
	on sheet 1 that crosses the position 80/100 in a zone of
	2mm radius. If a segment exists there, then it will be deleted.
	If more than one segment exists on the given spot, then
	the first that has been found will be deleted.

	.. code-block::
		
		delete segment 1 80 100 2

	|VNS| del s


	
	
#. Delete a strand of a net

	This example command searches for a strand
	on sheet 1 that crosses the position 80/100 in a zone of
	2mm radius. If a strand exists there, then it will be deleted.
	If more than one strand exists on the given spot, then
	the first that has been found will be deleted.

	.. code-block::
		
		delete strand 1 80 100 2

	|VNS| del t

	
	
#. Delete a net on a certain sheet

	.. code-block::

		delete net RESET_N 2

		
	|VNS| del n
	
	
	
#. Delete a whole net on all sheets

	.. code-block::

		delete net RESET_N
	
	|VNS| del N
	

	
	
#. Placing Net Labels

	A net label is just a text next
	to a net segment to indicate the net name.
	The label will be orientated automatically so that
	it is readable either from the front or from the right.
	The spacing between net segment and label is fixed.

	|VNS| p l

	
	Example: Place a net label on sheet 1 at x/y 30/100:
		
	.. code-block::

		demo place net_label  1  30 100

	
	
#. Move Net Label

	A net label can be moved along a net segment.
	Independed of the the grid settings, the label will
	always be at a fixed distance away from the segment.

	|VNS| m l

	
	
	
#. Delete Net Label

	|VNS| del l



	
#. Placing Net Off-Page-Connectors

	An off-page-connector is a text inside a box that
	is connected with a net segment. The box
	indicates the nature of the connector (input, output,
	bidir, tristate, passive).
	Inside the box the net name is shown.

	|VNS| p c

	Example: Place a net connector on sheet 1 at x/y position 50/100.
	The connector type is 'passive':
	
	.. code-block::
	
		demo place net_connector  1  50 100  passive
	
	
#. Delete Net Off-Page-Connectors

	|VNS| del c

	
	
++++++++++++++++++
Displaying Objects
++++++++++++++++++


#. Display grid

	.. code-block::

		display grid [on/off]

	|VNS| |NI|


#. Display nets

	.. code-block::

		display nets [on/off]

	|VNS| |NI|

	
#. Display ports

	.. code-block::

		display ports [on/off]

	|VNS| |NI|
	
	
#. Display device names

	.. code-block::

		display names [on/off]

	|VNS| |NI|

	
#. Display device values

	.. code-block::

		display values [on/off]

	|VNS| |NI|

	
#. Display device purposes

	.. code-block::

		display purposes [on/off]

	|VNS| |NI|


	
	
++++++++++++++++++++++++
Zooming, Cursor and Grid
++++++++++++++++++++++++

#. Zoom to fit

	Zooms to all objects on the canvas.

	.. code-block::

		zoom fit

	|VNS| |CS|

	
#. Zoom by level

	Sets the zoom factor.
	
	.. code-block::

		zoom level 10
	
	|VNS| |NI|

	
#. Zoom at

	Places the cursor at the given position and zooms on the cursor.
	
	.. code-block::

		zoom center 140 78 10

	|VNS| |NI|
	

#. Positioning the Cursor

	.. code-block::

		position cursor absolute 25 30
	
	|VNS| |NI|

	.. code-block::

		position cursor relative -5 0
	
	|VNS| |NI|


#. Grid

	.. code-block::

		set grid 2 2

	.. code-block::

		set grid 10 5
	
	|VNS| |NI|

	
++++++
Sheets
++++++

#. Show a sheet

	.. code-block::

		show sheet 2

	|VNS| |NI|

	
	
	
+++++++
Modules
+++++++


#. Show (or open) a module

	.. code-block::

		show module LED-driver [sheet]

	|VNS| |NI|

	|SC| Previous/Next module: F11/F12
	
	
#. Create a module

	.. code-block::

		create module MOTOR-driver

	.. code-block::

		create module templates/clock-generator

	.. note:: If the module already exists, nothing happens. The existing module
			will NOT be touched..
			
	.. note:: The module to be created must be in the current project
			directory or in a subdirectory thereof. Creating a module across several 
			directory levels like ../other_project/MOTOR-driver is not possible.
				
		

		
#. Save a Module

	Save with its own name:
	
	.. code-block::

		save module

	|VNS| |NI| Use the common shortcut CTRL-S instead.

	
	Save as:
	
	.. code-block::

		save module LED-driver_test

	|VNS| |NI|

	
	
#. Delete a module

	.. code-block::

		delete module [LED-driver]

	|VNS| |NI| (and dangerous ...)



		
+++++++++++++++++
Executing Scripts
+++++++++++++++++

Scripting allows to execute a batch of commands
in a comfortable way. This saves the operator from
perfoming a lot of erroneous and tedious tasks.


#. Execute a Script

	The command to launch a script from the console:

	.. code-block::

		execute script set_grid.scr


#. The commands inside the Script

	The commands inside the script are quite similar to
	those to be entered at the console. The small difference is
	that two additional statements specify the domain and the targeted
	module.

	.. code-block::

		schematic blood-sample-analyzer set grid spacing 20 20


#. Nested Scripts

	A script can be launched from inside a script.

	.. code-block::

		schematic blood-sample-analyzer execute script set_grid.scr

