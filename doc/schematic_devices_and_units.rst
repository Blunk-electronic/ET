.. _schematic_devices_and_units:

.. include:: placeholders.rst


Devices and Units
-----------------


#. Add Device

	This example command adds a new electrical device to the module.
	The destination is sheet 1 on 100/140 with rotation 0 degree.
	The package variant is D:

	.. code-block::

		add device $HOME/git/BEL/ET_component_library/devices/active/logic/7400_ext.dev 1 100 140 0 D


	.. code-block::

		add device $HOME/git/BEL/ET_component_library/devices/active/logic/7400_ext.dev 1 100 140 0 D


	|VNS| a d

	While the unit is sticking to the cursor it can be rotated by pressing
	the r-key.

	As soon as the unit is dropped in the schematic drawing, the physical
	counterpart - the package - is dropped in the board drawing.

	Initially on placing the unit and the package in the drawing, the placeholders
	(for example for name, value  purpose) are copies of those specified 
	in the symbol and package model. This implies that the position of the
	placeholders in the drawing get NOT updated if they are changed
	in the package or symbol model afterward.



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


	
#. Showing and Finding Devices and Units:

	To show a device:
	
	|VNS| h d

	
	To locate the first unit of a device, use this command.
	The argument L1 .. L3 controls the level of information
	to be output. For L1 the status bar will contain some basic
	information. For L2 or L3 a window opens that contains a lot
	more information on the requested device.
	The schematic editor jumps to the sheet where the unit is.

	.. code-block::

		show device L1 R1



	|VNS| |NI|
	
		
	To locate the first unit of a device on the current sheet:
		
	.. code-block::

		show device L1 IC1 .

	|VNS| |NI|
	
		
	To locate an explicitly given unit of a device:
		
	.. code-block::

		show device L1 IC1 IO-BANK2
		
	|VNS| |NI|






	
#. Rename a Device

	.. code-block::

		rename device R1 R9
	
	|VNS| n d
	


#. Renumber Devices

	.. code-block::

		renumber devices 100
	







#. Delete a Device

	Devices which have a representation in the schematic
	and in the board drawing can be deleted only in the
	schematic editor.
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


	|VNS| g u





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






#. Mirror a Unit

	Mirroring is always along the y-axis of the unit.
	The operation only mirrors the body and the ports.
	Placeholders are left as they are because they are not
	necessarily affected. So the operator has to move the 
	placeholders if required.
	This command toggles between mirror and no-mirror:

	.. code-block::

		mirror unit IC1 C


	|VNS| i u






#. Set Package Variant

	.. code-block::

		set variant IC1 S_0805

	|VNS| s a
	
	


#. Set Value

	.. code-block::

		set value R1 100R
		
	|VNS| s v

	


#. Set Partcode

	.. code-block::

		set partcode R1 R_PAC_S_0805_VAL_100R

	|VNS| s c

	


#. Set Purpose

	.. code-block::

		set purpose R1 brightness

	|VNS| s p





	
Placeholders for Name, Value and Purpose
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Next to a unit usually the name (like R2), the value (like 100R)
and the purpose (like "Brightness") is displayed via a placeholder.
The placeholder can be moved or rotated when the schematic drawing
requires it. 
	
#. Move Name

	.. code-block::

		move name IC1 C relative -10 3

	|VNS| m p
		


#. Move Value

	.. code-block::

		move value IC1 C absoltue 100 115

	|VNS| m p
		


#. Move Purpose

	.. code-block::

		move purpose RN1 1 relative -10 3

	|VNS| m p
	


	
#. Rotate Name

	.. code-block::

		rotate name IC1 B vertical

	|VNS| r p
		


#. Rotate Value

	.. code-block::

		rotate value IC1 B horizontal

	|VNS| r p
		


#. Rotate Purpose

	.. code-block::

		rotate purpose R1 1 vertical

	|VNS| r p
