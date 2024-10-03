.. _schematic:

.. include:: placeholders.rst

****************
Schematic Editor
****************



+++++++++++++++++
Devices and Units
+++++++++++++++++


#. Add device

	.. code-block::

		add device

	|VNS| a d

	
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

	
#. Rename device

	.. code-block::

		rename device R1 R9
	
	|VNS| n d
	
	
#. Delete unit

	.. code-block::

		delete unit IC1 [B]

	|VNS| del u
	

#. Drag unit

	.. code-block::

		drag unit IC1 [A]

	|VNS| d u


#. Invoke unit

	.. code-block::

		invoke unit IC1 [B]

	|VNS| i u

	
#. Move unit

	.. code-block::

		move unit IC1 [A]

	|VNS| m u
	

#. Rotate unit

	.. code-block::

		rotate unit IC1 [A]

	|VNS| o u


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

#. Draw net

	.. code-block::

	draw net [RESET_N]

	|VNS| d n

	change bend style: left mouse click or key b


#. Showing and finding nets

	- To show a net:

	|VNS| h n
	
	- To find a net use this command:
	
	.. code-block::

		show net RESET_N

	|VNS| |NI|
	
	- To find a net on the current sheet:
	
	.. code-block::

		show net RESET_N .

	|VNS| |NI|

	
#. Rename net

	- A strand on the current sheet:
	
	|VNS| n s


	- All strands on the current sheet:
	
	|VNS| n n


	- All strands on a specific sheet:

	.. code-block::

		rename net RESET_N RST_N 5

	
	- All strands on all sheets:

	.. code-block::

		rename net RESET_N RST_N
	
	|VNS| n N


#. Drag net segment

	|VNS| g n

	
#. Delete net segment

	|VNS| del n

	
#. Delete whole net

	.. code-block::

		delete net RESET_N
	
	
#. Place simple label

	|VNS| p l

	Rotate: right mouse click or key o
	
	
#. Place tag label

	|VNS| p L
	
	
#. Move label

	|VNS| m l

	
#. Delete label

	|VNS| del l


	
	
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
				
				
#. Save a module

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


		
