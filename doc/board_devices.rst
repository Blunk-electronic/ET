.. _board_devices:

.. include:: placeholders.rst


Devices
-------


There are electrical devices (having a counterpart in the schematic) and
non-electrical devices (without a representation in the schematic).

#. Showing and Locating Devices

	To locate a device, use this command.
	The argument L1 .. L3 controls the level of information
	to be output. For L1 the status bar will contain some basic
	information. For L2 or L3 a window opens that contains a lot
	more information on the requested device: 

	.. code-block::

		show device L2 IC20

	|VNS| h d




#. Add Non-Electrical Devices

	This command places a fiducial (or reference mark) with prefix FD
	at position 5/10:
	
	.. code-block::

		add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac FD 5 10
		add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac FD 5 10 45		
		add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac FD 5 10 45 bottom

	|VNS| a d

	Initially on placing the device in the drawing, the placeholders
	(for example for name, value  purpose) are copies of those specified 
	in the package model. This implies that the position of the
	placeholders in the board drawing get NOT updated if they are changed
	in the package model afterward.



#. Copy Non-Electrical Device

	The specified device must be a non-electrical device.

	.. code-block::

		copy device FD1 140 27

	|VNS| c d




#. Move Device

	.. code-block::

		move device X1 absolute -14 24
		move device X1 relative 1 0
		move device FD1 absolute -14 24
		move device MH1 relative 1 0

	|VNS| m d




	
#. Rotate Device

	.. code-block::

		rotate device R1
		rotate device R1 absolute 10
		rotate device R1 relative -45
		rotate device FD1 absolute 10
		rotate device MH1 relative -45

	|VNS| r d




	
	
#. Flip Device

	Flipping a device means to change the assembly side
	from top to bottom or vice versa. The package of the device
	is mirrored along its y-axis:

	This command toggles the assembly side:

	.. code-block::

		flip device C1


	This command sets an explicitly given assembly side:

	.. code-block::

		flip device C1 bottom
		flip device C1 top

	|VNS| l d

	
	The flip operation causes the placeholders for
	name, value and purpose to be reset to the default
	position (as specified in the package model).


	
	
#. Delete Non-Electrical Device

	Devices which appear only in the board drawing
	can be deleted only in the board editor. They are
	so called non-electrical devices because they 
	are not connected to any net.

	.. code-block::

		delete device FD1

	|VNS| del d

	Note: Electrical devices can only be deleted in the schematic !
	
	

	
#. Renaming a Non-Electrical Device

	.. code-block::

		rename device FD1 FD2

	|VNS| n d

	Note: Electrical devices can only be renamed in the schematic !
	




Placeholders for Name, Value and Purpose
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Next to a package usually the name (like R2), the value (like 100R)
and the purpose (like "Brightness") is displayed via a placeholder.
The placeholder can be moved or rotated when the board drawing
requires it. Usually only one placeholder for name, value or purpose
is sufficient. However, for packages that require more area, more than
one placeholder is required. For this reason the placholders have
an index to identify them.
Placeholders can be part of the silkscreen or the assembly documentation.

By default, placeholders are tied to the package and thus have a
position relative to the reference point of the parent package. This implies:
- If the package moves, then the placeholder moves along.
- If the package rotates, then the placeholder rotates about the 
  reference point of the package. The new rotation of the actual 
  placeholder content about its own origin is the sum of the new 
  rotation of the package and the rotation of the placeholder.
- If a console command is applied that performs a relative movement,
  then the placeholder is anchored relatively to the package.

Alternatively, the binding between placeholder and package can be
lifted so that the placeholder gets an absolute position. This can
be required when the placeholder must remain on the same spot, regardless
of the movement of the package. This implies:
- If the package moves or rotates, then the placeholder stays where it is.
- The placeholder can be moved independently of the package.
- If a console command is applied that performs an absolute positioning,
  then the connection with the package is severed.
- If the operator moves a placeholder via the GUI then the connection
  with the package is severed automatically. The connection can be
  restored by assigning a relative position.


#. Move Name

	This example command moves the name placeholder number 2 of IC1
	in the silkscreen on the top side of the board by -10/3:

	.. code-block::

		move name IC1 silkscreen top 2 relative -10 3

	|VNS| m p

	
		

#. Move Value

	.. code-block::

		move value IC1 silkscreen top 1 absolute 100 115

	|VNS| m p
		


#. Move Purpose

	.. code-block::

		move purpose RN1 assy bottom 2 relative -10 3

	|VNS| m p
	



#. Rotate Name

	This example command rotates the name placeholder number 2 of IC1
	in the silkscreen on the top side of the board by 45 degrees:

	.. code-block::

		rotate name IC1 silkscreen top 2 relative 45

	|VNS| r p
		


#. Rotate Value

	.. code-block::

		rotate value IC1 silkscreen top 1 absolute -45

	|VNS| r p
		


#. Rotate Purpose

	.. code-block::

		rotate purpose RN1 assy bottom 2 relative -10

	|VNS| r p




#. Reset Placholders

	The package model of a device specifies the default positions
	of placeholders. In order to restore the default 
	positions of all placeholders this command shall be 
	applied:

	.. code-block::

		restore placeholders R1

	This command also changes the anchor mode so that
	the placeholders are positioned relative to the package.

