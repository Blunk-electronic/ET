.. _schematic:

****************
Schematic Editor
****************


.. |VNS| replace:: Verb-Noun key sequence:

.. |NI| replace:: Not intended

.. |SC| replace:: Short Cut:

.. |CS| replace:: Under Construction

+++++++++++++++++
Devices and Units
+++++++++++++++++

Commands and Verb-Noun key sequences
------------------------------------

#. Add device

	.. code-block::

		add device

	|VNS| a d

#. Show device

	.. code-block::

		show device R1

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

	|VNS| r n
		

#. Rotate value

	.. code-block::

		rotate value IC1 [A]

	|VNS| r v
		

#. Rotate purpose

	.. code-block::

		rotate purpose RN1 [A]

	|VNS| r p
	

		
+++++++++++++++++++
Nets and Net Labels
+++++++++++++++++++

Commands and Verb-Noun key sequences
------------------------------------

#. Draw net

	.. code-block::

	draw net [RESET_N]

	|VNS| d n

	change bend style: left mouse click or key b

	
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

	Rotate: right mouse click or key r
	
	
#. Place tag label

	|VNS| p L
	
	
#. Move label

	|VNS| m l

	
#. Delete label

	|VNS| del l


++++++++++++++++++
Displaying Objects
++++++++++++++++++

Commands and Verb-Noun key sequences
------------------------------------

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

Commands and Verb-Noun key sequences
------------------------------------

#. Zoom to fit

	.. code-block::

		zoom fit

	|VNS| |CS|

	
#. Zoom by level

	.. code-block::

		zoom level 10
	
	|VNS| |NI|

	
#. Zoom at

	.. code-block::

		zoom center 140 78 [level]

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

Commands and Verb-Noun key sequences
------------------------------------

#. Show a sheet

	.. code-block::

		show sheet 2

	|VNS| |NI|

+++++++
Modules
+++++++

Commands and Verb-Noun key sequences
------------------------------------

#. Show (or open) a module

	.. code-block::

		show module LED-driver [sheet]

	|VNS| |CS|
	
	
.. #. Create a module
.. 
.. 	.. code-block::
.. 
.. 		create module LED-driver
.. 
.. 	.. code-block::
.. 
.. 		create module templates/clock-generator
.. 		
.. 	|VNS| |CS|
		

#. Save a module

	- save with its own name
	
	.. code-block::

		save module

	|VNS| |CS|

	- save with a different name
	
	.. code-block::

		save module LED-driver_test

	|VNS| |NI|

	
	
#. Delete a module

	.. code-block::

		delete module [LED-driver]

	|VNS| |CS|


		
