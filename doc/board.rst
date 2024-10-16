.. _board:

.. include:: placeholders.rst

*******************
Board Layout Editor
*******************


+++++++
Devices
+++++++


There are electrical devices (having a counterpart in the schematic) and
non-electrical devices (without a representation in the schematic).

#. Move electrical device

	.. code-block::

		move device X1 absolute -14 24
		move device X1 relative 1 0

	|VNS| m d


#. Move non-electrical device

	.. code-block::

		move device FD1 absolute -14 24
		move device MH1 relative 1 0

	|VNS| m n


	
#. Rotate electrical device

	.. code-block::

		rotate device R1 absolute 10
		rotate device R1 relative -45

	|VNS| o d

#. Rotate non-electrical device

	.. code-block::

		rotate device FD1 absolute 10
		rotate device MH1 relative -45

	|VNS| o n


	
	
#. Flip/mirror electrical device

	.. code-block::

		flip device C1 bottom
		flip device C1 top

	|VNS| l d


#. Flip/mirror non-electrical device

	.. code-block::

		flip device FD1 bottom

	|VNS| l n

	
#. Delete non-electrical device

	.. code-block::

		delete device FD1

	|VNS| del n

	Note: Electrical devices can only be deleted in the schematic !
	
	
	
	
+++++++++++++++++++++
Ratsnest and Airwires
+++++++++++++++++++++


#. Update ratsnest

	.. code-block::

		update ratsnest

	|VNS| u r


	
	
+++++++++++++++++++++++++++++++++++++
Routing, laying out Tracks and Traces
+++++++++++++++++++++++++++++++++++++

#. Drawing tracks of a net

	.. code-block::

		route net

	|VNS| r n


	
#. Ripping-up routed segments

	.. code-block::

		ripup net

	|VNS| i n

Select the ripup mode by pressing key m (single segment or all segments of the net).





++++
Vias
++++


#. Place via

	.. code-block::

		place via [net name]

	|VNS| p v


#. Move via

.. 	.. code-block::
.. 
.. 		place via [net name]

	|VNS| m v

	
#. Delete via

.. 	.. code-block::
.. 
.. 		place via [net name]

	|VNS| del v

	

++++++++++++++++++++++++++++++
Fill Zones in Conductor Layers
++++++++++++++++++++++++++++++


#. Define outline

	.. code-block::

		route net gnd 1 zone line 23 0 line 232 0 line 232 33 line 100 33
		route net gnd 1 zone line 25 25 arc 30 25 27 25 ccw line 30 28 line 25 28
		route net gnd 1 zone circle 35 30 3
		
		
		
#. Set width of fill lines

	.. code-block::
	
		set zone width 0.2

		
		
#. (Re)fill or pour zones

	.. code-block::

		fill zone [net name] [net name] [net name] ... 

	|VNS| f z



++++++++++++++++++++++
Route Restrict Objects
++++++++++++++++++++++


#. Drawing lines

	This command draws a line in layer 1 with a
	linewidth of 0.2mm starting at (50;30) and
	ending at (70;30):

	.. code-block::

		draw route_restrict [1] 0.2 line 50 30 70 30
		
		
	As restrictions can be applied to many layers
	at the same time, this command draws the line
	in layers 1, 3 and 5 to 9:

	.. code-block::

		draw route_restrict [1,3,5-9] 0.2 line 50 30 70 30
		


#. Drawing arcs

	This command draws an arc in layer 1 with a
	linewidth of 0.2mm having its center at (60;40),
	starting at (50;40), ending at (70;40) in 
	counter-clockwise direction:

	.. code-block::

		board demo draw route_restrict [1] 0.2 arc 60 40  50 40  70 40 ccw


		
#. Drawing lines

	This command draws a circle in layer 1 with a
	linewidth of 0.2mm having its center at (60;40)
	with a radius of 5mm:

	.. code-block::

		board demo draw route_restrict [1] 0.2 circle 60 40  5

	
	
++++++++++++++++++++
Via Restrict Objects
++++++++++++++++++++


Objects in via restrict layers are to be drawn the same
way as route restrict objects (see above). The only difference
is to use the noun via_restrict instead as shown in this example:


	.. code-block::

		draw via_restrict [1] 0.2 line 50 30 70 30
		
		

	

+++++
Texts
+++++


#. Place text

	.. code-block::

		place text

	|VNS| p t

	
	This command places a text in conductor layer 3.
	The linewidth is 0.15mm, the text size 1mm.
	The position is at (20;5) with a rotation of 0 degrees.

	.. code-block::
		
		place text conductor 3 0.15 1 20 5 0 "Dummy Text"
	
	
	
	
++++++++++++++++++++++
Assembly Documentation
++++++++++++++++++++++

#. Draw

	.. code-block::

		draw assy top line [width] [from x] [from y] [to x] [to y]

	|VNS| d l


#. Move

	|VNS| m a


#. Delete

	|VNS| del a

	
	

++++++++++
Silkscreen
++++++++++

#. Draw

	.. code-block::

		draw silkscreen top line [width] [from x] [from y] [to x] [to y]

	|VNS| d l


#. Move

	|VNS| m s
	

#. Delete

	|VNS| del s

	

	
+++++++++++++
Drawing Frame
+++++++++++++


Position of the Drawing Frame
-----------------------------

#. Absolute Coordinates

	.. code-block::

		move frame absolute -150 -105


#. Relative Coordinates

	.. code-block::

		move frame relative -10 -10

		
	
	
+++++++
Modules
+++++++

#. Show (or open) a module

	.. code-block::

		show module LED-driver [sheet]

	|VNS| |NI|

	|SC| Previous/Next module: F11/F12
	

				
#. Save a module

	Save with its own name:
	
	.. code-block::

		save module

	|VNS| |NI| Use the common shortcut CTRL-S instead.

	Save as:
	
	.. code-block::

		save module LED-driver_test

	|VNS| |NI|

	
