.. _board:

.. include:: placeholders.rst

*******************
Board Layout Editor
*******************

+++++++++++++
Board Outline
+++++++++++++

#. Drawing the Board Outline

	
	Draw the outline of a EURO card with dimensions 160x100 mm:

	.. code-block::
		
		draw outline line 0 0 line 160 0 line 160 80 line 0 80


+++++++++++++
Signal Layers
+++++++++++++

#. Adding a Signal Layer

	This example adds a signal layer with
	conductor thickness of 0.035mm and a dielectric
	thickness of 0.2mm:
	
	.. code-block::
	
		add layer 0.035 0.2
		

#. Deleting a Signal Layer

	This example deletes signal layer 2:
	
	.. code-block::
	
		delete layer 2

		
		
+++++++
Devices
+++++++


There are electrical devices (having a counterpart in the schematic) and
non-electrical devices (without a representation in the schematic).

#. Adding non-electrical Devices

	This command places a fiducial (or reference mark) with prefix FD
	at position 5/10:
	
	.. code-block::

		add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac FD 5 10
		


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
	
	

	
#. Renaming a Non-Electrical Device

	.. code-block::

		rename device FD1 FD2


	
	
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

#. Laying out Tacks of a Net

	|VNS| r n
	
	.. code-block::

		route net

	This example places a track of net GND in
	conductor layer 1 (topmost). 
	The track is a line segment of 2mm width.
	The segment starts at x/y 20/20 and ends at 50/20:
		
	.. code-block::

		route net GND 1 line 2  20 20  50 20

		

	
#. Ripping-up routed segments

	.. code-block::

		ripup net

	|VNS| i n

Select the ripup mode by pressing key m (single segment or all segments of the net).





++++
Vias
++++

#. Setting the Drill Size

	This example sets the drill size (diameter)
	of vias to 0.6mm:
	
	.. code-block::
	
		set via drill 0.6


		
#. Setting the Restring

	This example sets the width of the
	restring in inner signal layers to 0.25mm:
	
	.. code-block::
	
		set via restring inner 0.25


	This example sets the width of the
	restring in outer signal layers to 0.2mm:
	
	.. code-block::
	
		set via restring outer 0.2

		
		

		
#. Placing Vias

	|VNS| p v
	
	.. code-block::

		place via [net name]


	This example places a through-via connected
	with net GND at x/y position 50/20:
	
	.. code-block::
	
		place via GND 50 20


	This example places a blind-via connected
	with net GND at x/y position 50/20.
	The via is drilled from top down to inner layer 3:
	
	.. code-block::
	
		place via GND 50 20 blind top 3


	This example places a blind-via connected
	with net GND at x/y position 50/20.
	The via is drilled from bottom up to inner layer 15:
	
	.. code-block::
	
		place via GND 50 20 blind bottom 15
		
		
	This example places a buried-via connected
	with net GND at x/y position 50/20.
	The via connects the inner layers from 5 to 8:
	
	.. code-block::
	
		place via GND 50 20 buried 5 8

		
		
		
#. Moving Vias

	.. code-block::

		move via

	|VNS| m v

	
	
#. Deleting Vias

	.. code-block::

		delete via

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

#. Drawing Objects

	.. code-block::

		draw silkscreen top line [width] [from x] [from y] [to x] [to y]

	|VNS| d l


#. Move

	|VNS| m s
	
	

#. Deleting Objects

	|VNS| del s

	This example command deletes a silkscreen object
	on the top side of the board in the vicinity of point
	40/50. The search zone around the given point has
	a radius of 1mm:
	
	.. code-block::
	
		delete silkscreen top 40 50 1
		
	
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

	
