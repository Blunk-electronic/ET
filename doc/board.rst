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


	|VNS| d o
	
		
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
	
		

	
#. Deleting Track Segments

	.. code-block::

		delete track

	|VNS| del c

	
	.. code-block::

		delete track gnd 1 10 15 2
		
Select the delete mode by pressing key m (single segment or all segments of the net).




#. Deleting Freetrack Segments

	.. code-block::

		delete freetrack 2 10 15 2

	|VNS| del c	
		


#. Moving Track Segments

	|VNS| m c


#. Moving Freetrack Segments
	
	|VNS| m c
	


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


#. Define Outline

	Fill zone connected with a net:

	.. code-block::

		route net gnd 1 zone line 23 0 line 232 0 line 232 33 line 100 33
		route net gnd 1 zone line 25 25 arc 30 25 27 25 ccw line 30 28 line 25 28
		route net gnd 1 zone circle 35 30 3
	
	
	Floating fill zone connected with no net:
	
		.. code-block::
	
		route freetrack 1 zone line 18 9 line 30 9 line 30 13 line 18 13
		

		
#. Move Zone Segment

	To move a segment of a zone contour:

	|VNS| m c
	
		
		
#. Set Fill Style

	.. code-block::

		set zone fill solid


	.. code-block::

		set zone fill hatched

		NOTE: This is currently under construction.

		
#. Set Spacing

	.. code-block::
	
		set zone spacing 1.5

		
		
#. Set Linewidth of Border and Fill Stripes

	.. code-block::
	
		set zone linewidth 0.2

		

#. Set Thermal Relief Maxium Gap Width

	This is the maximal space between pad and fill zone.
	For spaces greater than gap_max no spoke will be generated:

	.. code-block::
	
		set zone relief gap_max 0.4

		

#. Set Thermal Relief Minimum Track Width

	This is the minimal width of the thermal relief spokes.
	It applies to ALL pads connected with the zone.

	.. code-block::
	
		set zone relief width_min 0.3


		
#. Set Connection Style

	.. code-block::
	
		set zone connection thermal


	.. code-block::
	
		set zone connection solid

		

#. Set Isolation

	This is the space between the zone and foreign conductor objects:

	.. code-block::
	
		set zone isolation 0.4

		

#. Set Priority

	.. code-block::
	
		set zone priority 2


		
#. Set Easing Style

	.. code-block::
	
		set zone easing style none

	.. code-block::
	
		set zone easing style chamfer
		
	.. code-block::
	
		set zone easing style fillet

	.. code-block::
	
		set zone easing radius 0.5
		

		
		
		
#. (Re)fill or pour zones

	.. code-block::

		fill zone [net name] [net name] [net name] ... 

	|VNS| f z


	
#. Clear Fill Zone

	.. code-block::

		clear zone [net name] [net name] [net name] ... 

	|VNS| c z

	

++++++++++++++++++++++
Route Restrict Objects
++++++++++++++++++++++

Route restrict objects can be used to define areas where
no tracks are allowed or where tracks are not allowed to 
pass through. These objects do not have a linewidth because
they represent a barrier.


#. Drawing Lines

	This command draws a line in layer 1 starting at (50;30) and
	ending at (70;30):

	.. code-block::

		draw route_restrict [1] line 50 30 70 30
		
		
	As restrictions can be applied to many layers
	at the same time, this command draws the line
	in layers 1, 3 and 5 to 9:

	.. code-block::

		draw route_restrict [1,3,5-9] line 50 30 70 30
		


#. Drawing Arcs

	This command draws an arc in layer 1 having its center at (60;40),
	starting at (50;40), ending at (70;40) in 
	counter-clockwise direction:

	.. code-block::

		board demo draw route_restrict [1] arc 60 40  50 40  70 40 ccw


		
#. Drawing Circles

	This command draws a circle in layer 1
	having its center at (60;40)
	with a radius of 5mm:

	.. code-block::

		board demo draw route_restrict [1] circle 60 40  5

		
		
#. Drawing Zones

	.. code-block::

		draw route_restrict [1] zone line 0 0 line 100 0 line 100 100 line 0 100

	|VNS| d z
		
		
#. Deleting

	.. code-block::
	
		delete route_restrict 50 30 10
		
		
	
++++++++++++++++++++
Via Restrict Objects
++++++++++++++++++++


In the via restrict layers areas can be specified
where no vias a allowed.


	.. code-block::

		draw via_restrict [1] zone line 0 0 line 100 0 line 100 100 line 0 100
		
	|VNS| d z

	

++++++++++++++++++++++
Texts and Placeholders
++++++++++++++++++++++


#. Place Text

	.. code-block::

		place text

	|VNS| p x

	
	This command places a text in conductor layer 3.
	The linewidth is 0.15mm, the text size 1mm.
	The position is at (20;5) with a rotation of 0 degrees.

	.. code-block::
		
		place text conductor 3 0.15 1 20 5 0 "Dummy Text"
	
	
#. Move Text

	|VNS| m x

	

#. Place Placeholder

	Placeholders have a meaning such as 
	COMPANY, CUSTOMER, PARTCODE, DRAWING_NUMBER,
	ASSEMBLY_VARIANT, PROJECT, MODULE, REVISION.
	
	Once a placeholder has been placed, its content will be
	filled automaticall according to its meaning.

	This command places a placeholder in conductor layer 3.
	The linewidth is 0.15mm, the text size 1mm.
	The position is at (20;5) with a rotation of 0 degrees.

	.. code-block::
		
		place placeholder conductor 1 0.15 1 20 5 0 module
	

	.. code-block::
		
		place placeholder silkscreen top 0.15 1 20 5 0 partcode


	.. code-block::
		
		place placeholder assy top 0.15 1 20 5 0 partcode


	.. code-block::
		
		place placeholder stencil top 0.15 1 20 5 0 partcode

		
	
++++++++++++++++++++++
Assembly Documentation
++++++++++++++++++++++

#. Drawing Objects

	.. code-block::

		draw assy top line [width] [from x] [from y] [to x] [to y]

	|VNS| d l
	
	
	.. code-block::
		
		draw assy top line 1  27 31   30 31
	
	.. code-block::
		
		draw assy top arc 1  100 100   50 100   150 100  cw

	.. code-block::
	
		draw assy top circle 1  100 100  20

	.. code-block::
	
		draw assy top zone line 26 25 line 28 25 line 28 33 line 26 33

	|VNS| d z
	
		
#. Moving Objects

	|VNS| m a


	
#. Deleting Objects

	.. code-block::
	
		delete assy top 100 100  2

	|VNS| del a

	
	

++++++++++
Silkscreen
++++++++++

#. Drawing Objects

	.. code-block::

		draw silkscreen top line [width] [from x] [from y] [to x] [to y]

	|VNS| d l
	
	
	.. code-block::
		
		draw silkscreen top line 1  27 31   30 31
	
	.. code-block::
		
		draw silkscreen top arc 1  100 100   50 100   150 100  cw

	.. code-block::
	
		draw silkscreen top circle 1  100 100  20
	
	.. code-block::
	
		board demo draw silkscreen top zone line 26 25 line 28 25 line 28 33 line 26 33
	
	|VNS| d z
	
	
#. Texts

	.. code-block::
		
		place text silkscreen top 0.5 3 33 33 0 "SILK 1"
	

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
		
	
	
++++++++
Stopmask
++++++++

#. Drawing Objects

	.. code-block::
		
		draw stop top line 1 70 55  60 66
		
	|VNS| d l
	
	.. code-block::
	
		draw stop top arc 2 100 100  50 100  150 100 ccw

	.. code-block::
	
		place text stop top 0.1 3 33 43 0 "STOP 1"

	.. code-block::
		
		draw stop top circle 2 100 100  20
		
	.. code-block::
		
		draw stop top zone line 52 0 line 60 0 line 60 4 line 52 4
		
	|VNS| d z
		
		

#. Move

	|VNS| m t

		
		
#. Deleting Objects
		
	|VNS| del t
		
		
	.. code-block::
	
		delete stop top 50 100 10
		

		
+++++++
Stencil
+++++++

#. Drawing Objects

	.. code-block::
		
		draw stencil top line 1 70 55  60 66
	
	.. code-block::
	
		draw stencil top arc 2 100 100  50 100  150 100 ccw

	.. code-block::
		
		draw stencil top circle 2 100 100  20
		
	.. code-block::
	
		draw stencil top zone line 0 70 line 10 70 line 10 80 line 0 80

		
		
#. Deleting Objects
		
	.. code-block::
	
		delete stencil top 50 100 10


		
+++++++
Keepout
+++++++

In the keepout layers zones can be defined where no placement
of devices is allowed.
		
	.. code-block::
	
		draw keepout top zone line 0 70 line 10 70 line 10 80 line 0 80
		
		
		
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

	
