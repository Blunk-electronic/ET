.. _board:

.. include:: placeholders.rst

*******************
Board Layout Editor
*******************

+++++++++++++++++++++++++++
Board Outline (Outer Edges)
+++++++++++++++++++++++++++

#. Drawing the Board Outline:

	
	Draw the outline of a EURO card with dimensions 160x100 mm:

	.. code-block::
		
		draw outline line 0 0 line 160 0 line 160 80 line 0 80


	|VNS| d l

	
		
#. Move Outline Segment

	To move a segment of the board contour:

	|VNS| m o
	

#. Delete Outline Segment

	To delete a segment of the board contour:

	|VNS| del o




++++++++++++++++++++++++++++++++
Holes in the Board (Inner Edges)
++++++++++++++++++++++++++++++++

#. Drawing Holes

	|VNS| d l
	

	This command draws a hole consisting of four lines:

	.. code-block::

		board demo draw hole line 45.5 0.5 line 48 0.5 line 48 8 line 47 8
	
	
	This command draws a circular hole at (32/5) with a radius of 6mm:
	
	.. code-block::
	
		board demo draw hole circle 32 5 6
	

	
#. Move Hole Segment

	To move a segment of a hole:

	|VNS| m o
	

#. Delete Hole Segment

	To delete a segment of a hole:

	|VNS| del o

	.. code-block::
	
		board demo delete hole 32 5
	
	
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




#. Copy Non-Electrical Device

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
++++++++++++++++++++++++++++++++++++++++++++++++++

Next to a package usually the name (like R2), the value (like 100R)
and the purpose (like "Brightness") is displayed via a placeholder.
The placeholder can be moved or rotated when the board drawing
requires it. Usually only one placeholder for name, value or purpose
is sufficient. However, for packages that require more area, more than
one placeholder is required. For this reason the placholders have
an index to identify them.
Placeholders can be part of the silkscreen or the assembly documentation.

By default placeholders are tied to the package and thus have a
position relative to the reference point of the parent package. This implies:
- If the package moves then the placeholder moves along.
- If the package rotates then the placeholder rotates about the 
  reference point of the package. The new rotation of the actual 
  placeholder content about its own origin is the sum of the new 
  rotation of the package and the rotation of the placeholder.
- If a console command is applied that performs a relative movement
  then the placeholder is anchored relatively to the package.

Alternatively the connection between placeholder and package can be
breached so that the placeholder gets an absolute position. This implies:
- If the package moves or rotates then the placeholder stays where it is.
- The placeholder can be moved independently of the package.
- If a console command is applied that performs an absolute positioning
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

	|VNS| t n
	
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





#. Draw Freetrack Segments

	This example draws a freetrack line in signal layer 1
	with linewidth 0.25mm from x/y 10/10 to x/y 16/13:

	.. code-block::

		route freetrack 1 line 0.25 10 10 16 13


	This example draws an arc freetrack in signal layer 1
	with linewidth 0.25, center at 50/50, from 50/0 to 50/100, clockwise:

	.. code-block::
		
		route freetrack 1 arc 0.25 50 50 50 0 50 100 cw


	This example draws a freetrack circle in signal layer 1
	with linewidth 0.25mm, center at 50/50, radius 4mm:

	.. code-block::

		route freetrack 1 circle 0.25 50 50 4






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

	The spacing defines the distance between fill lines
	if the fill style is hatched.

	.. code-block::
	
		set zone spacing 1.5

		
		
#. Set Linewidth of Border and Fill Stripes

	The border of the zone consists of segments (like lines, arcs, circle)
	and has a certain linewidth. When the outline
	of the zone is specified then it must be considered that the actual border
	runs right in the middle of the segments.
	
	.. code-block::
	
		set zone linewidth 0.2

		

#. Set Thermal Relief Maxium Gap Width

	This is the maximal space between terminal edge and fill zone.
	For spaces greater than gap_max no spoke will be generated:

	.. code-block::
	
		set zone relief gap_max 0.4

		

#. Set Thermal Relief Minimum Track Width

	This is the minimal width of the thermal relief spokes.
	It applies to ALL terminals connected with the zone.

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


	
#. Empty/Clear Fill Zone

	.. code-block::

		clear zone [net name] [net name] [net name] ... 

	|VNS| e z

	

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

	|VNS| d l
		
		
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
	
	|VNS| d l

	

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
	filled automatically according to its meaning.

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
		
		draw stopmask top line 1 70 55  60 66
		
	|VNS| d l
	
	.. code-block::
	
		draw stopmask top arc 2 100 100  50 100  150 100 ccw

	.. code-block::
	
		place text stopmask top 0.1 3 33 43 0 "STOP 1"

	.. code-block::
		
		draw stopmask top circle 2 100 100  20
		
	.. code-block::
		
		draw stopmask top zone line 52 0 line 60 0 line 60 4 line 52 4

		
		

#. Move

	|VNS| m t

		
		
#. Deleting Objects
		
	|VNS| del t
		
		
	.. code-block::
	
		delete stopmask top 50 100 10
		

		
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

		

#. Moving Objects

	|VNS| m e

	
		
#. Deleting Objects

	|VNS| del e

		
	.. code-block::
	
		delete stencil top 50 100 10


		
+++++++
Keepout
+++++++

In the keepout layers zones can be defined where no placement
of devices is allowed.
		
	.. code-block::
	
		draw keepout top zone line 0 70 line 10 70 line 10 80 line 0 80
		

	|VNS| d l
		

#. Moving Objects

	|VNS| m k

	
		
#. Deleting Objects

	|VNS| del k

		
		
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

		
	


++++++++++++++++++
Displaying Objects
++++++++++++++++++


#. Display Conductor Layers

	This requires to specify the affected conductor layer.

	.. code-block::

		display conductors 1 [on/off]



#. Display Vias

	.. code-block::

		display vias [on/off]





#. Display Silkscreen

	.. code-block::

		display silkscreen top [on/off]




#. Display Assembly Documentation

	.. code-block::

		display assy top [on/off]




#. Display Board Outline (Edges)

	.. code-block::

		display outline [on/off]




#. Display Keepout

	.. code-block::

		display keepout top [on/off]




#. Display Stencil (Solder Paste Opening)

	.. code-block::

		display stencil top [on/off]




#. Display Solder Stopmask

	.. code-block::

		display stopmask top [on/off]





#. Display Route Restrict

	This requires to specify the affected conductor layer.

	.. code-block::

		display restrict route 1 [on/off]





#. Display Via Restrict

	This requires to specify the affected conductor layer.

	.. code-block::

		display restrict via 1 [on/off]




	
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


	

+++++++++++++++++
Executing Scripts
+++++++++++++++++

#. Execute a Script

	see section Scripting in schematic documentation
