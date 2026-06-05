.. _board_tracks:

.. include:: placeholders.rst



Ratsnest and Airwires
---------------------


#. Update ratsnest

	.. code-block::

		update ratsnest

	|VNS| u r


	
	

Routing, laying out Tracks and Traces
-------------------------------------


#. Showing and Finding Nets

	To show a net:

	|VNS| h n
	
	To find a net use this command:
	
	.. code-block::

		show net RESET_N

	|VNS| |NI|



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
	

	This example draws a track starting at R1
	Terminal 1 and ending at 50/20:

		
	.. code-block::

		route net GND 1 line 2  R1 1  50 20


	This example draws a track starting at R1
	Terminal 1 and ending at 50/20:

		
	.. code-block::

		route net GND 1 line 2  R1 1  50 20


	This example draws a track from IC1 pad H7 in 
	x direction to the 5th grid line in along x axis:
	
	.. code-block::

		route net NET_1 1 line 0.25 R1 1 to x 5


	This command draws an arc. Center at 50/50,
	start at 50/0, and at 50/100 and CCW direction:

	.. code-block::

		route net GND 1 arc 0.25 50 50 50 0 50 100 ccw


#. Deleting Track Segments

	.. code-block::

		delete track

	|VNS| del c

	
	.. code-block::

		delete track gnd 1 10 15 2
		
	Select the delete mode by pressing key m (single segment or all segments of the net).





Freetracks
----------



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
