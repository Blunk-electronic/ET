.. _board_route_restrict:

.. include:: placeholders.rst


Route Restrict Objects
----------------------

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
