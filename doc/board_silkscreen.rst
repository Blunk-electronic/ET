.. _board_silkscreen:

.. include:: placeholders.rst


Silkscreen
----------

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
