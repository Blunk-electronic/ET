.. _board_outline:

.. include:: placeholders.rst


Board Outline (Outer Edges)
---------------------------

Drawing the Board Outline
^^^^^^^^^^^^^^^^^^^^^^^^^

	Only the start point of lines is required.
	
	Draw the outline of a EURO card with dimensions 160x100 mm:

	.. code-block::
		
		draw outline line 0 0 line 160 0 line 160 80 line 0 80


	Arcs can also be segments of the outline. For an arc the first two
	fields identify x/y of the center. So in this example the arc center
	is at 130/140. The arc starts at 130/170, runs counter-clockwise and
	ends where the contour has started, at 130/110:

	.. code-block::

		draw outline line 130 110 line 160 110 line 160 170 line 130 170 arc 130 140  130 170 ccw


	|VNS| d l



	
		
Move Outline Segment
^^^^^^^^^^^^^^^^^^^^

	To move a segment of the board contour:

	|VNS| m o


	

Delete Outline Segment
^^^^^^^^^^^^^^^^^^^^^^

	To delete a segment of the board contour:

	|VNS| del o

	.. code-block::
	
		board demo delete outline 32 0 1
