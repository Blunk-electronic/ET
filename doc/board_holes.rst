.. _board_holes:

.. include:: placeholders.rst


Holes in the Board (Inner Edges)
--------------------------------

Drawing Holes
^^^^^^^^^^^^^

	|VNS| d l
	

	This command draws a hole consisting of four lines:

	.. code-block::

		board demo draw hole line 45.5 0.5 line 48 0.5 line 48 8 line 47 8
	
	
	This command draws a circular hole at (32/5) with a radius of 6mm:
	
	.. code-block::
	
		board demo draw hole circle 32 5 6
	


	
Move Hole Segment
^^^^^^^^^^^^^^^^^

	To move a segment of a hole:

	|VNS| m o

	


Delete Hole Segment
^^^^^^^^^^^^^^^^^^^

	To delete a segment of a hole:

	|VNS| del o

	.. code-block::
	
		board demo delete hole 32 5 2
	
