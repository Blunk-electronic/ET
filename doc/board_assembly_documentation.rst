.. _board_assembly_documentation:

.. include:: placeholders.rst


Assembly Documentation
----------------------

Drawing Objects
^^^^^^^^^^^^^^^

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


		
Moving Objects
^^^^^^^^^^^^^^

	|VNS| m a


	
Deleting Objects
^^^^^^^^^^^^^^^^

	.. code-block::
	
		delete assy top 100 100  2

	|VNS| del a

