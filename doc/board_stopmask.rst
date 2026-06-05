.. _board_stopmask:

.. include:: placeholders.rst


Stopmask
--------

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
		
