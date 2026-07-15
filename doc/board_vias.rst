.. _board_vias:

.. include:: placeholders.rst



Vias
----

Setting the Drill Size
^^^^^^^^^^^^^^^^^^^^^^

	This example sets the drill size (diameter)
	of vias to 0.6mm:
	
	.. code-block::
	
		set via drill 0.6


		
Setting the Restring
^^^^^^^^^^^^^^^^^^^^

	This example sets the width of the
	restring in inner signal layers to 0.25mm:
	
	.. code-block::
	
		set via restring inner 0.25


	This example sets the width of the
	restring in outer signal layers to 0.2mm:
	
	.. code-block::
	
		set via restring outer 0.2

		
		

		
Placing Vias
^^^^^^^^^^^^

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

		
		
		
Moving Vias
^^^^^^^^^^^

	.. code-block::

		move via

	|VNS| m v

	
	
Deleting Vias
^^^^^^^^^^^^^

	.. code-block::

		delete via

	|VNS| del v
