.. _netchangers:

.. include:: placeholders.rst

***********
Netchangers
***********

Netchangers are transition points between two nets.
Other CAE-tools refer to them as "net-ties". 
In System ET, a netchanger is more than just a makeshift to
connect two nets. It is a real physical point where
two nets meet, both in the schematic and in the board drawing.
The transition
point is defined in the schematic and in the board drawing.



++++++++++++++++
Schematic Editor
++++++++++++++++


#. Place a Netchanger


	This example places a netchanger on sheet 2 at x/y 60/30
	with rotation 0 degrees:

	.. code-block::
	
		demo add netchanger 2 60 30 0




#. Move a Netchanger

	This example moves netchanger 2 to the absolute
	position sheet 3 to x/y 30/0:

	.. code-block::

		demo move netchanger 2 absolute 3 30 0


	This example moves netchanger 2 relative by 
	10 sheets and by x/y 30/0:

	.. code-block::

		demo move netchanger 2 relative 10 30 0





#. Drag a Netchanger



#. Rotate a Netchanger

	The rotation of a netchanger is either 0 or 90 degrees.

	.. code-block::
	
		demo rotate netchanger 2 90



#. Delete a Netchanger




++++++++++++
Board Editor
++++++++++++
