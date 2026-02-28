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
	
		add netchanger 2 60 30 0




#. Move a Netchanger

	Moving a netchanger means that the existing
	connections with net segments are severed and
	possible new connectons estabilshed.

	This example moves netchanger 2 to the absolute
	position sheet 3 to x/y 30/0:

	.. code-block::

		move netchanger 2 absolute 3 30 0


	This example moves netchanger 2 relative by 
	10 sheets and by x/y 30/0:

	.. code-block::

		move netchanger 2 relative 10 30 0





#. Drag a Netchanger

	Dragging a netchanger means that the existing
	connections with net segments are kept and thus
	the affected net segments are dragged along.
	Possible new connectons are estabilshed.
	Dragging from one sheet to another is not possible.

	This example drags netchanger 2 to the absolute
	position x/y 30/0:

	.. code-block::

		drag netchanger 2 absolute 30 0


	This example drags netchanger 2 relative by x/y 30/0:

	.. code-block::

		drag netchanger 2 relative 30 0







#. Rotate a Netchanger

	The rotation of a netchanger is either 0 or 90 degrees.

	.. code-block::
	
		rotate netchanger 2 90





#. Set Direction

	.. code-block::
	
		set netchanger 2 forward

	.. code-block::
	
		set netchanger 2 backward




#. Delete a Netchanger

	.. code-block::
	
		delete netchanger 2



++++++++++++
Board Editor
++++++++++++
