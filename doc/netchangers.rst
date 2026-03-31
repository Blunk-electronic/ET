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

	|VNS| a e


	While the netchanger is sticking to the cursor it can be rotated by pressing
	the r-key. Similar the direction can be changed via the t-key.

	Regarding the name of the netchanger (like N1, N2, N3, ...):
	The position of the name is fixed relatively to the body of the netchanger.
	There is is way to move the name.
	The prefix N is also fixed and can not be changed.



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

	|VNS| m e




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

	|VNS| g e





#. Rotate a Netchanger

	The rotation of a netchanger is either 0 or 90 degrees.

	.. code-block::
	
		rotate netchanger 2 90

	|VNS| r e



#. Copy a Netchanger

	This example command copies the netchanger 1 to
	sheet 4 x/y 100 40. The copy will be assigned the
	next available index:

	.. code-block::
	
		copy netchanger 1 4 100 40

	|VNS| c e




#. Set Direction

	The direction of a netchanger is relevant for agile 
	hardware development. This is not about the information
	or energy flow but about net names.
	The direction of a netchanger specifies the way
	connected nets are renamed when the two nets are
	to be merged into one net. The net that is connected with
	the master port enforces its name onto the net on
	the slave port.

	.. code-block::
	
		set netchanger 2 forward

	.. code-block::
	
		set netchanger 2 backward

	|VNS| s e



#. Delete a Netchanger

	.. code-block::
	
		delete netchanger 2

	|VNS| del e

	

	
	
#. Rename a Netchanger

	This example command renames netchanger 1 to 14:
	
	.. code-block::
	
		rename netchanger 1 14

	|VNS| n e





#. Showing and Finding a Netchanger:

	The schematic editor jumps to the sheet where the netchanger
	is.

	|VNS| h e

	.. code-block::

		show netchanger 44

	


	
++++++++++++
Board Editor
++++++++++++

#. Move a Netchanger

	|VNS| m e

	.. code-block::

		move netchanger 1 absolute 210 100"


	.. code-block::

		move netchanger 1 relative -10 5.2




#. Set the Signal Layer

	A netchanger connects the tracks of two nets
	in a certain signal layer.

	Thus example command sets the signal
	layer of netchanger 1 to layer 3:

	.. code-block::

		set netchanger 1 layer 3

