.. _board_groups:

.. include:: placeholders.rst

Working with Groups of Objects
------------------------------


Define a Group
^^^^^^^^^^^^^^
	
	The easiest way to group objects is to type
	the verb noun sequence

	|VNS| e g

	then move the pointer to one corner of the area of
	interest, keep the left mouse button pressed, move to
	the other corner of the area and relase the button.

	The same can be achieved using the cursor: Move it
	to the first corner, press space key, move to the 
	second corner and press space key again.

	A third way to define a group is the commandline.
	This example command defines a group of
	objects that are in the specified rectangular zone.
	The zone has its lower-left corner at x/y 5/10,
	extents 100mm to the right and 40mm up:

	.. code-block::
	
		define group 5 5  100 40


.. Additional parameters to specify a layer ?
.. define a circular group ?

	Objects inside a group are highlighted.

	Objects of a group are copied to the clipboard
	so that they can be pasted multiple times using the
	paste command (see below).




Clear a Group
^^^^^^^^^^^^^

	Clearing a group means to deselect all objects
	that are in the current group.

	.. code-block::
	
		clear group

	|VNS| l g





Delete a Group
^^^^^^^^^^^^^^

	Deleting a group means to delete the objects which
	are in the current group.

	.. code-block::
	
		delete group

	|VNS| del g





Copy Group
^^^^^^^^^^

Single Copy
+++++++++++

	A single copy of a group can be made this way.
	A group can be copied to a given place.
	The copy is placed with an offset relative
	to the original group.

	This example command copies the group
	relative by an x/y offset of 10/20:

	.. code-block::
	
		copy group 10 20

	|VNS| c g



Multiple Copy
+++++++++++++

	If a group is to be copied multiple times then
	the clipboard must be used.
	These commands copy the current group into the clipboard.

	Without arguments the center of the current
	group is taken as reference point:

	.. code-block::
	
		copy group

	Via the short cut keys:

	|VNS| c g

	Now hit the l key to activate the clipboard. Then do
	a mouse click or press space at the cursor position. To
	set the reference point.
	



	The reference point can explicitly be specified by x and y
	coordinates:

	.. code-block::
	
		copy group 54 30



	To paste the content of the clipboard the
	absolute destination must be specified. The group
	will then be placed with its reference point at
	the specified destination:

	.. code-block::
	
		paste group 50 70

	|VNS| P g

	The content of the clipboard can be pasted any time. It will
	be overwritten only on a new copy-to-clipboard operation.




.. 
	Add Objects to Group
	^^^^^^^^^^^^^^^^^^^^


		This example command adds objects to a group
		that are in the zone around the given point.
		The zone radius is 2mm:

		.. code-block::
		
			add group 120 20 2


	Remove Objects from Group
	^^^^^^^^^^^^^^^^^^^^^^^^^

		.. code-block::
		
			remove group 120 20 2
