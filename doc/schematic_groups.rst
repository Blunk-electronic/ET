.. _schematic_groups:

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
	objects that are on sheet 2 in the specified rectangular zone.
	The zone has its lower-left corner at x/y 5/10,
	extents 100mm to the right and 40mm up:

	.. code-block::
	
		define group 2  5 5  100 40

.. Additional parameters to specify a layer ?
.. define a circular group ?

	Objects inside a group are highlighted. As long as
	the group mode is active, further areas can be selected
	and thus added to an existing group.


Clear a Group
^^^^^^^^^^^^^

	Clearing a group means to deselect all objects
	that are in the current group.

	.. code-block::
	
		clear group

	|VNS| l g

	Additionally, on pressing the ESC-Key, the existing
	group can be cleared.



Delete a Group
^^^^^^^^^^^^^^

	Deleting a group means to delete the objects which
	are in the current group.

	.. code-block::
	
		delete group

	|VNS| del g



Move a Group
^^^^^^^^^^^^

	A group can be moved across the current sheet
	or to another sheet. The coordinates to be provided
	are relative. So in the example below, the group
	will be moved by 0 sheets, x 100mm and y 20mm:

	.. code-block::
	
		move group 0 100 20





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



	Copy Group
	^^^^^^^^^^

		Copy group to sheet 4 x/y 30/40

		.. code-block::
		
			copy group 4  30 40


	Move, Drag group

