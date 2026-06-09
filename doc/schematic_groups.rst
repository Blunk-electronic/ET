.. _schematic_groups:

.. include:: placeholders.rst

Working with Groups of Objects
------------------------------


Define a Group
^^^^^^^^^^^^^^

	This example command defines a group of
	objects that are on sheet 2 in the specified rectangular zone.
	The zone has its lower-left corner at x/y 5/10,
	extents 100mm to the right and 40mm up:

	.. code-block::
	
		define group 2  5 5  100 40

	Objects inside a group are highlighted.

.. Additional parameters to specify a layer ?
.. define a circular group ?


Clear a Group
^^^^^^^^^^^^^

	Clearing a group means to deselect all objects
	that are in the current group.

	.. code-block::
	
		clear group



Delete a Group
^^^^^^^^^^^^^^

	Deleting a group means to delete the objects which
	are in the current group.

	.. code-block::
	
		delete group


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

