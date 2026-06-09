.. _board_groups:

.. include:: placeholders.rst

Working with Groups of Objects
------------------------------


Define a Group
^^^^^^^^^^^^^^
	
	This example command defines a group of
	objects that are in the specified rectangular zone.
	The zone has its lower-left corner at x/y 5/10,
	extents 100mm to the right and 40mm up:

	.. code-block::
	
		define group 2  5 5  100 40


.. Additional parameters to specify a layer ?
.. define a circular group ?

Delete a Group
^^^^^^^^^^^^^^

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
