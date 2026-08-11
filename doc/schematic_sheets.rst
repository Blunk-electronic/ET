.. _schematic_sheets:

.. include:: placeholders.rst


Sheets
------

For schematic sheets tree categories exist: PRODUCT, DEVELOPMENT and ROUTING.
In the title block of the drawing frame, the category is always displayed.


Add a new sheet
^^^^^^^^^^^^^^^

	This command creates a new sheet and appends
	it to the existing sheets. By default a sheet of 
	category PRODUCT is created.

	.. code-block::

		add sheet

	|VNS| |NI|


	The sheet category can be specified so that a sheet
	of the desired category is created right away:

	.. code-block::

		add sheet development


	.. code-block::

		add sheet routing




Show a sheet
^^^^^^^^^^^^

	.. code-block::

		show sheet 2

	|VNS| |NI|


	
Set Sheet Category
^^^^^^^^^^^^^^^^^^

	.. code-block::

		set sheet 2 development


	.. code-block::

		set sheet 2 routing


	.. code-block::

		set sheet 2 product





Delete a Sheet
^^^^^^^^^^^^^^

	This example command deletes a sheet incl. everything
	on it:

	.. code-block::

		delete sheet 2
