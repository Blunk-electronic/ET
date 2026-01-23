.. _module_operations:

.. include:: placeholders.rst

*****************
Module Operations
*****************




#. Show (or open) a module

	.. code-block::

		show module LED-driver [sheet]

	|VNS| |NI|

	Via the function keys you can advance from one module to the next:

	|SC| Previous/Next module: F11/F12
	




	
#. Create a module

	created a module can currently be done in the schematic domain only:

	.. code-block::

		create module MOTOR-driver

	.. code-block::

		create module templates/clock-generator

	.. note:: If the module already exists, nothing happens. The existing module
			will NOT be touched..
			
	.. note:: The module to be created must be in the current project
			directory or in a subdirectory thereof. Creating a module across several 
			directory levels like ../other_project/MOTOR-driver is not possible.
				
		

		
#. Save a Module

	Save with its own name:
	
	.. code-block::

		save module

	|VNS| |NI| Use the common shortcut CTRL-S instead.

	
	Save as:
	
	.. code-block::

		save module LED-driver_test

	|VNS| |NI|

	
	

#. Delete a module

	Deleting a module can currently be done in the schematic domain only:

	.. code-block::

		delete module [LED-driver]

	|VNS| |NI| (and dangerous ...)



