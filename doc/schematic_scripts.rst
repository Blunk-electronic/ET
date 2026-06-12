.. _schematic_scripts:

.. include:: placeholders.rst


		
Scripts
-------

Scripting allows to execute a batch of commands
in a comfortable way. This saves the operator from
perfoming a lot of erroneous and tedious tasks.


Execute a Script
^^^^^^^^^^^^^^^^

	The command to launch a script from the console:

	.. code-block::

		execute script set_grid.scr


The commands inside the Script
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

	The commands inside the script are quite similar to
	those to be entered at the console. The small difference is
	that two additional statements specify the domain and the targeted
	module.

	.. code-block::

		schematic blood-sample-analyzer set grid spacing 20 20


Nested Scripts
^^^^^^^^^^^^^^

	A script can be launched from inside a script.

	.. code-block::

		schematic blood-sample-analyzer execute script set_grid.scr

