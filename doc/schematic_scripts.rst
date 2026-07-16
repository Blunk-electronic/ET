.. _schematic_scripts:

.. include:: placeholders.rst


		
Scripts
-------

Scripting allows to execute a batch of commands
in a comfortable way. This saves the operator from
perfoming a lot of erroneous and tedious tasks.


Execute a Script
^^^^^^^^^^^^^^^^

	ET has an internal script processor that reads and 
	executes a script file. The scripting feature allows manipulating
	designs without GUI:
	The command to launch a script from the internal console:

	.. code-block::

		execute script set_grid.scr


	Execute a script from outside vie the console of the operating
	system:

	.. parsed-literal::

		$ et --open-project my_et_project/ --script my_et_project/my_script.scr --save-project-as modified_project --log-level 2


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

