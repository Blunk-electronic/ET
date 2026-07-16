.. _project:

.. include:: placeholders.rst


Project Configuration
=====================

|SysName| does not feature a main window or a control panel (|CS|).
Instead the project configuration is done via the command line
and with a text editor (like Joe, Kate, Mousepad, ...).


Create Project
--------------

Change into the directory where the project is to live.

	.. parsed-literal::
		
		$ et --create-project |MYP|

This command creates the project directory |MYP| with some files in it.

.. warning:: An already existing project of the same name will be deleted without warning !

.. note:: The project to be created must be a subdirectory of the 
		current working directory. Creating a project across several 
		directory levels like ../ecad/my_et_project/ is not possible.





Open Project
------------

	.. parsed-literal::

		$ et --open-project |MYP|
		
Since there is no module in this new bare project, the schematic
and board editor window opens an untitled module. All you see here
for the moment is the default drawing frame.

Optionally the file name of the generic module to be opened can be added:

	.. parsed-literal::
		
		$ et --open-project my_et_project/ --module my_et_project/power_supply.mod

or just

	.. parsed-literal::

		$ et --open-project my_et_project/ --module power_supply.mod


The module file must exist in the project directory.

further-on a sheet can be specified so that the desired sheet gets opened right away:

	.. parsed-literal::
		
		$ et --open-project my_et_project/ --module power_supply.mod --sheet 3


NOTE: The project to be opened must be a child directory of the 
current working directory. Opening a project across several directory levels like ecad/my_et_project/
is not possible.

A log level can also be passed:

	.. parsed-literal::
	
		$ et --open-project my_et_project/ --log-level 2





Conventions
-----------

The conventions file is the place where file where prefixes,
units of measurement and other things are defined. It is not mandatory.
This step can be omitted. If so, lots of design checks wil not be performed.
Change into the root directory of your projects and generate a conventions
file with this command:

	.. code-block::
	
		$ et --make-conventions conventions.txt

This file can now be found in the root directory of your projects.
Edit it according to your customs.
Assign the conventions file to the module by editing the rig configuration file.

An example file can be seen here <https://github.com/Blunk-electronic/ET_training/blob/master/conventions.txt>

Opening a project includes syntax checking. See the report for details.

..
	If a conventions file is available run:

	$ et --conventions conventions.txt --open-project /home/user/ecad/my_et_project/ --log-level 2


	The project can also be saved with a different name at a different place:


	$ et --conventions conventions.txt --open-project /home/user/ecad/my_et_project/ --save-project-as /home/user/tmp/eval --log-level 2


