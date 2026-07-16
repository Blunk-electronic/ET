.. _runmode:



		
Runmode
-------

By default ET launches a GUI. For automated processing
the GUI is not required. A command line switch for the 
runmode can be used. This example launches 
ET in headless mode, which means without any graphical user interface:

	.. parsed-literal::

		$ et --open-project my_project --runmode headless


Depending on the specified runmode, ET launches a dedicated GUI. The next example starts the
module edtior, which is default. In this mode you can edit the schematic and layout of a module:

	.. parsed-literal::

		$ et --open-project my_project --runmode module


Other runmodes currently under construction are:
- rig
- symbol
- package
- device
