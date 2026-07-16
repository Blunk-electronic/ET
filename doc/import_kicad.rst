.. _import_kicad:

.. include:: placeholders.rst


Import Kicad Projects and Component Models
==========================================


Import Kicad Project
--------------------

To import a single KiCad V5 design into a native project 'my_et_project' run this command: 

	.. parsed-literal::

		$ et --import-format kicad_v5 --import-project my_kicad_project/


Optionally provide a log level for debugging:

	.. parsed-literal::
		
		$ et --import-format kicad_v5 --import-project my_kicad_project/ --log-level 2


ET creates in the current working directory a folder named "ET/et_import" where you find the now native project.
Inside the project you will find a directory named 'libraries' where the imported component libraries
associated with the project live. 
There is also an import report where log messages can be found. See "ET/reports". Depending on the log level this report
contains more or less debug information.

NOTE: Currently all kinds of text styles (normal, italic, bold) are ignored and replaced
by ET internal hard coded fonts.



