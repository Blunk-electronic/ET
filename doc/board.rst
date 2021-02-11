.. _board:

.. include:: placeholders.rst

**************+++**
Board Layout Editor
*******************

++++
Vias
++++

Commands and Verb-Noun key sequences
------------------------------------

#. Place via

	.. code-block::

		place via [net name]

	|VNS| p v



++++++++++++++++++++++++++++
Polygons in Conductor Layers
++++++++++++++++++++++++++++

Commands and Verb-Noun key sequences
------------------------------------

#. (Re)fill or pour polygons

	.. code-block::

		fill polygon [net name] [net name] [net name] ... 

	|VNS| f p


	

+++++
Texts
+++++

Commands and Verb-Noun key sequences
------------------------------------

#. Place text

	.. code-block::

		place text

	|VNS| p t


	
+++++++
Modules
+++++++

Commands and Verb-Noun key sequences
------------------------------------

#. Show (or open) a module

.. 	.. code-block::
.. 
.. 		show module LED-driver [sheet]

	|VNS| |NI|

	|SC| Previous/Next module: F11/F12
	

				
#. Save a module

	- save with its own name
	
.. 	.. code-block::
.. 
.. 		save module

	|VNS| |NI| Use the common shortcut CTRL-S instead.

	- save with a different name
	
.. 	.. code-block::
.. 
.. 		save module LED-driver_test
.. 
.. 	|VNS| |NI|

	
