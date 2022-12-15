.. _board:

.. include:: placeholders.rst

**************+++**
Board Layout Editor
*******************


+++++++
Devices
+++++++

Commands and Verb-Noun key sequences
------------------------------------

#. Move device

	.. code-block::

		move device X1 absolute -14 24
		move device X1 relative 1 0

	|VNS| m d

	



+++++++++++++++++++++
Ratsnest and Airwires
+++++++++++++++++++++

Commands and Verb-Noun key sequences
------------------------------------

#. Update ratsnest

	.. code-block::

		update ratsnest

	|VNS| u r

	
	

++++
Vias
++++

Commands and Verb-Noun key sequences
------------------------------------

#. Place via

	.. code-block::

		place via [net name]

	|VNS| p v



++++++++++++++++++++++++++++++
Fill Zones in Conductor Layers
++++++++++++++++++++++++++++++

Commands and Verb-Noun key sequences
------------------------------------

#. Define outline

	.. code-block::

		route net gnd 1 zone line 23 0 line 232 0 line 232 33 line 100 33
		route net gnd 1 zone line 25 25 arc 30 25 27 25 ccw line 30 28 line 25 28
		route net gnd 1 zone circle 35 30 3
		
		
		
#. Set width of fill lines

	.. code-block::
	
		set zone width 0.2

		
		
#. (Re)fill or pour zones

	.. code-block::

		fill zone [net name] [net name] [net name] ... 

	|VNS| f z


	

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

	
