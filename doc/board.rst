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

There are electrical devices (having a counterpart in the schematic) and
non-electrical devices (without a representation in the schematic).

#. Move electrical device

	.. code-block::

		move device X1 absolute -14 24
		move device X1 relative 1 0

	|VNS| m d


#. Move non-electrical device

	.. code-block::

		move device FD1 absolute -14 24
		move device MH1 relative 1 0

	|VNS| m n


	
#. Rotate electrical device

	.. code-block::

		rotate device R1 absolute 10
		rotate device R1 relative -45

	|VNS| r d

#. Rotate non-electrical device

	.. code-block::

		rotate device FD1 absolute 10
		rotate device MH1 relative -45

	|VNS| r n


	
	
#. Flip/mirror electrical device

	.. code-block::

		flip device C1 bottom
		flip device C1 top

	|VNS| l d


#. Flip/mirror non-electrical device

	.. code-block::

		flip device FD1 bottom

	|VNS| l n

	
#. Delete non-electrical device

	.. code-block::

		delete device FD1

	|VNS| del n

	Note: Electrical devices can only be deleted in the schematic !
	
+++++++++++++++++++++
Ratsnest and Airwires
+++++++++++++++++++++

Commands and Verb-Noun key sequences
------------------------------------

#. Update ratsnest

	.. code-block::

		update ratsnest

	|VNS| u r


	
+++++++++++++++++++++++++++++++++++++
Routing, laying out Tracks and Traces
+++++++++++++++++++++++++++++++++++++

Commands and Verb-Noun key sequences
------------------------------------

#. Drawing tracks of a net

	.. code-block::

		route net

	|VNS| t n

	

++++
Vias
++++

Commands and Verb-Noun key sequences
------------------------------------

#. Place via

	.. code-block::

		place via [net name]

	|VNS| p v


#. Move via

.. 	.. code-block::
.. 
.. 		place via [net name]

	|VNS| m v

	
#. Delete via

.. 	.. code-block::
.. 
.. 		place via [net name]

	|VNS| d v

	

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


	
+++++
Lines
+++++

Commands and Verb-Noun key sequences
------------------------------------

#. Assembly Documentation

##. Draw

	.. code-block::

		draw assy top line [width] [from x] [from y] [to x] [to y]

	|VNS| d l


##. Move

	|VNS| m a



#. Silkscreen

##. Draw

	.. code-block::

		draw silkscreen top line [width] [from x] [from y] [to x] [to y]

	|VNS| d l


##. Move

	|VNS| m s
	
	
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

	
