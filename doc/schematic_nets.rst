.. _schematic_nets:

.. include:: placeholders.rst


Nets and Net Labels
-------------------

#. Drawing Net Segments

	|VNS| d n

	change bend style: left mouse click or key b

	The equivalent to this shortcut is the command

	.. code-block::

		draw net


	
	To specify the net name type this command:

	.. code-block::

		draw net [RESET_N]


	To draw a segment of a net on a given sheet starting
	at a certain point and ending at a certain point:
		
	Example: Draw a segment of net GND on sheet 1 starting
	at x/y 30/100 and ending at 50/100:
		
	.. code-block::

		draw net GND  1  30 100  50 100

	* A net segment is rejected if

	#. it causes a loop inside a strand,
	#. it causes a connection between two different nets.




#. Showing and Finding Nets

	- To show a net:

	|VNS| h n
	
	- To find a net use this command:
	
	.. code-block::

		show net RESET_N

	|VNS| |NI|
	

	
	
	
#. Renaming a Net

	A single particular strand on the current sheet:
	
	|VNS| n t


	All strands on the current sheet:
	
	|VNS| n n


	Rename a specific strand on a specific sheet.
	This example renames a strand on sheet 2 that
	crosses a zone of 2mm radius around the point 50/90:

	.. code-block::

		rename net RESET_N RST_N 2 50 90 2



	All strands on a specific sheet:

	.. code-block::

		rename net RESET_N RST_N 5

	
	All strands on all sheets:

	.. code-block::

		rename net RESET_N RST_N
	
	|VNS| n N


	
	
#. Drag net segment

	When a net segment is being dragged, then other connected segments are
	dragged along. If the ends of the net are dragged to a port of a
	unit, netchanger or submodule, then a connection between port and
	net segment is established.
	
	A segment which is connected with a port can not be dragged away 
	anymore.

	If the segment meets another net, then these two nets will NOT be connected.

	This example command searches for a net segment
	on sheet 1 that crosses the position 80/100 in a zone of
	2mm radius. It drags the attacked end of the segment by 10/0mm.
	
	If more than one segment has been found in the given zone,
	then the first of them will be selected.


	.. code-block::

		drag segment 1 80 100 2 relative 10 0
	
	|VNS| g s

	
	
	
#. Delete net segment

	This example command searches for a net segment
	on sheet 1 that crosses the position 80/100 in a zone of
	2mm radius. If a segment exists there, then it will be deleted.
	If more than one segment exists on the given spot, then
	the first that has been found will be deleted.

	.. code-block::
		
		delete segment 1 80 100 2

	|VNS| del s


	
	
#. Delete a strand of a net

	This example command searches for a strand
	on sheet 1 that crosses the position 80/100 in a zone of
	2mm radius. If a strand exists there, then it will be deleted.
	If more than one strand exists on the given spot, then
	the first that has been found will be deleted.

	.. code-block::
		
		delete strand 1 80 100 2

	|VNS| del t

	
	
#. Delete a net on a certain sheet

	.. code-block::

		delete net RESET_N 2

		
	|VNS| del n
	
	
	
#. Delete a whole net on all sheets

	.. code-block::

		delete net RESET_N
	
	|VNS| del N
	

	
	
#. Placing Net Labels

	A net label is just a text next
	to a net segment to indicate the net name.
	The label will be orientated automatically so that
	it is readable either from the front or from the right.
	The spacing between net segment and label is fixed.

	|VNS| p l

	
	Example: Place a net label on sheet 1 at x/y 30/100:
		
	.. code-block::

		demo place net_label  1  30 100

	
	
#. Move Net Label

	A net label can be moved along a net segment.
	Independed of the the grid settings, the label will
	always be at a fixed distance away from the segment.

	|VNS| m l

	
	
	
#. Delete Net Label

	|VNS| del l



	
#. Placing Net Off-Page-Connectors

	An off-page-connector is a text inside a box that
	is connected with a net segment. The box
	indicates the nature of the connector (input, output,
	bidir, tristate, passive).
	Inside the box the net name is shown.

	|VNS| p c

	Example: Place a net connector on sheet 1 at x/y position 50/100.
	The connector type is 'passive':
	
	.. code-block::
	
		demo place net_connector  1  50 100  passive
	
	
#. Delete Net Off-Page-Connectors

	|VNS| del c

	


#. Set Net Class

	By default a net is member of class 'default'.
	These figures apply for the default class:
	- clearance 0.3mm
	- minimal track width 0.3mm
	- restring minimum 0.3mm


	This example makes net GND a member of class pwr:

	.. code-block::
	
		demo set class GND pwr



#. Set Net Scope

	This example sets the GND as global:

	.. code-block::
	
		set scope GND global


	.. code-block::
	
		set scope AGND local

