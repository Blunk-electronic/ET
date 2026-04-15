.. _submodules:

.. include:: placeholders.rst

**********
Submodules
**********



++++++++++++++++
Schematic Editor
++++++++++++++++


#. Add a Submodule

	Submodules are always instantiated from a generic module. 
	On instantiation the submodule is given an instance name.
	To edit a submodule, the generic module (the file with extension *.mod)
	must be edited.


	This example command places an instance
	of the submodule oscillator.mod with the instance name OSC1
	on sheet 1 at x/y 170/90.
	In the schematic the instance appears as a box with the dimensions
	30 x 30 mm:

	.. code-block::
	
		add submodule oscillator.mod OSC1 1 170 90 30 30




#. Assign the Generic File

	This example command assigns the generic module file:

	.. code-block::

		set submodule_file OSC1 templates/oscillator_experimental.mod





#. Move a Submodule


	.. code-block::
	
		move submodule OSC1 relative 2 0 0


	.. code-block::
	
		move submodule OSC1 absolute 4 170 150




#. Drag a Submodule

	Dragging a submodule drags the connected nets segments along.
	If a submodule port ends up where a net segment is, it becomes connected with the net.
	Dragging is not possible across sheets.


	.. code-block::
	
		drag submodule OSC1 relative 10 0


	.. code-block::
	
		drag submodule OSC1 absolute 170 150





#. Rename a Submodule


	.. code-block::
	
		rename submodule OSC1 CLK_GENERATOR_1




#. Copy a Submodule


	.. code-block::
	
		copy submodule OSC1 OSC2 170 120





#. Delete a Submodule


	.. code-block::
	
		delete submodule OSC2




Ports
-----

	The port of a submodule is named after a net which must exist inside the submodule.
	The net becomes visible to the outside world if it is connected with a netchanger.
	If a net inside the submodule is not connected with a netchanger, then it can not
	be seen from outside.
	To place a port of a submodule the module instance, the exported net, the port position
	and the port direction must be given. See the follwing example command where
	OSC1 is the instance, OSC_OUT is the port name, 0/10 is the position and slave is the
	direction.
	The direction has nothing to do with energy or information flow. It can either be 
	"master" or "slave". The direction is relevant for
	netlist generation. A master port enforces its name onto the connected net whereas a
	slave port propagates the name of the connected net into the submodule.


#. Add a Port

	.. code-block::

		add port OSC1 OSC_OUT 0 10 slave 



#. Delete a Port

	.. code-block::

		delete port OSC1 OSC_OUT



#. Move a Port

	Moving a port disconnects it from net old segments and places it at the given position.
	If the port ends up where a net segment is, it becomes connected with the net.

	.. code-block::

		move port OSC1 OSC_OUT absolute 110 260

	.. code-block::

		move port OSC1 OSC_OUT relative 10 -2



#. Drag a Port

	Dragging a port drags the connected nets segments along.
	If the port ends up where a net segment is, it becomes connected with the net.

	.. code-block::

		drag port OSC1 OSC_OUT absolute 110 260


	.. code-block::

		drag port OSC1 OSC_OUT relative 10 -2







