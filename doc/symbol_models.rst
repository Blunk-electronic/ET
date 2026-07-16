.. _symbol_modles:

.. .. include:: placeholders.rst


Schematic Symbols
=================

A symbol is an abstraction of a component in the schematic. 
Symbols can represent a virtual component such as a GND-symbol
or something real like a resistor that is mounted on the PCB.
By default a pcb-type symbol will be created.
The newly created symbol should be saved right away.
To create a native symbol like 'opamp.sym' run this command: 


Importing and Syntax Check
--------------------------

In Headless Mode
^^^^^^^^^^^^^^^^

	
	.. parsed-literal::
		
		$ et --open-symbol libraries/symbols/transistor_npn.sym


	.. parsed-literal::
		
		$ et --open-symbol libraries/symbols/transistor_npn.sym --log-level 5

	
	.. parsed-literal::
		
		$ et --open-symbol libraries/symbols/transistor_npn.sym --save-symbol-as tmp/test.sym




Create a Symbol
---------------


	.. parsed-literal::

		$ et --create-symbol --symbol-appearance pcb --save-symbol-as tmp/dummy_opamp.sym


	.. parsed-literal::

		$ et --create-symbol --symbol-appearance virtual --save-symbol-as tmp/gnd.sym


Since the appearance has a default, it can be omitted:

	.. parsed-literal::

		$ et --create-symbol --save-symbol-as tmp/dummy_opamp.sym




Open a Symbol
-------------

To open a native symbol drawing like 'opamp.sym' run this command: 

	.. parsed-literal::

		$ et --open-symbol libraries/symbols/opamp.sym


The symbol can also be saved under a different name at a different place:

	.. parsed-literal::

		$ et --open-symbol libraries/symbols/opamp.sym --save-symbol-as tmp/dummy_opamp.sym


Opening a symbol includes syntax checking. See the report for details.

