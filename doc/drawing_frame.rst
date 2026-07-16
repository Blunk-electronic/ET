.. _drawing_frame:



		
Drawing Frame
=============

There is a distiction between drawing frames for schematic and PCB.

To create or open a schematic frame template:

	.. parsed-literal::

		$ et --create-schematic-frame tmp/frames/A4_landscape.frs


	.. parsed-literal::

		$ et --open-schematic-frame lib/frames/schematic/A4_landscape.frs --save-schematic-frame-as tmp/frames/A4_landscape.frs




Similar a board frame template can be created or opened:

	.. parsed-literal::

		$ et --create-pcb-frame tmp/frames/A4_landscape.frb


	.. parsed-literal::

		$ et --open-pcb-frame lib/frames/pcb/A4_landscape.frb --save-pcb-frame-as tmp/frames/A4_landscape.frb

