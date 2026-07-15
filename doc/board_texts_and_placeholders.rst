.. _board_texts_and_placeholders:

.. include:: placeholders.rst


Texts and Placeholders
----------------------


Place Text
^^^^^^^^^^

	.. code-block::

		place text

	|VNS| p x

	
	This command places a text in conductor layer 3.
	The linewidth is 0.15mm, the text size 1mm.
	The position is at (20;5) with a rotation of 0 degrees.

	.. code-block::
		
		place text conductor 3 0.15 1 20 5 0 "Dummy Text"


	.. code-block::
		
		place text silkscreen top 0.5 3 33 33 0 "SILK 1"


	.. code-block::
		
		place text assy top 0.5 3 33 25 0 "ASSY 1"


	.. code-block::
		
		place text stopmask top 0.5 3 33 33 0 "STOP 1"

	
	
Move Text
^^^^^^^^^

	|VNS| m x

	

Place Placeholder
^^^^^^^^^^^^^^^^^

	Placeholders have a meaning such as 
	COMPANY, CUSTOMER, PARTCODE, DRAWING_NUMBER,
	ASSEMBLY_VARIANT, PROJECT, MODULE, REVISION.
	
	Once a placeholder has been placed, its content will be
	filled automatically according to its meaning.

	This command places a placeholder in conductor layer 3.
	The linewidth is 0.15mm, the text size 1mm.
	The position is at (20;5) with a rotation of 0 degrees.

	.. code-block::
		
		place placeholder conductor 1 0.15 1 20 5 0 module
	

	.. code-block::
		
		place placeholder silkscreen top 0.15 1 20 5 0 partcode


	.. code-block::
		
		place placeholder assy top 0.15 1 20 5 0 partcode


	.. code-block::
		
		place placeholder stencil top 0.15 1 20 5 0 partcode
