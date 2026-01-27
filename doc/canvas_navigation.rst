.. _canvas_navigation:

.. include:: placeholders.rst

************************
Navigating on the Canvas
************************


++++
Grid
++++

#. Quick Increasing of the grid spacing

Keep the left CTRL-key pressed and press SHIFT briefly.

#. Quick Decreasing of the grid spacing

Keep the right CTRL-key pressed and press SHIFT briefly.


The commandline allows setting the properties of the grid.

#. Enabling and Disabling

	.. code-block::

		set grid on
		set grid off


#. Spacing

	.. code-block::

		set grid spacing 5
		set grid spacing 5 10


#. Style

	.. code-block::

		set grid style dots
		set grid style lines


	


	
++++++
Cursor
++++++


#. Placing

	The cursor can be positioned on left mouse button click.
	It can also be moved via the arrow keys (right, left, up, down).
	he cursor position always snaps to the nearest grid point.


	In order to place the cursor at a certain position:

	.. code-block::

		set cursor 10 10


#. Placing and Zooming

	In order to place the cursor at a certain position
	and to zoom on that point with a certain zoom-factor:

	.. code-block::

		set cursor 10 10 50



#. Moving

	The cursor can be moved by a certain distance in x or y direction.

	.. code-block::

		move cursor 5 -10

		
	
++++
Zoom
++++

#. Zoom on Pointer

	To zoom at the current mouse pointer, keep CTRL pressed and 
	turn the mouse wheel.



#. Zoom on Cursor

	To zoom at the current cursor position, keep CTRL pressed and 
	press the + or - key.
	

	
#. Setting Zoom Factor

	.. code-block::

		set zoom 50


	
#. Zoom-to-Fit / Fit All

	.. code-block::

		zoom all

		
		
+++++
Scale
+++++

#. Setting the Scale

	.. code-block::

		set scale 10

	After setting the scale, a default grid spacing
	is applied automatically. The user is required to
	set the grid spacing anew.
