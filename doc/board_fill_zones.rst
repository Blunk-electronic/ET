.. _board_fill_zones:

.. include:: placeholders.rst


Fill Zones in Conductor Layers
------------------------------


#. Define Outline

	Fill zone connected with a net:

	.. code-block::

		route net gnd 1 zone line 23 0 line 232 0 line 232 33 line 100 33
		route net gnd 1 zone line 25 25 arc 30 25 27 25 ccw line 30 28 line 25 28
		route net gnd 1 zone circle 35 30 3
	
	
	Floating fill zone connected with no net:
	
		.. code-block::
	
		route freetrack 1 zone line 18 9 line 30 9 line 30 13 line 18 13
		

		
#. Move Zone Segment

	To move a segment of a zone contour:

	|VNS| m c
	
		
		
#. Set Fill Style

	.. code-block::

		set zone fill solid


	.. code-block::

		set zone fill hatched

		NOTE: This is currently under construction.

		

#. Set Spacing

	The spacing defines the distance between fill lines
	if the fill style is hatched.

	.. code-block::
	
		set zone spacing 1.5

		
		
#. Set Linewidth of Border and Fill Stripes

	The border of the zone consists of segments (like lines, arcs, circle)
	and has a certain linewidth. When the outline
	of the zone is specified then it must be considered that the actual border
	runs right in the middle of the segments.
	
	.. code-block::
	
		set zone linewidth 0.2

		

#. Set Thermal Relief Maxium Gap Width

	This is the maximal space between terminal edge and fill zone.
	For spaces greater than gap_max no spoke will be generated:

	.. code-block::
	
		set zone relief gap_max 0.4

		

#. Set Thermal Relief Minimum Track Width

	This is the minimal width of the thermal relief spokes.
	It applies to ALL terminals connected with the zone.

	.. code-block::
	
		set zone relief width_min 0.3


		
#. Set Connection Style

	.. code-block::
	
		set zone connection thermal


	.. code-block::
	
		set zone connection solid

		

#. Set Isolation

	This is the space between the zone and foreign conductor objects:

	.. code-block::
	
		set zone isolation 0.4

		

#. Set Priority

	.. code-block::
	
		set zone priority 2


		
#. Set Easing Style

	.. code-block::
	
		set zone easing style none

	.. code-block::
	
		set zone easing style chamfer
		
	.. code-block::
	
		set zone easing style fillet

	.. code-block::
	
		set zone easing radius 0.5
		

		
		
		
#. (Re)fill or pour zones

	.. code-block::

		fill zone [net name] [net name] [net name] ... 

	|VNS| f z


	
#. Empty/Clear Fill Zone

	.. code-block::

		clear zone [net name] [net name] [net name] ... 

	|VNS| e z
