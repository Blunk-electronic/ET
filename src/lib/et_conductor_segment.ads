------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         CONDUCTOR SEGMENT                                --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:

with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with et_pcb_sides;				use et_pcb_sides;
with et_board_coordinates;		use et_board_coordinates;
with et_design_rules_board;		use et_design_rules_board;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_mirroring;				use et_mirroring;


package et_conductor_segment is

	use pac_geometry_brd;
	use pac_polygons;
	
	use pac_geometry_2;

	
-- LINES

	-- The center of a conductor line segment.
	-- Drawn by the operator in the layout editor:
	type type_conductor_line is new pac_geometry_2.type_line with record
		width	: type_track_width;
	end record;



	-- CS procedure to set linewidth
		

	-- Converts a line with a given width to a polygon
	-- with round caps on the line ends:
	function to_polygon (
		line 		: in type_conductor_line;
		tolerance	: in type_distance_positive)
		return type_polygon;


	
	
	-- Computes the shortest distance from a point to
	-- a conductor line segment. If the return is negative,
	-- then the point is inside the segment.
	-- If the segment contour is not closed, then an exception
	-- is raised:
	--function get_shortest_distance (
		--point	: in type_point;
		--segment	: in type_conductor_line_segment)
		--return type_distance_model;
	

	package pac_conductor_lines is new doubly_linked_lists (type_conductor_line);
	use pac_conductor_lines;
	

	function get_A (
		line : in pac_conductor_lines.cursor)
		return type_vector_model;

	function get_B (
		line : in pac_conductor_lines.cursor)
		return type_vector_model;



	
	-- Iterates the lines.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		lines	: in pac_conductor_lines.list;
		process	: not null access procedure (position : in pac_conductor_lines.cursor);
		proceed	: not null access boolean);


	
	-- Mirrors a list of lines along the given axis:
	procedure mirror_lines (
		lines	: in out pac_conductor_lines.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

	
	-- Rotates a list of lines by the given angle about the origin:
	procedure rotate_lines (
		lines	: in out pac_conductor_lines.list;
		angle	: in type_rotation_model);


	-- Moves a list of lines by the given offset:
	procedure move_lines (
		lines	: in out pac_conductor_lines.list;
		offset	: in type_vector_model);


	-- Converts a list of lines to a list of polygons:
	function to_polygons (
		lines		: in pac_conductor_lines.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list;
							 
	
	-- Logs the properties of the given line:
	procedure line_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_lines.cursor;
		log_threshold 	: in type_log_level);


	

-- ARCS

	-- The center of a conductor arc segment.
	-- Drawn by the operator in the layout editor:
	type type_conductor_arc is new pac_geometry_2.type_arc with record
		width	: type_track_width;
	end record;


	-- CS function get_width
	
	
	function to_polygon (
		arc 		: in type_conductor_arc;
		tolerance	: in type_distance_positive)							
		return type_polygon;
	

	-- Computes the shortest distance from a point to
	-- a conductor arc segment. If the return is negative,
	-- then the point is inside the segment:
	--function get_shortest_distance (
		--point	: in type_point;
		--segment	: in type_conductor_arc_segment)
		--return type_distance_model;

	
	package pac_conductor_arcs is new doubly_linked_lists (type_conductor_arc);
	use pac_conductor_arcs;


	function get_A (
		arc : in pac_conductor_arcs.cursor)
		return type_vector_model;

	
	function get_B (
		arc : in pac_conductor_arcs.cursor)
		return type_vector_model;


	

	-- Iterates the arcs.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		arcs	: in pac_conductor_arcs.list;
		process	: not null access procedure (position : in pac_conductor_arcs.cursor);
		proceed	: not null access boolean);


	
	-- Mirrors a list of arcs along the given axis:
	procedure mirror_arcs (
		arcs	: in out pac_conductor_arcs.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

	
	-- Rotates a list of arcs by the given angle about the origin:
	procedure rotate_arcs (
		arcs	: in out pac_conductor_arcs.list;
		angle	: in type_rotation_model);


	-- Moves a list of arcs by the given offset:
	procedure move_arcs (
		arcs	: in out pac_conductor_arcs.list;
		offset	: in type_vector_model);


	-- Converts a list of arcs to a list of polygons:
	function to_polygons (
		arcs		: in pac_conductor_arcs.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list;

	
	-- Logs the properties of the given arc:
	procedure arc_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_arcs.cursor;
		log_threshold 	: in type_log_level);



	
-- CIRCLES

	type type_conductor_circle is new type_circle with record
		width	: type_track_width := type_track_width'first;
	end record;


	-- Converts the outer edge of a conductor circle to a polygon:	
	function to_polygon_outside (
		circle 		: in type_conductor_circle;
		tolerance	: in type_distance_positive)							
		return type_polygon;

	
	-- Converts the inner edge of a conductor circle to a polygon:	
	function to_polygon_inside (
		circle 		: in type_conductor_circle;
		tolerance	: in type_distance_positive)							
		return type_polygon;


	
	package pac_conductor_circles is new doubly_linked_lists (type_conductor_circle);
	use pac_conductor_circles;


	-- Iterates the circles.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		circles	: in pac_conductor_circles.list;
		process	: not null access procedure (position : in pac_conductor_circles.cursor);
		proceed	: not null access boolean);


	
	-- Mirrors a list of circles along the given axis:
	procedure mirror_circles (
		circles	: in out pac_conductor_circles.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

	
	-- Rotates a list of circles by the given angle about the origin:
	procedure rotate_circles (
		circles	: in out pac_conductor_circles.list;
		angle	: in type_rotation_model);


	-- Moves a list of circles by the given offset:
	procedure move_circles (
		circles	: in out pac_conductor_circles.list;
		offset	: in type_vector_model);


	-- Converts the outer edges of circles to a list of polygons:
	function to_polygons_outside (
		circles		: in pac_conductor_circles.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list;


	-- Converts the inner edges of circles to a list of polygons:
	function to_polygons_inside (
		circles		: in pac_conductor_circles.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list;

	
	-- Logs the properties of the given circle:
	procedure circle_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_circles.cursor;
		log_threshold 	: in type_log_level);


	
end et_conductor_segment;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
