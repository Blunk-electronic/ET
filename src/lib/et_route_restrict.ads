------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          ROUTE RESTRICT                                  --
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

with ada.strings;	 			use ada.strings;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;

with et_pcb_sides;				use et_pcb_sides;
with et_board_coordinates;		use et_board_coordinates;
with et_mirroring;				use et_mirroring;
with et_board_text;				use et_board_text;
with et_contour_to_polygon;		use et_contour_to_polygon;
with et_pcb_stack;				use et_pcb_stack;
with et_logging;				use et_logging;


package et_route_restrict is

	use pac_geometry_2;
	use pac_contours;
	use pac_geometry_brd;
	use pac_polygons;
	use pac_text_board;
	

	
-- LINES:

	type type_route_restrict_line is new
		pac_geometry_2.type_line with null record;
	

	
	package pac_route_restrict_lines is new doubly_linked_lists (type_route_restrict_line);
	use pac_route_restrict_lines;

	-- Mirrors a list of lines along the given axis:
	procedure mirror_lines (
		lines	: in out pac_route_restrict_lines.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);
	
	-- Rotates a list of lines by the given angle about the origin:
	procedure rotate_lines (
		lines	: in out pac_route_restrict_lines.list;
		angle	: in type_rotation_model);

	-- Moves a list of lines by the given offset:
	procedure move_lines (
		lines	: in out pac_route_restrict_lines.list;
		offset	: in type_vector_model);



	
-- ARCS:	

	type type_route_restrict_arc is new
		pac_geometry_2.type_arc with null record;
	

	
	package pac_route_restrict_arcs is new doubly_linked_lists (type_route_restrict_arc);
	use pac_route_restrict_arcs;

	-- Mirrors a list of arcs along the given axis:
	procedure mirror_arcs (
		arcs	: in out pac_route_restrict_arcs.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);
	
	-- Rotates a list of arcs by the given angle about the origin:
	procedure rotate_arcs (
		arcs	: in out pac_route_restrict_arcs.list;
		angle	: in type_rotation_model);

	-- Moves a list of arcs by the given offset:
	procedure move_arcs (
		arcs	: in out pac_route_restrict_arcs.list;
		offset	: in type_vector_model);


	

-- CIRCLES:	

	type type_route_restrict_circle is new
		pac_geometry_2.type_circle with null record;

	
	-- Converts the outer edge of a circle to a polygon:	
	function to_polygon_outside (
		circle 		: in type_route_restrict_circle;
		tolerance	: in type_distance_positive)							
		return type_polygon;

	
	-- Converts the inner edge of a circle to a polygon:	
	function to_polygon_inside (
		circle 		: in type_route_restrict_circle;
		tolerance	: in type_distance_positive)							
		return type_polygon;

	
	package pac_route_restrict_circles is new doubly_linked_lists (type_route_restrict_circle);
	use pac_route_restrict_circles;

	-- Mirrors a list of circles along the given axis:
	procedure mirror_circles (
		circles	: in out pac_route_restrict_circles.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);
	
	-- Rotates a list of circles by the given angle about the origin:
	procedure rotate_circles (
		circles	: in out pac_route_restrict_circles.list;
		angle	: in type_rotation_model);

	-- Moves a list of circles by the given offset:
	procedure move_circles (
		circles	: in out pac_route_restrict_circles.list;
		offset	: in type_vector_model);


	-- Converts the outer edges of circles to a list of polygons:
	function to_polygons_outside (
		circles		: in pac_route_restrict_circles.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list;


	-- Converts the inner edges of circles to a list of polygons:
	function to_polygons_inside (
		circles		: in pac_route_restrict_circles.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list;


	
	
-- ZONES:
	type type_route_restrict_zone is new type_contour with null record;

	package pac_route_restrict_zones is new doubly_linked_lists (type_route_restrict_zone);

	
	type type_route_restrict_cutout is new type_contour with null record;
	-- CS not sure whether this is really required.
		
	package pac_route_restrict_cutouts is new doubly_linked_lists (type_route_restrict_cutout);
	-- CS not sure whether this is really required.





	-- Logs the properties of the given line of route restrict
	procedure line_route_restrict_properties (
		face			: in type_face;
		cursor			: in pac_route_restrict_lines.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given arc of route restrict
	procedure arc_route_restrict_properties (
		face			: in type_face;
		cursor			: in pac_route_restrict_arcs.cursor;
		log_threshold 	: in type_log_level);

	-- CS procedure circle_route_restrict_properties

	
	
end et_route_restrict;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
