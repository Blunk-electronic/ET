------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     STENCIL / SOLDER PASTE MASK                          --
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

with et_pcb_sides;				use et_pcb_sides;
with et_pcb_coordinates_2;		use et_pcb_coordinates_2;
with et_mirroring;				use et_mirroring;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_conductor_segment;
with et_logging;				use et_logging;


package et_stencil is

	use pac_geometry_2;
	use pac_contours;


-- LINES
	
	type type_stencil_line is new
		et_conductor_segment.type_conductor_line with null record;
	-- CS inherits a linewidth of type_track_width. Use a dedicated type
	-- for linewidth if requried.


	package pac_stencil_lines is new doubly_linked_lists (type_stencil_line);
	use pac_stencil_lines;
	

	-- Iterates the lines.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		lines	: in pac_stencil_lines.list;
		process	: not null access procedure (position : in pac_stencil_lines.cursor);
		proceed	: not null access boolean);

	-- CS likewise iteratator for arcs and circles

	-- Returns true if the "proposed-flag" of the given line is set:
	function is_proposed (
		line_cursor	: in pac_stencil_lines.cursor)
		return boolean;

	
	-- Returns true if the "selected-flag" of the given line is set:
	function is_selected (
		line_cursor	: in pac_stencil_lines.cursor)
		return boolean;



	
	-- Mirrors a list of lines along the given axis:
	procedure mirror_lines (
		lines	: in out pac_stencil_lines.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

	
	-- Rotates a list of lines by the given angle about the origin:
	procedure rotate_lines (
		lines	: in out pac_stencil_lines.list;
		angle	: in type_rotation_model);

	
	-- Moves a list of lines by the given offset:
	procedure move_lines (
		lines	: in out pac_stencil_lines.list;
		offset	: in type_distance_relative);


	

-- ARCS
	
	type type_stencil_arc is new
		et_conductor_segment.type_conductor_arc with null record;
	-- CS inherits a linewidth of type_track_width. Use a dedicated type
	-- for linewidth if requried.


	package pac_stencil_arcs is new doubly_linked_lists (type_stencil_arc);
	use pac_stencil_arcs;
	

	-- Returns true if the "proposed-flag" of the given arcis set:
	function is_proposed (
		arc_cursor	: in pac_stencil_arcs.cursor)
		return boolean;
	
	-- Returns true if the "selected-flag" of the given arc is set:
	function is_selected (
		arc_cursor	: in pac_stencil_arcs.cursor)
		return boolean;

	
	-- Mirrors a list of arcs along the given axis:
	procedure mirror_arcs (
		arcs	: in out pac_stencil_arcs.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

	
	-- Rotates a list of arcs by the given angle about the origin:
	procedure rotate_arcs (
		arcs	: in out pac_stencil_arcs.list;
		angle	: in type_rotation_model);

	
	-- Moves a list of arcs by the given offset:
	procedure move_arcs (
		arcs	: in out pac_stencil_arcs.list;
		offset	: in type_distance_relative);


	
	
-- CIRCLES
	
	type type_stencil_circle is new 
		et_conductor_segment.type_conductor_circle with null record;
	-- CS inherits a linewidth of type_track_width. Use a dedicated type
	-- for linewidth if requried.

	package pac_stencil_circles is new doubly_linked_lists (type_stencil_circle);
	use pac_stencil_circles;	


	-- Returns true if the "proposed-flag" of the given circle is set:
	function is_proposed (
		circle_cursor	: in pac_stencil_circles.cursor)
		return boolean;
	
	-- Returns true if the "selected-flag" of the given circle is set:
	function is_selected (
		circle_cursor	: in pac_stencil_circles.cursor)
		return boolean;

	
	-- Mirrors a list of circles along the given axis:
	procedure mirror_circles (
		circles	: in out pac_stencil_circles.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);
	
	-- Rotates a list of circles by the given angle about the origin:
	procedure rotate_circles (
		circles	: in out pac_stencil_circles.list;
		angle	: in type_rotation_model);

	-- Moves a list of circles by the given offset:
	procedure move_circles (
		circles	: in out pac_stencil_circles.list;
		offset	: in type_distance_relative);

	


-- CONTOURS
	
	type type_stencil_zone is new type_contour with null record;
	
	package pac_stencil_zones is new doubly_linked_lists (type_stencil_zone);
	use pac_stencil_zones;


	-- Iterates the contours. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		zones	: in pac_stencil_zones.list;
		process	: not null access procedure (position : in pac_stencil_zones.cursor);
		proceed	: not null access boolean);


	-- Returns the first zone which has an open contour among the
	-- given list of zones:
	function get_first_open (
		zones : in out pac_stencil_zones.list)
		return pac_stencil_zones.cursor;


	
	package pac_stencil_zone_cursors is 
		new doubly_linked_lists (pac_stencil_zones.cursor);

	-- Returns the cursors to all zones which have an open contour:
	function get_open_zones (
		zones : in out pac_stencil_zones.list)
		return pac_stencil_zone_cursors.list;


	
	
	-- Mirrors a list of contours along the given axis:
	procedure mirror_contours (
		contours	: in out pac_stencil_zones.list;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS);
	
	-- Rotates a list of contours by the given angle about the origin:
	procedure rotate_contours (
		contours	: in out pac_stencil_zones.list;
		angle		: in type_rotation_model);

	-- Moves a list of contours by the given offset:
	procedure move_contours (
		contours	: in out pac_stencil_zones.list;
		offset		: in type_distance_relative);


	
	-- This is the type for solder paste stencil objects in general:
	type type_stencil is record
		lines 		: pac_stencil_lines.list;
		arcs		: pac_stencil_arcs.list;
		circles		: pac_stencil_circles.list;
		contours	: pac_stencil_zones.list; -- CS rename to zones
	end record;


	type type_stencil_both_sides is record
		top		: type_stencil;
		bottom	: type_stencil;
	end record;




	-- Mirrors the given objects along the given axis:
	procedure mirror_stencil_objects (
		stencil	: in out type_stencil;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);
	
	-- Rotates the given objects by the given angle
	-- about the origin:
	procedure rotate_stencil_objects (
		stencil	: in out type_stencil;
		angle	: in type_rotation_model);

	-- Moves the given objects by the given offset:
	procedure move_stencil_objects (
		stencil	: in out type_stencil;
		offset	: in type_distance_relative);

	

	-- Logs the properties of the given arc of stencil
	procedure arc_stencil_properties (
		face			: in type_face;
		cursor			: in pac_stencil_arcs.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given circle of stencil
	procedure circle_stencil_properties (
		face			: in type_face;
		cursor			: in pac_stencil_circles.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given line of stencil
	procedure line_stencil_properties (
		face			: in type_face;
		cursor			: in pac_stencil_lines.cursor;
		log_threshold 	: in type_log_level);

	
	
end et_stencil;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
