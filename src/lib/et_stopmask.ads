------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          SOLDER STOPMASK                                 --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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
with ada.strings;				use ada.strings;

with et_pcb_sides;				use et_pcb_sides;
with et_pcb_coordinates_2;		use et_pcb_coordinates_2;
with et_mirroring;				use et_mirroring;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_conductor_segment;
with et_text;

with et_logging;				use et_logging;


package et_stopmask is

	use pac_geometry_2;
	use pac_contours;
	use pac_text_board;


-- LINES:
	
	type type_stop_line is new		
		et_conductor_segment.type_conductor_line with null record;
	-- CS inherits a linewidth of type_track_width. Use a dedicated type
	-- for linewidth if requried.

	package pac_stop_lines is new doubly_linked_lists (type_stop_line);
	use pac_stop_lines;


	-- Iterates the lines.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		lines	: in pac_stop_lines.list;
		process	: not null access procedure (position : in pac_stop_lines.cursor);
		proceed	: not null access boolean);

	-- CS likewise iteratator for arcs and circles

	
	
	-- Returns true if the "proposed-flag" of the given line is set:
	function is_proposed (
		line_cursor	: in pac_stop_lines.cursor)
		return boolean;

	
	-- Returns true if the "selected-flag" of the given line is set:
	function is_selected (
		line_cursor	: in pac_stop_lines.cursor)
		return boolean;

	
	-- Mirrors a list of lines along the given axis:
	procedure mirror_lines (
		lines	: in out pac_stop_lines.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

	-- Rotates a list of lines by the given angle:
	procedure rotate_lines (
		lines	: in out pac_stop_lines.list;
		angle	: in type_rotation_model);					

	-- Moves a list of lines by the given offset:
	procedure move_lines (
		lines	: in out pac_stop_lines.list;
		offset	: in type_distance_relative);					


	

-- ARCS:
	
	type type_stop_arc is new
		et_conductor_segment.type_conductor_arc with null record;
	-- CS inherits a linewidth of type_track_width. Use a dedicated type
	-- for linewidth if requried.

	package pac_stop_arcs is new doubly_linked_lists (type_stop_arc);
	use pac_stop_arcs;	


	-- Returns true if the "proposed-flag" of the given arcis set:
	function is_proposed (
		arc_cursor	: in pac_stop_arcs.cursor)
		return boolean;
	
	-- Returns true if the "selected-flag" of the given arc is set:
	function is_selected (
		arc_cursor	: in pac_stop_arcs.cursor)
		return boolean;

	
	-- Mirrors a list of arcs along the given axis:
	procedure mirror_arcs (
		arcs	: in out pac_stop_arcs.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

	-- Rotates a list of arcs by the given angle:
	procedure rotate_arcs (
		arcs	: in out pac_stop_arcs.list;
		angle	: in type_rotation_model);					

	-- Moves a list of arcs by the given offset:
	procedure move_arcs (
		arcs	: in out pac_stop_arcs.list;
		offset	: in type_distance_relative);					



-- CIRCLES:
	
	type type_stop_circle is new
		et_conductor_segment.type_conductor_circle with null record;
	-- CS inherits a linewidth of type_track_width. Use a dedicated type
	-- for linewidth if requried.

	package pac_stop_circles is new doubly_linked_lists (type_stop_circle);
	use pac_stop_circles;



	-- Returns true if the "proposed-flag" of the given circle is set:
	function is_proposed (
		circle_cursor	: in pac_stop_circles.cursor)
		return boolean;
	
	-- Returns true if the "selected-flag" of the given circle is set:
	function is_selected (
		circle_cursor	: in pac_stop_circles.cursor)
		return boolean;

	
	-- Mirrors a list of circles along the given axis:
	procedure mirror_circles (
		circles	: in out pac_stop_circles.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);	

	-- Rotates a list of circles by the given angle:
	procedure rotate_circles (
		circles	: in out pac_stop_circles.list;
		angle	: in type_rotation_model);					

	-- Moves a list of circles by the given offset:
	procedure move_circles (
		circles	: in out pac_stop_circles.list;
		offset	: in type_distance_relative);					


	
-- ZONES:
	
	type type_stop_zone is new type_contour with null record;
	package pac_stop_zones is new doubly_linked_lists (type_stop_zone);
	use pac_stop_zones;



	-- Returns true if the given zone consists of a circle:
	function is_circular (
		zone	: in pac_stop_zones.cursor)
		return boolean;


	
	-- Iterates the zones.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		zones	: in pac_stop_zones.list;
		process	: not null access procedure (position : in pac_stop_zones.cursor);
		proceed	: not null access boolean);

	
	-- Mirrors a list of contours along the given axis:
	procedure mirror_contours (
		contours	: in out pac_stop_zones.list;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS);

	-- Rotates a list of contours by the given angle:
	procedure rotate_contours (
		contours	: in out pac_stop_zones.list;
		angle		: in type_rotation_model);					

	-- Moves a list of contours by the given offset:
	procedure move_contours (
		contours	: in out pac_stop_zones.list;
		offset		: in type_distance_relative);					






-- TEXTS:
	
	-- for texts in conductor layer to be exposed:
	type type_stop_text is new type_text_fab_with_content with null record;
	

	package pac_stop_texts is new doubly_linked_lists (type_stop_text);
	use pac_stop_texts;


	-- Returns true if the "proposed-flag" of the given text set:
	function is_proposed (
		text_cursor	: in pac_stop_texts.cursor)
		return boolean;
	
	-- Returns true if the "selected-flag" of the given text is set:
	function is_selected (
		text_cursor	: in pac_stop_texts.cursor)
		return boolean;


	
	-- Returns the position, linewidth and content
	-- of the given text:
	function to_string (
		text : in pac_stop_texts.cursor)
		return string;
	

	-- Iterates the texts.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		texts	: in pac_stop_texts.list;
		process	: not null access procedure (position : in pac_stop_texts.cursor);
		proceed	: not null access boolean);




	
	-- Mirrors a list of texts along the given axis:
	procedure mirror_texts (
		texts	: in out pac_stop_texts.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

	-- Rotates a list of texts by the given angle:
	procedure rotate_texts (
		texts	: in out pac_stop_texts.list;
		angle	: in type_rotation_model);					

	-- Moves a list of texts by the given offset:
	procedure move_texts (
		texts	: in out pac_stop_texts.list;
		offset	: in type_distance_relative);					


	
	-- This is the type for stopmask objects in general.
	-- This has nothing to do with the stop mask of pads.
	type type_stopmask is tagged record
		lines 	: pac_stop_lines.list;
		arcs	: pac_stop_arcs.list;
		circles	: pac_stop_circles.list;
		zones	: pac_stop_zones.list;
		texts	: pac_stop_texts.list;
	end record;


	
	-- Logs the properties of the given arc of stop mask
	procedure arc_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_arcs.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given circle of stop mask
	procedure circle_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_circles.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given line of stop mask
	procedure line_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_lines.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given stop mask text
	procedure text_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_texts.cursor;
		log_threshold 	: in type_log_level);

	
end et_stopmask;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
