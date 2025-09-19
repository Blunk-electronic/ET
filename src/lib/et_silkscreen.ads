------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            SILKSCREEN                                    --
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
with et_board_geometry;			use et_board_geometry;
with et_board_text;				use et_board_text;
with et_text;
with et_logging;				use et_logging;
with et_mirroring;				use et_mirroring;


package et_silkscreen is

	use pac_geometry_2;
	use pac_contours;
	use pac_text_board;


	subtype type_linewidth is type_distance_positive range 0.15 .. 10.0;
	

-- LINES:
	
	type type_silk_line is new pac_geometry_2.type_line with record
		width	: type_linewidth;
	end record;

	package pac_silk_lines is new doubly_linked_lists (type_silk_line);
	use pac_silk_lines;


	-- Iterates the lines.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		lines	: in pac_silk_lines.list;
		process	: not null access procedure (position : in pac_silk_lines.cursor);
		proceed	: not null access boolean);

	-- CS likewise iteratator for circles

	
	-- Returns true if the "proposed-flag" of the given line is set:
	function is_proposed (
		line_cursor	: in pac_silk_lines.cursor)
		return boolean;

	
	-- Returns true if the "selected-flag" of the given line is set:
	function is_selected (
		line_cursor	: in pac_silk_lines.cursor)
		return boolean;



	
	-- Mirrors a list of lines along the given axis:
	procedure mirror_lines (
		lines	: in out pac_silk_lines.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

	-- Rotates a list of lines by the given angle:
	procedure rotate_lines (
		lines	: in out pac_silk_lines.list;
		angle	: in type_rotation_model);					

	-- Moves a list of lines by the given offset:
	procedure move_lines (
		lines	: in out pac_silk_lines.list;
		offset	: in type_vector_model);					


	

-- ARCS:
	
	type type_silk_arc is new pac_geometry_2.type_arc with record
		width	: type_linewidth;
	end record;

	package pac_silk_arcs is new doubly_linked_lists (type_silk_arc);
	use pac_silk_arcs;


	-- Iterates the arcs.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		arcs	: in pac_silk_arcs.list;
		process	: not null access procedure (position : in pac_silk_arcs.cursor);
		proceed	: not null access boolean);

	

	
	-- Returns true if the "proposed-flag" of the given arcis set:
	function is_proposed (
		arc_cursor	: in pac_silk_arcs.cursor)
		return boolean;
	
	-- Returns true if the "selected-flag" of the given arc is set:
	function is_selected (
		arc_cursor	: in pac_silk_arcs.cursor)
		return boolean;
	

	-- Mirrors a list of arcs along the given axis:
	procedure mirror_arcs (
		arcs	: in out pac_silk_arcs.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);	

	-- Rotates a list of arcs by the given angle:
	procedure rotate_arcs (
		arcs	: in out pac_silk_arcs.list;
		angle	: in type_rotation_model);					

	-- Moves a list of arcs by the given offset:
	procedure move_arcs (
		arcs	: in out pac_silk_arcs.list;
		offset	: in type_vector_model);					

	

-- CIRCLES:

	type type_silk_circle is new pac_geometry_2.type_circle with record
		width	: type_linewidth;
	end record;
	
	package pac_silk_circles is new doubly_linked_lists (type_silk_circle);
	use pac_silk_circles;


	-- Iterates the circles.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		circles	: in pac_silk_circles.list;
		process	: not null access procedure (position : in pac_silk_circles.cursor);
		proceed	: not null access boolean);


	
	-- Returns true if the "proposed-flag" of the given circle is set:
	function is_proposed (
		circle_cursor	: in pac_silk_circles.cursor)
		return boolean;
	
	-- Returns true if the "selected-flag" of the given circle is set:
	function is_selected (
		circle_cursor	: in pac_silk_circles.cursor)
		return boolean;

	
	-- Mirrors a list of circles along the given axis:
	procedure mirror_circles (
		circles	: in out pac_silk_circles.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

	-- Rotates a list of circles by the given angle:
	procedure rotate_circles (
		circles	: in out pac_silk_circles.list;
		angle	: in type_rotation_model);					

	-- Moves a list of circles by the given offset:
	procedure move_circles (
		circles	: in out pac_silk_circles.list;
		offset	: in type_vector_model);					



	
-- ZONES:
	
	type type_silk_zone is new type_contour with null record;
	package pac_silk_zones is new doubly_linked_lists (type_silk_zone);
	use pac_silk_zones;


	-- Returns true if the given zone consists of a circle:
	function is_circular (
		zone	: in pac_silk_zones.cursor)
		return boolean;



	

	-- Iterates the zones.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		zones	: in pac_silk_zones.list;
		process	: not null access procedure (position : in pac_silk_zones.cursor);
		proceed	: not null access boolean);


	

	-- Mirrors a list of contours along the given axis:
	procedure mirror_contours (
		contours	: in out pac_silk_zones.list;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS);					

	-- Rotates a list of contours by the given angle:
	procedure rotate_contours (
		contours	: in out pac_silk_zones.list;
		angle		: in type_rotation_model);					

	-- Moves a list of contours by the given offset:
	procedure move_contours (
		contours	: in out pac_silk_zones.list;
		offset		: in type_vector_model);					

	
	
	
-- TEXTS:
	
	type type_silk_text is new type_text_fab_with_content with null record;	

	package pac_silk_texts is new doubly_linked_lists (type_silk_text);
	use pac_silk_texts;



	-- Returns true if the "proposed-flag" of the given text set:
	function is_proposed (
		text_cursor	: in pac_silk_texts.cursor)
		return boolean;
	
	-- Returns true if the "selected-flag" of the given text is set:
	function is_selected (
		text_cursor	: in pac_silk_texts.cursor)
		return boolean;


	
	-- Returns the position, linewidth and content
	-- of the given text:
	function to_string (
		text : in pac_silk_texts.cursor)
		return string;
	

	-- Iterates the texts.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		texts	: in pac_silk_texts.list;
		process	: not null access procedure (position : in pac_silk_texts.cursor);
		proceed	: not null access boolean);



	
	-- Mirrors a list of texts along the given axis:
	procedure mirror_texts (
		texts	: in out pac_silk_texts.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

	-- Rotates a list of texts by the given angle:
	procedure rotate_texts (
		texts	: in out pac_silk_texts.list;
		angle	: in type_rotation_model);					

	-- Moves a list of texts by the given offset:
	procedure move_texts (
		texts	: in out pac_silk_texts.list;
		offset	: in type_vector_model);					



	
	-- This is the base type for silkscreen objects in general:
	type type_silkscreen is tagged record
		lines 	: pac_silk_lines.list;
		arcs	: pac_silk_arcs.list;
		circles	: pac_silk_circles.list;
		zones	: pac_silk_zones.list;
		texts	: pac_silk_texts.list;
	end record;



	-- Logs the properties of the given line:
	procedure line_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_silk_lines.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given arc:
	procedure arc_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_silk_arcs.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given circle:
	procedure circle_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_silk_circles.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given text:
	procedure text_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_silk_texts.cursor;
		log_threshold 	: in type_log_level);
	
end et_silkscreen;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
