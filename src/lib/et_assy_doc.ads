------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        ASSEMBLY DOCUMENTAION                             --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;				use et_geometry;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_text;
with et_logging;				use et_logging;


package et_assy_doc is

	use pac_geometry_2;
	use pac_contours;
	use pac_text_board;

	subtype type_linewidth is type_distance_positive range 0.15 .. 10.0;

	
-- LINES:
	
	type type_doc_line is new pac_geometry_2.type_line with record
		width	: type_linewidth;
	end record;

	
	function to_string (
		line	: in type_doc_line)
		return string;

	
	package pac_doc_lines is new doubly_linked_lists (type_doc_line);
	use pac_doc_lines;


	-- Iterates the lines.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		lines	: in pac_doc_lines.list;
		process	: not null access procedure (position : in pac_doc_lines.cursor);
		proceed	: not null access boolean);

	-- CS likewise iteratator for arcs and circles

	-- Returns true if the "proposed-flag" of the given line is set:
	function is_proposed (
		line_cursor	: in pac_doc_lines.cursor)
		return boolean;

	
	-- Returns true if the "selected-flag" of the given line is set:
	function is_selected (
		line_cursor	: in pac_doc_lines.cursor)
		return boolean;


	
	-- Mirrors a list of lines along the given axis:
	procedure mirror_lines (
		lines	: in out pac_doc_lines.list;
		axis	: in type_axis_2d := Y);					

	-- Rotates a list of lines by the given angle:
	procedure rotate_lines (
		lines	: in out pac_doc_lines.list;
		angle	: in type_rotation);					

	-- Moves a list of lines by the given offset:
	procedure move_lines (
		lines	: in out pac_doc_lines.list;
		offset	: in type_distance_relative);					


	
-- ARCS:
	
	type type_doc_arc is new pac_geometry_2.type_arc with record
		width	: type_linewidth;
	end record;

	package pac_doc_arcs is new doubly_linked_lists (type_doc_arc);
	use pac_doc_arcs;


	-- Returns true if the "proposed-flag" of the given arcis set:
	function is_proposed (
		arc_cursor	: in pac_doc_arcs.cursor)
		return boolean;
	
	-- Returns true if the "selected-flag" of the given arc is set:
	function is_selected (
		arc_cursor	: in pac_doc_arcs.cursor)
		return boolean;

	
	-- Mirrors a list of arcs along the given axis:
	procedure mirror_arcs (
		arcs	: in out pac_doc_arcs.list;
		axis	: in type_axis_2d := Y);					

	-- Rotates a list of arcs by the given angle:
	procedure rotate_arcs (
		arcs	: in out pac_doc_arcs.list;
		angle	: in type_rotation);					

	-- Moves a list of arcs by the given offset:
	procedure move_arcs (
		arcs	: in out pac_doc_arcs.list;
		offset	: in type_distance_relative);					

	

	
-- CIRCLES:

	type type_doc_circle is new pac_geometry_2.type_circle with record
		width	: type_linewidth;
	end record;
	
	package pac_doc_circles is new doubly_linked_lists (type_doc_circle);
	use pac_doc_circles;
	


	-- Returns true if the "proposed-flag" of the given circle is set:
	function is_proposed (
		circle_cursor	: in pac_doc_circles.cursor)
		return boolean;
	
	-- Returns true if the "selected-flag" of the given circle is set:
	function is_selected (
		circle_cursor	: in pac_doc_circles.cursor)
		return boolean;

	
	-- Mirrors a list of circles along the given axis:
	procedure mirror_circles (
		circles	: in out pac_doc_circles.list;
		axis	: in type_axis_2d := Y);					

	-- Rotates a list of circles by the given angle:
	procedure rotate_circles (
		circles	: in out pac_doc_circles.list;
		angle	: in type_rotation);					

	-- Moves a list of circles by the given offset:
	procedure move_circles (
		circles	: in out pac_doc_circles.list;
		offset	: in type_distance_relative);					


	
-- CONTOURS:
	
	type type_doc_contour is new type_contour with null record;
	package pac_doc_contours is new doubly_linked_lists (type_doc_contour);
	use pac_doc_contours;
	

	-- Mirrors a list of contours along the given axis:
	procedure mirror_contours (
		contours	: in out pac_doc_contours.list;
		axis		: in type_axis_2d := Y);					

	-- Rotates a list of contours by the given angle:
	procedure rotate_contours (
		contours	: in out pac_doc_contours.list;
		angle		: in type_rotation);					

	-- Moves a list of contours by the given offset:
	procedure move_contours (
		contours	: in out pac_doc_contours.list;
		offset		: in type_distance_relative);					

	
	
	
-- TEXTS:
	
	type type_doc_text is new type_text_fab_with_content with record
		vectors	: type_vector_text;
	end record;	

	package pac_doc_texts is new doubly_linked_lists (type_doc_text);
	use pac_doc_texts;


	-- Mirrors a list of texts along the given axis:
	procedure mirror_texts (
		texts	: in out pac_doc_texts.list;
		axis	: in type_axis_2d := Y);					

	-- Rotates a list of texts by the given angle:
	procedure rotate_texts (
		texts	: in out pac_doc_texts.list;
		angle	: in type_rotation);					

	-- Moves a list of texts by the given offset:
	procedure move_texts (
		texts	: in out pac_doc_texts.list;
		offset	: in type_distance_relative);					

	
	
	-- This is the base type for assy doc objects in general:
	type type_assy_doc is tagged record
		lines 		: pac_doc_lines.list;
		arcs		: pac_doc_arcs.list;
		circles		: pac_doc_circles.list;
		contours	: pac_doc_contours.list;
		texts		: pac_doc_texts.list;
	end record;


	-- Logs the properties of the given line of assembly documentation
	procedure line_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_doc_lines.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given arc of assembly documentation
	procedure arc_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_doc_arcs.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given circle of assembly documentation
	procedure circle_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_doc_circles.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given text:
	procedure text_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_doc_texts.cursor;
		log_threshold 	: in type_log_level);
	
	
end et_assy_doc;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
