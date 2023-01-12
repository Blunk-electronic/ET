------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            SILKSCREEN                                    --
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
with ada.containers.indefinite_doubly_linked_lists;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;				use et_geometry;
with et_pcb_stack;				use et_pcb_stack;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_text;
--with et_conductor_text;			use et_conductor_text;
with et_logging;				use et_logging;


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



-- ARCS:
	
	type type_silk_arc is new pac_geometry_2.type_arc with record
		width	: type_linewidth;
	end record;

	package pac_silk_arcs is new doubly_linked_lists (type_silk_arc);
	use pac_silk_arcs;


-- CIRCLES:

	type type_silk_circle is new pac_geometry_2.type_circle with record
		width	: type_linewidth;
	end record;
	
	package pac_silk_circles is new doubly_linked_lists (type_silk_circle);
	use pac_silk_circles;
	

-- CONTOURS:
	
	type type_silk_contour is new type_contour with null record;
	package pac_silk_contours is new indefinite_doubly_linked_lists (type_silk_contour);
	use pac_silk_contours;
	

	
-- TEXTS:
	
	-- for texts in conductor layer to be exposed:
	type type_silk_text is new type_text_fab_with_content with record
		vectors	: type_vector_text;
	end record;	

	package pac_silk_texts is new doubly_linked_lists (type_silk_text);
	use pac_silk_texts;

	
	
	-- This is the base type for silkscreen objects in general:
	type type_silkscreen is tagged record
		lines 		: pac_silk_lines.list;
		arcs		: pac_silk_arcs.list;
		circles		: pac_silk_circles.list;
		contours	: pac_silk_contours.list;
		texts		: pac_silk_texts.list;
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
