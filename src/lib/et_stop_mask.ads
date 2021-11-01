------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             STOP MASK                                    --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
with et_conductor_text;			use et_conductor_text;

with et_logging;				use et_logging;


package et_stop_mask is

	use pac_geometry_brd;

	use pac_geometry_2;
	use pac_polygons;
	use pac_text_fab;


	type type_stop_line is new type_line with record
		width	: type_general_line_width;
	end record;

	package pac_stop_lines is new doubly_linked_lists (type_stop_line);


	type type_stop_arc is new type_arc with record
		width	: type_general_line_width;
	end record;

	package pac_stop_arcs is new doubly_linked_lists (type_stop_arc);

	package pac_stop_circles is new indefinite_doubly_linked_lists (type_fillable_circle);

	package pac_stop_polygons is new indefinite_doubly_linked_lists (type_polygon_non_conductor);
	package pac_stop_cutouts is new doubly_linked_lists (type_polygon);
	
	-- This is the type for stop mask objects in general.
	-- This has nothing to do with the stop mask of pads.
	type type_stop_mask is tagged record
		lines 		: pac_stop_lines.list;
		arcs		: pac_stop_arcs.list;
		circles		: pac_stop_circles.list;
		polygons	: pac_stop_polygons.list;
		cutouts		: pac_stop_cutouts.list;
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


	
end et_stop_mask;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
