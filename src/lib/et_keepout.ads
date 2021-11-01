------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              KEEPOUT                                     --
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


package et_keepout is
	
	use pac_geometry_brd;

	use pac_geometry_2;
	use pac_polygons;
	use pac_text_fab;

	
	-- GUI relevant only: The line width of keepout:
	keepout_line_width : constant type_general_line_width := text_parameters_fab.width_min;

	type type_keepout_line is new type_line with null record;
	package pac_keepout_lines is new doubly_linked_lists (type_keepout_line);

	type type_keepout_arc is new type_arc with null record;
	package pac_keepout_arcs is new doubly_linked_lists (type_keepout_arc);
	
	package pac_keepout_circles is new doubly_linked_lists (type_fillable_circle_solid);

	type type_keepout_polygon is new type_polygon with null record;
	package pac_keepout_polygons is new doubly_linked_lists (type_keepout_polygon);
	
	package pac_keepout_cutouts is new doubly_linked_lists (type_polygon);	
	
	type type_keepout is tagged record
		lines 		: pac_keepout_lines.list;
		arcs		: pac_keepout_arcs.list;
		circles		: pac_keepout_circles.list;
		polygons	: pac_keepout_polygons.list;
		cutouts 	: pac_keepout_cutouts.list;
	end record;



	-- Logs the properties of the given line of keepout
	procedure line_keepout_properties (
		face			: in type_face;
		cursor			: in pac_keepout_lines.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given arc of keepout
	procedure arc_keepout_properties (
		face			: in type_face;
		cursor			: in pac_keepout_arcs.cursor;
		log_threshold 	: in type_log_level);

	-- Logs the properties of the given circle of keepout
	procedure circle_keepout_properties (
		face			: in type_face;
		cursor			: in pac_keepout_circles.cursor;
		log_threshold 	: in type_log_level);


	
end et_keepout;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
