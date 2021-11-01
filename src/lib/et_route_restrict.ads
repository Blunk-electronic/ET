------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          ROUTE RESTRICT                                  --
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

with ada.strings;	 			use ada.strings;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;				use et_geometry;
with et_pcb_stack;				use et_pcb_stack;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_text;
with et_conductor_text;			use et_conductor_text;
with et_logging;				use et_logging;


package et_route_restrict is

	use pac_geometry_brd;

	use pac_geometry_2;
	use pac_polygons;
	use pac_text_fab;

	
	-- GUI relevant only: The line width of route restrict:
	route_restrict_line_width : constant type_general_line_width := text_parameters_fab.width_min;

	
	type type_route_restrict_line is new type_line with null record;
	
	package pac_route_restrict_lines is new doubly_linked_lists (type_route_restrict_line);

	
	type type_route_restrict_arc is new type_arc with null record;
	
	package pac_route_restrict_arcs is new doubly_linked_lists (type_route_restrict_arc);

	
	type type_route_restrict_circle is new type_fillable_circle_solid with null record;
	
	package pac_route_restrict_circles is new doubly_linked_lists (type_route_restrict_circle);

	
	type type_route_restrict_polygon is new type_polygon with null record;

	package pac_route_restrict_polygons is new doubly_linked_lists (type_route_restrict_polygon);

	
	type type_route_restrict_cutout is new type_polygon with null record;
		
	package pac_route_restrict_cutouts is new doubly_linked_lists (type_route_restrict_cutout);


	

	



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
