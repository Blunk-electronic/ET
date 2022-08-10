------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           PCB CONTOURS                                   --
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
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_contour_to_polygon;


package et_pcb_contour is

	use pac_geometry_brd;

	use pac_geometry_2;
	use pac_contours;
	use pac_polygons;
	use pac_polygon_offsetting;
	use pac_text_fab;


	-- As a safety measure we derive dedicated types for
	-- the outer and inner edge of the PCB from the general contour type.
	
	-- There is only one outer contour of a PCB:
	type type_outer_contour is new type_contour with null record;


	
	-- There can be many holes inside the PCB area:
	-- This is a single hole:
	type type_hole is new type_contour with null record;

	-- These are all holes inside the PCB area:
	package pac_holes is new doubly_linked_lists (type_hole);
	use pac_holes;


	
	-- In order to handle fill zones, the holes must be converted to
	-- a list of polygons:
	package pac_holes_as_polygons is new doubly_linked_lists (type_polygon);
	use pac_holes_as_polygons; -- CS rename to pac_holes

	
	-- Iterates the holes. Aborts the process when the proceed-flag goes false:
	--procedure iterate (
		--holes	: in pac_holes_as_polygons.list;
		--process	: not null access procedure (position : in pac_holes_as_polygons.cursor);
		--proceed	: not null access boolean);
	
	
	-- Converts a list of holes to a list of polygons:
	function to_polygons (
		holes		: in pac_holes.list;
		tolerance	: in type_distance_positive)
		return pac_holes_as_polygons.list;


	-- Offsets a list of holes (in the form of polygons).
	-- The parameter "offset" is always positive because
	-- holes can only become greater:
	procedure offset_holes (
		holes		: in out pac_holes_as_polygons.list;
		offset		: in type_distance_positive;
		debug		: in boolean := false);

	
		
	-- GUI relevant only: The line width of contours:
	pcb_contour_line_width : constant type_general_line_width := text_parameters_fab.width_min;

	
	
	type type_contour_text 
		is new pac_text_fab.type_text_fab_with_content with
	record
		vectors	: type_vector_text;		
	end record;

	package pac_contour_texts is new doubly_linked_lists (type_contour_text);

	
end et_pcb_contour;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
