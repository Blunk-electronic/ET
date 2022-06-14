------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             FILL ZONES                                   --
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


with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_string_processing;		use et_string_processing;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;				use et_geometry;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_design_rules;			use et_design_rules;

package et_fill_zones is
	
	use pac_geometry_brd;
	use pac_geometry_2;
	use pac_contours;
	use pac_polygons;


	procedure dummy;

	
	-- The factor that causes the fill stripes to overlap slightly.
	-- It is required in order to avoid a possible small gap between them
	-- that could occur during manufacturing.
	-- The lower the factor the more overlap. 1.0 means no overlap.
	overlap_factor : constant type_distance_positive := 0.99;
	
	
	package pac_stripes is new doubly_linked_lists (pac_geometry_brd.type_line);

	no_stripes : constant pac_stripes.list := pac_stripes.empty_list;

	-- The fill zone may disintegrate into smaller islands.
	-- In the best case there is only one island.
	-- Each island has an outer border (which is basically a polygon):
	type type_outer_border is new type_polygon;

	-- An island may have multiple inner areas which are not filled.
	-- They are usually a result of holes in the PCB, tracks, pads, vias, ...
	-- Such a cutout area is an inner border (which is basically a polygon):
	type type_inner_border is new type_polygon;

	-- Since we have lots of those inner cutout areas we store them in a list:
	package pac_inner_borders is new doubly_linked_lists (type_inner_border);


	type type_island is record
		border	: type_outer_border;
		cutouts	: pac_inner_borders.list;
		stripes	: pac_stripes.list;
	end record;
		
	package pac_islands is new doubly_linked_lists (type_island);

	no_fill : constant pac_islands.list := pac_islands.empty_list;
	-- CS rename to no_islands


	
-- A FILL ZONE IN GENERAL
	
	type type_zone (fill_style : type_fill_style) 
		is new type_contour with -- outer contour as drawn by the operator
	record

		-- the minimum width of borders and fill lines:
		width_min : type_track_width := type_track_width'first;

		-- the space between the fill_zone and foreign conductor objects:
		isolation : type_track_clearance := type_track_clearance'first; 
	
		easing : type_easing;

		fill : pac_islands.list := no_fill; -- CS rename to islands
		
		case fill_style is
			when SOLID		=> null;
			when HATCHED	=> hatching : type_conductor_hatching;
		end case;
	end record;

	

	
-- SOLID FILLED ZONE:
	
	type type_zone_solid
		is new type_zone (fill_style => SOLID) with null record;

	package pac_zones_solid is new doubly_linked_lists (type_zone_solid);



	
-- HATCHED FILL ZONE:
	
	type type_zone_hatched
		is new type_zone (fill_style => HATCHED) with null record;

	package pac_zones_hatched is new doubly_linked_lists (type_zone_hatched);


	
	


	



	--package pac_fill_lines is new doubly_linked_lists (type_line);

	--no_fill_lines : constant pac_fill_lines.list := pac_fill_lines.empty_list;


-- HORIZONTAL FILL LINES
	--package pac_h_lines is new doubly_linked_lists (type_line);


	--type type_side is (LEFT, RIGHT);
	
	
	-- Iterates the h_lines. Aborts the process when the proceed-flag goes false:
	--procedure iterate (
		--h_lines	: in pac_h_lines.list;
		--side	: in type_side;				  
		--process	: not null access procedure (position : in pac_h_lines.cursor);
		--proceed	: not null access boolean);


	
	-- Returns true if the given h_lines overlap each other in x direction.
	--function overlap (
		--hl_1, hl_2 : in pac_h_lines.cursor)
		--return boolean;


	--type type_row is record
		--lines	: pac_h_lines.list;
	--end record;

	--package pac_rows is new doubly_linked_lists (type_row);



	--type type_adjacent is (ABOVE, BELOW);

	
	--function get_adjacent_h_line (
		--row		: in pac_rows.cursor;
		--h_line	: in pac_h_lines.cursor;							 
		--place	: in type_adjacent;
		--side	: in type_side)
		--return pac_h_lines.cursor;

	
	
-- BORDERS
	--package pac_border_lines is new doubly_linked_lists (type_line);

	--type type_border is record
		--border : pac_border_lines.list;
	--end record;

	--package pac_borders is new doubly_linked_lists (type_border);


---- FILL
	--type type_fill is tagged record
		--rows	: pac_rows.list;
		--borders	: pac_borders.list;
	--end record;



	
	
	
	

														 
	
end et_fill_zones;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
