------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                      POLYGONS IN CONDUCTOR LAYERS                        --
--                                                                          --
--                               S p e c                                    --
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

package et_conductor_polygons is
	
	use pac_geometry_brd;
	use pac_geometry_2;
	use pac_polygons;

	
	-- Polygons in conductor layers have a dedicated type for the hatching:
	type type_polygon_conductor (fill_style : type_fill_style) 
		is new type_polygon with
	record

		-- the minimum width:
		width_min : type_track_width := type_track_width'first;

		-- the space between the polygon and foreign conductor objects:
		isolation : type_track_clearance := type_track_clearance'first; 
	
		easing : type_easing;
		
		case fill_style is
			when SOLID		=> null;
			when HATCHED	=> hatching : type_conductor_hatching;
		end case;
	end record;

	

-- SOLID CONDUCTOR POLYGONS
	type type_polygon_conductor_solid 
		is new type_polygon_conductor (fill_style => SOLID) with null record;

	package pac_conductor_polygons_solid is new doubly_linked_lists (type_polygon_conductor_solid);


	
	
-- HATCHED CONDUCTOR POLYGONS
	type type_polygon_conductor_hatched
	is new type_polygon_conductor (fill_style => HATCHED) with null record;

	package pac_conductor_polygons_hatched is new doubly_linked_lists (type_polygon_conductor_hatched);


	
	


	



	package pac_fill_lines is new doubly_linked_lists (type_line);

	no_fill_lines : constant pac_fill_lines.list := pac_fill_lines.empty_list;


-- HORIZONTAL FILL LINES
	package pac_h_lines is new doubly_linked_lists (type_line);

	-- Returns true if the given h_lines overlap each other in x direction.
	function overlap (
		hl_1, hl_2 : in pac_h_lines.cursor)
		return boolean;


	type type_row is record
		lines	: pac_h_lines.list;
	end record;

	package pac_rows is new doubly_linked_lists (type_row);


	
	
-- BORDERS
	package pac_border_lines is new doubly_linked_lists (type_line);

	type type_border is record
		border : pac_border_lines.list;
	end record;

	package pac_borders is new doubly_linked_lists (type_border);


-- FILL
	type type_fill is tagged record
		rows	: pac_rows.list;
		borders	: pac_borders.list;
	end record;



	
	
	
	-- The factor that causes the fill lines to overlap slightly.
	-- It is required in order to avoid a possible small gap between them
	-- that could occur during manufacturing.
	-- The lower the factor the more overlap. 1.0 means no overlap.
	fill_line_overlap_factor : constant type_distance_positive := 0.99;
	

														 
	
	procedure dummy;

	
end et_conductor_polygons;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
