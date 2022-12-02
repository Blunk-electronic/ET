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
with et_pcb_stack;
with et_geometry;				use et_geometry;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_design_rules;			use et_design_rules;

package et_fill_zones is
	
	use pac_geometry_brd;
	use pac_contours;
	
	use pac_polygons;
	use pac_polygon_offsetting;
	use pac_polygon_list;
	
	package pac_stripes is new doubly_linked_lists (pac_geometry_brd.type_line);

	no_stripes : constant pac_stripes.list := pac_stripes.empty_list;



	-- The fill zone may disintegrate into smaller islands.
	-- In the best case there is only one island.
	type type_island is record
		
		-- An island has a single outer border (like a shoreline):
		outer_border	: type_polygon;
		
		-- An island may have multiple inner areas which are not filled.
		-- They are a result of holes in the PCB, tracks, pads, vias, ...
		-- We call such a void area an inner border (like a lake inside an island).
		-- There may be several of them inside the island:
		inner_borders	: pac_polygon_list.list; -- CS rename to lakes ?

		-- The horizontal lines that fill the conducting area of the island:		
		stripes			: pac_stripes.list;
	end record;


	
	package pac_islands is new doubly_linked_lists (type_island);
	use pac_islands;
	
	no_islands : constant pac_islands.list := pac_islands.empty_list;

	
	-- Iterates the islands. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		islands	: in pac_islands.list;
		process	: not null access procedure (position : in pac_islands.cursor);
		proceed	: not null access boolean);

	

	type type_style (style : type_fill_style) is record
		linewidth : type_track_width;
		
		case style is
			when SOLID	 => null;
			when HATCHED => spacing : type_track_clearance;
		end case;
	end record;
	


	
	procedure make_stripes (
		island	: in out type_island;
		style	: in type_style);

	
	procedure fill_island (
		islands		: in out pac_islands.list;
		position	: in pac_islands.cursor;
		style		: in type_style;
		process		: not null access procedure (
						island	: in out type_island;
						style	: in type_style));


	
-- A FILL ZONE IN GENERAL


	keyword_isolation	: constant string := "isolation";

	
	type type_zone (fill_style : type_fill_style) 
		is new type_contour with -- outer contour as drawn by the operator
	record

		-- the width of borders and fill stripes:
		linewidth : type_track_width := type_track_width'first;

		-- the space between the fill_zone and foreign conductor objects:
		isolation : type_track_clearance := type_track_clearance'first; 
	
		easing : type_easing;

		islands : pac_islands.list := no_islands;
		
		case fill_style is
			when SOLID		=> null;
			when HATCHED	=> spacing : type_track_clearance;
		end case;
	end record;



	function get_half_linewidth (
		zone	: in type_zone)
		return type_float_internal_positive;
	

-- QUERY POINT TO ZONE LOCATION

	-- Returns true if the given point lies between
	-- the islands. If the point lies exactly on the edge
	-- of an island, then it is regarded as lying ON the island,
	-- thus the return would be false.
	-- Takes the real conducting area of the island into account,
	-- means, the full width of the border is considered as part
	-- of the conducting area:
	function between_islands (
		zone	: in type_zone;
		point	: in type_vector;
		debug	: in boolean := false)
		return boolean;


	-- If the given point lies inside an inner border,
	-- then this function returns the cursor to that inner border.
	-- If the point lies elsewhere, then the result is no_element.
	-- Takes the real conducting area of the surrounding island into account,
	-- means, the full width of the border is considered as part
	-- of the conducting area:
	function get_inner_border (
		zone	: in type_zone;
		point	: in type_vector;
		debug	: in boolean := false)
		return pac_polygon_list.cursor;
	
								  
	type type_location is (
		-- A place where conducting material is.
		-- If the place is exactly on the edge between
		-- conducting and non-conducting area then
		-- it is regarded as INSIDE the conducting area:
		CONDUCTING_AREA, 

		-- A place where no conducting material is. This
		-- can be between islands or inside inner borders:
		NON_CONDUCTING_AREA

		-- NOTE: The border of islands and inner borders
		-- has a certain linewidth. The full linewidth
		-- IS taken into account when it is about this
		-- type of location !
		-- The subprograms that use type_location regard
		-- the full linewidth as conducting area !
		); 

	

	-- Tests the given point whether it is in the conducting
	-- area of a zone or outside the conducting area.
	-- See details in type specification of type_location:
	function get_location (
		zone	: in type_zone;
		point	: in type_vector;
		debug	: in boolean := false)
		return type_location;


										
	type type_distance_to_conducting_area (
		edge_exists : boolean;
		centerline_exists : boolean)
	is record
		case edge_exists is
			when TRUE => 
				distance_to_edge : type_float_internal_positive;

				case centerline_exists is
					when TRUE => 
						distance_to_centerline : type_float_internal_positive;
					when FALSE => null;
				end case;

			when FALSE => null;
		end case;
	end record;	


	in_conducting_area : constant type_distance_to_conducting_area := (
		edge_exists				=> true,
		distance_to_edge		=> 0.0,
		centerline_exists		=> true,
		distance_to_centerline	=> 0.0);

	
	-- Returns the distance of a point to the nearest
	-- island into the given direction.
	-- Assumes, the given start point is somewhere between the islands.
	-- Returns just "false" if no island found in
	-- the given direction.
	-- CS return
	-- See details in type specification of type_location:
	function get_distance_to_nearest_island (
		zone		: in type_zone;
		start_point	: in type_vector;
		direction	: in type_angle;
		debug		: in boolean := false)
		return type_distance_to_conducting_area;


	
	-- Returns the distance to the conducting area
	-- of a ray starting at start_point and running into
	-- given direction. If the start point is already in
	-- a conducting area, then the returned distance is zero.
	-- Returns just "false" if no conducting area found in
	-- the given direction.
	-- See details in type specification of type_location:
	function get_distance_to_conducting_area (
		zone		: in type_zone;
		start_point	: in type_vector;
		direction	: in type_angle;
		debug		: in boolean := false)
		return type_distance_to_conducting_area;
	
	
-- SOLID FILLED ZONE:
	
	type type_zone_solid
		is new type_zone (fill_style => SOLID) with null record;

	package pac_zones_solid is new doubly_linked_lists (type_zone_solid);



	
-- HATCHED FILL ZONE:
	
	type type_zone_hatched
		is new type_zone (fill_style => HATCHED) with null record;

	package pac_zones_hatched is new doubly_linked_lists (type_zone_hatched);

														 
	
end et_fill_zones;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
