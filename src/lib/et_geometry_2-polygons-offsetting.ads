------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / POLYGONS / OFFSETTING                  --
--                                                                          --
--                               S p e c                                    --
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


generic
	
package et_geometry_2.polygons.offsetting is
	
	-- See:
	-- <https://gis.stackexchange.com/questions/61786/how-to-scale-reduce-my-polygon-without-changing-the-central-lat-long>
	-- <https://stackoverflow.com/questions/54033808/how-to-offset-polygon-edges>

	-- CS subtype for offset

	-- A single edge of a polygon is offset so that a new
	-- edge is formed. It runs parallel to the original edge but
	-- is shifted to the left or right (according to the desired offset).
	-- An intersection of this edge with another "offset edge" is called 
	-- a "direct intersection".
	-- Furthermore we get an infinite long line (overlapping the edge).
	-- An intersection of this line with the previous line can be considered
	-- as a new vertex.
	type type_offset_edge is record
		-- The new edge after the offset operation:
		edge	: type_edge;

		-- The new edge expressed as an infinite long line:
		line	: type_line_vector;
	end record;

	function to_string (oe : in type_offset_edge) return string;

	-- When preprocessing the polygon for each edge an "offset edge" is
	-- created and stored in a list:
	package pac_offset_edges is new doubly_linked_lists (type_offset_edge);
	use pac_offset_edges;

	function to_string (oe : in pac_offset_edges.cursor) return string;

		
	type type_next_intersection is record
		cursor : pac_offset_edges.cursor;
		place  : type_vector;
	end record;


	-- Looks for a direct intersection after the edge "start"
	-- in counter-clockwise direction. If there is a direct
	-- intersection then the cursor to the corresponding "offset edge"
	-- along with the location vector of the actual intersection is returned. 
	-- If no direct intersection found, returns cursor no_element. The returned
	-- location vector is then irrelevant:
	function get_next_direct_intersection (
		start	: in pac_offset_edges.cursor;
		first	: in pac_offset_edges.cursor;
		debug	: in boolean := false)
		return type_next_intersection;


	
	type type_edge_intersection (direct_available : boolean) is record
		--edge_indirect : pac_offset_edges.cursor;
		
		-- An indirect intersection (via infinite long line) is
		-- always there:
		--place_indirect : type_vector;
		indirect : type_next_intersection;
		
		case direct_available is
			when TRUE => 
				--place_direct : type_vector;
				direct : type_next_intersection;
				
			when FALSE => null;
		end case;
	end record;

	
	package pac_edge_intersections is new indefinite_doubly_linked_lists (type_edge_intersection);


	--function get_relevant (
		--intersection : in pac_edge_intersections.cursor) 
		--return type_vector;
	
	
	-- Offsets (the edges of) a polygon. 
	-- If offset is positive then the edges are moved toward the outside
	-- of the polygon. The polygon area then becomes greater.
	-- If offset is negative then the edges are moved inside. The polygon area
	-- then shrinks.
	procedure offset_polygon (
		polygon		: in out type_polygon;
		offset		: in type_distance;
		debug		: in boolean := false);


	-- Offsets a list of polygons:
	function offset_polygons (
		polygons	: in pac_polygons.list;
		offset		: in type_distance)
		return pac_polygons.list;

	
	procedure offset_polygons (
		polygons	: in out pac_polygons.list;
		offset		: in type_distance);

private
	
	type type_mode is (
		EXPAND,		-- polygon area becomes greater
		SHRINK,		-- polygon area becomes smaller
		NOTHING);	-- no change

	-- Converts the given offset to type mode.
	-- If offset is greater zero, then returns EXPAND.
	-- If offset is less than zero, then returns SHRINK.
	-- If offset is zero, then returns NOTHING:
	function to_mode (
		offset : in type_distance)
		return type_mode;

	
end et_geometry_2.polygons.offsetting;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
