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

	
-- STEP 1 related:
------------------
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



-- STEP 2 related:
------------------
	-- For each "offset edge" there is another edge that intersects the 
	-- candidate edge. This composite type has a cursor that points to
	-- the next "offset edge" that is intersecting the candidate edge.
	-- It also provides via "place" the exact point of intersection:
	type type_next_intersection is record
		cursor : pac_offset_edges.cursor;
		place  : type_vector;
	end record;



	type type_edge_intersection (direct_available : boolean) is record
		-- An indirect intersection (via infinite long line) is
		-- ALWAYS THERE:
		indirect : type_next_intersection;

		-- A direct intersection MAY exist:
		case direct_available is
			when TRUE => 
				direct : type_next_intersection;
				
			when FALSE => null;
		end case;
	end record;


	-- The result of step 2 is a list of edge intersection:
	package pac_edge_intersections is new indefinite_doubly_linked_lists (type_edge_intersection);


	
-- STEP 3 related:
------------------	
	-- The result of step 2 is examined and converted to a list of
	-- final vertices. These vertices in turn are converted to the final polygon.
	-- See body of procedure offset_polygon for details.



	
	
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
