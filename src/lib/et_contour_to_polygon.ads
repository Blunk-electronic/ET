------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      CONTOUR TO POLYGON CONVERSION                       --
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


with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_pcb_coordinates;		use et_pcb_coordinates;


package et_contour_to_polygon is


	use pac_geometry_2;
	
	use pac_contours;
	use pac_segments;
	
	use pac_polygons;
	use pac_edges;


	function to_edge (
		line : in type_line)
		return type_edge;

	
	function to_line (
		edge : in type_edge)
		return type_line;

	
	-- Converts an arc to a list of edges.
	-- The edges start on the start point of the arc
	-- and end on the end point of the arc.
	-- The accuracy is determined by the given tolerance.
	-- The tolerance is the maximum allowed deviation from
	-- the ideal arc:
	function to_edges (
		arc			: in type_arc;
		tolerance	: in type_distance_positive;
		debug		: in boolean := false)				  
		return pac_edges.list;
	

	-- Converts a circle to a list of edges in CCW order.
	-- The accuracy is determined by the given tolerance.
	-- The tolerance is the maximum allowed deviation from
	-- the ideal circle:
	function to_edges (
		circle		: in type_circle;
		tolerance	: in type_distance_positive;
		debug		: in boolean := false)				  
		return pac_edges.list;

	
	-- Converts a contour to a polygon.
	-- A contour consists of line and arc segments. Since polygons 
	-- consist of edges (lines) only, special treatment is required 
	-- in order to model a arcs:
	--  Each arc segment is replaced by many short line segments.
	-- For arc conversion the accuracy is determined by the given tolerance.
	-- The tolerance is the maximum allowed deviation from the ideal arc.
	-- The edges of the returned polygon are ordered counter-clockwise:
	function to_polygon (
		contour		: in type_contour'class;
		tolerance	: in type_distance_positive;
		debug		: in boolean := false)					
		return type_polygon;


	-- Converts a polygon to a contour.
	-- Since a polygon consists of edges (lines) only,
	-- the resulting contour will also contain only line segments.
	-- The winding of the returned contour is CCW:
	function to_contour (
		polygon	: in type_polygon;
		debug	: in boolean := false)					
		return type_contour;
	
	
end et_contour_to_polygon;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
