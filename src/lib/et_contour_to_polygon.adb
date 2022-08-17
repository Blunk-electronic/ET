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

with ada.text_io;				use ada.text_io;
with et_geometry;				use et_geometry;


package body et_contour_to_polygon is

	function to_edge (
		line : in type_line)
		return type_edge
	is
		result : type_edge;
	begin
		result.start_point := to_vector (line.start_point);
		result.end_point := to_vector (line.end_point);
		return result;
	end to_edge;
	

	function to_line (
		edge : in type_edge)
		return type_line
	is
		result : type_line;
	begin
		result.start_point := to_point (edge.start_point);
		result.end_point := to_point (edge.end_point);
		return result;
	end to_line;
	

	function to_edges (
		arc			: in type_arc;
		tolerance	: in type_distance_positive;
		debug		: in boolean := false)				  
		return pac_edges.list
	is
		use pac_geometry_brd;

		-- Convert the given tolerance to a float type:
		f_tol : constant type_float_internal_positive := type_float_internal (tolerance);
	begin
		return to_edges (to_arc_fine (arc), f_tol, debug);
	end to_edges;


	function to_edges (
		circle		: in type_circle;
		tolerance	: in type_distance_positive;
		debug		: in boolean := false)				  
		return pac_edges.list
	is
		-- This is the list of edges to be returned:
		result : pac_edges.list;

		-- Convert the given circle to an arc (same start and end point):
		arcs : type_arcs (1 .. 2);

		edges_left, edges_right : pac_edges.list;
		
	begin
		arcs := split_circle (circle);
		
		if debug then
			put_line ("arc left : " & to_string (arcs (1)));
			put_line ("arc right: " & to_string (arcs (2)));
		end if;

		-- The left arc (1) runs from the top to the bottom.
		-- The right arc (2) runs from bottom to top.
		edges_left  := to_edges (arcs (1), tolerance, debug);
		edges_right := to_edges (arcs (2), tolerance, debug);

		-- Join the two arcs to a single one:
		edges_left.splice (before => pac_edges.no_element, source => edges_right);

		-- The edges are ordered counter-clockwise.
		return edges_left;
	end to_edges;
	
	
	function to_polygon (
		contour		: in type_contour'class;
		tolerance	: in type_distance_positive;
		debug		: in boolean := false)					
		return type_polygon
	is
		result : type_polygon;

		procedure query_segment (c : in pac_contour_segments.cursor) is
			s : type_contour_segment := element (c);
			e_list : pac_edges.list;
		begin
			case s.shape is
				when LINE =>
					-- Convert the line to an edge and append to
					-- edges of the resulting polygon:
					result.edges.append (to_edge (s.segment_line));

				when ARC =>
					-- Convert the arc to a list of small lines
					-- and append this list to the edges of the 
					-- resulting polygon:
					e_list := to_edges (s.segment_arc, tolerance, debug);
					
					result.edges.splice (
						before	=> pac_edges.no_element,					
						source	=> e_list);
			end case;			
		end query_segment;
		
		
	begin
		if contour.contour.circular then

			if debug then
				new_line;
				put_line ("converting circular contour to polygon ...");
			end if;
			
			-- Convert the single circle of the given contour
			-- to a list of edges:
			result.edges := to_edges (contour.contour.circle, tolerance, debug);
		else
			-- Iterate the contour segments:
			contour.contour.segments.iterate (query_segment'access);
		end if;


		-- Whatever the winding of the given contour was,
		-- we set the winding to the system wide default:
		set_winding (result);
		
		-- check edge lengths, remove edges too short ?
		
		return result;
	end to_polygon;


	function to_contour (
		polygon	: in type_polygon;
		debug	: in boolean := false)					
		return type_contour
	is
		result : type_contour;

		procedure query_edge (c : in pac_edges.cursor) is
			l :  type_edge := element (c); -- CS use rename
		begin
			if debug then
				put_line (to_string (l));
			end if;

			-- Add the edge as a line segment to the contour:
			result.contour.segments.append ((
				shape			=> LINE,
				segment_line	=> to_line (l)));
			
		end query_edge;

		
	begin
		-- Iterate all edges of the given polygon:
		polygon.edges.iterate (query_edge'access);
		return result;
	end to_contour;

	
	
end et_contour_to_polygon;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
