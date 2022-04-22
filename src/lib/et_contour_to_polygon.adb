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

package body et_contour_to_polygon is


	function to_edges (
		arc			: in type_arc;
		tolerance	: in type_distance_positive;
		debug		: in boolean := false)				  
		return pac_edges.list
	is
		use pac_functions_distance;

		-- This is the list of edges to be returned:
		result : pac_edges.list;

		-- Convert the given tolerance to a float type:
		f_tol : constant type_float_internal := type_float_internal (tolerance);

		-- The given arc is usually offset from the origin.
		-- This offset is later required to create the actual edges 
		-- where the given arc is:
		arc_offset : constant type_distance_relative := to_distance_relative (arc.center);

		-- Make a copy of the given arc with its center on the origin (0/0):
		arc_origin : constant type_arc := type_arc (move_to (arc, origin));

		-- Get the start and end angles of the arc:
		arc_angles : constant type_arc_angles := to_arc_angles (arc_origin);

		-- Get the radius of the given angle:
		radius : constant type_float_internal := type_float_internal (arc_angles.radius);
		
		-- This is the total angle between start and end point of the given arc:
		span : type_float_internal;


		
		-- In order to compute the nunber of edges and the angle between
		-- their start and end points these variable are required:
		
		edge_ct_float : type_float_internal; -- number of segments
		edge_ct_final : positive; -- real number of segments
		-- CS subtype ?

		-- The minimal and final angle between start and end 
		-- point of a single edge:
		angle_min : type_float_internal;
		angle_final : type_float_internal;


		-- The edges will be build on these location vectors:
		p_start, p_walk, p_walk_previous : type_vector;


		-- Rotates the given location vector by angle_final * m.
		-- m is a multipier according to the current edge being built:
		function rotate (
			p : in type_vector;
			m : in positive)
			return type_vector
		is 
			result : type_vector := p;
		begin
			case arc.direction is
				when CW =>
					rotate_by (result, - angle_final * type_float_internal (m)); 

				when CCW =>
					rotate_by (result, + angle_final * type_float_internal (m)); 
			end case;

			return result;
		end rotate;


		-- Builds a final edge from the current location vectors 
		-- p_walk and p_walk_previous and appends it to the result:
		procedure make_edge is
			edge : type_edge;
		begin
			-- CS: Improvement required: It would be sufficient to call move_by
			-- only once. The start point of an edge is the same as the end point
			-- of the previous edge.
			edge.start_point := move_by (p_walk_previous, arc_offset);
			edge.end_point   := move_by (p_walk,          arc_offset);

			result.append (edge);

			if debug then
				put_line ("edge: " & to_string (edge));
			end if;
		end make_edge;
		
		
	begin
		-- Get the span of the arc:
		span := type_float_internal (get_span (arc_origin));

		-- Compute the minimal angle required between the segments:
		angle_min := 90.0 - arcsin ((radius - f_tol) / radius, units_per_cycle);

		-- Compute the number of edges required. The result is not an integer.
		-- So we must round up to the nearest integer:
		edge_ct_float := span / angle_min;
		edge_ct_final := positive (type_float_internal'ceiling (edge_ct_float));

		-- Compute the number of edges:
		angle_final := span / type_float_internal (edge_ct_final);

		
		if debug then
			--put_line ("arc    : " & to_string (arc));
			put_line ("arc          : " & to_string (arc_angles));
			put_line ("span         : " & to_string (span));
			put_line ("angle_min    : " & to_string (angle_min));
			put_line ("fab tol      : " & to_string (tolerance));
			--put_line ("edge ct float: " & to_string (edge_ct_float));
			put_line ("edge ct final: " & positive'image (edge_ct_final));
			put_line ("angle final  : " & to_string (angle_final));
		end if;


		-- The start point of the first edge.
		-- Further start and end points will be computed by
		-- rotating this point about the origin:
		p_start := to_vector (arc_origin.start_point);
		
		p_walk_previous := p_start;

		-- We rotate p_start as many times as edges are required:
		for e in 1 .. edge_ct_final loop

			-- For each pass the multipier increases:
			p_walk := rotate (p_start, e);

			make_edge;

			p_walk_previous := p_walk;
		end loop;
		
		return result;
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
		-- Iterate the contour segments:
		contour.contour.segments.iterate (query_segment'access);
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
