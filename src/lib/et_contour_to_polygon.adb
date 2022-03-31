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
		arc		: in type_arc;
		debug	: in boolean := false)				  
		return pac_edges.list
	is
		use pac_functions_distance;
		
		result : pac_edges.list;

		arc_angles : constant type_arc_angles := to_arc_angles (arc);

		span : type_float_internal;
		betha : type_float_internal;
		f_tol : constant type_float_internal := type_float_internal (fab_tolerance);
		radius : constant type_float_internal := type_float_internal (arc_angles.radius);
		
		edge_ct_float : type_float_internal;
		edge_ct_final : positive; -- CS subtype ?
		
		angle : type_float_internal;
	begin
		span := type_float_internal (get_span (arc));
		betha := 90.0 - arcsin ((radius - f_tol) / radius, units_per_cycle);

		edge_ct_float := span / betha;
		edge_ct_final := positive (type_float_internal'ceiling (edge_ct_float));

		angle := span / type_float_internal (edge_ct_final);
		
		if debug then
			--put_line ("arc    : " & to_string (arc));
			put_line ("arc    : " & to_string (arc_angles));
			put_line ("span   : " & to_string (span));
			put_line ("betha  : " & to_string (betha));
			--put_line ("edge ct float: " & to_string (edge_ct_float));
			put_line ("edge ct final: " & positive'image (edge_ct_final));
			put_line ("alpha  : " & to_string (angle));
		end if;
		

		
		return result;
	end to_edges;

	
	
	function to_polygon (
		contour	: in type_contour'class;
		debug	: in boolean := false)					
		return type_polygon
	is
		result : type_polygon;

		procedure query_segment (c : in pac_contour_segments.cursor) is
			s : type_contour_segment := element (c);
			e_list : pac_edges.list;
		begin
			case s.shape is
				when LINE =>
					-- Append the segment line as it is to the 
					-- edges of the resulting polygon:
					result.edges.append (s.segment_line);

				when ARC =>
					-- Convert the arc to a list of small lines
					-- and append this list to the edges of the 
					-- resulting polygon:
					e_list := to_edges (s.segment_arc, debug);
					
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
			l :  type_line := element (c);
		begin
			if debug then
				put_line (to_string (l));
			end if;

			-- Add the edge as a line segment to the contour:
			result.contour.segments.append ((
				shape			=> LINE,
				segment_line	=> l));
			
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
