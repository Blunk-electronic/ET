------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / POLYGONS / CLIPPING                    --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--


with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.unbounded;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with et_exceptions;				use et_exceptions;


package body et_geometry_2.polygons.clipping is

	use pac_polygon_segments;
	use pac_points;
	use pac_vertices;
	

	function to_string (intersection : in type_intersection)
		return string
	is begin
		return to_string (intersection.position) 
			& " " & type_direction'image (intersection.direction);
	end to_string;


	
	function to_string (vertex : in type_vertex)
		return string
	is begin
		return to_string (vertex.position) 
			& " " & type_direction'image (vertex.direction);
	end to_string;

	

	function to_string (vertices : in pac_vertices.list) return string is
		use ada.strings.unbounded;
		
		result : unbounded_string;
		
		procedure query_vertex (v : in pac_vertices.cursor) is begin
			result := result & " " 
				& trim (to_string (get_x (element (v).position)), left)
				& "/"
				& trim (to_string (get_y (element (v).position)), left)
				& " " & type_category'image (element (v).category)
				& " " & type_direction'image (element (v).direction);
		end query_vertex;
			
	begin
		vertices.iterate (query_vertex'access);

		return to_string (result);
	end to_string;


	
	function is_entering (v : pac_vertices.cursor) return boolean is begin
		if element (v).category = INTERSECTION 
		and element (v).direction = ENTERING then
			return true;
		else
			return false;
		end if;
	end is_entering;

	
	function is_leaving (v : pac_vertices.cursor) return boolean is begin
		if element (v).category = INTERSECTION 
		and element (v).direction = LEAVING then
			return true;
		else
			return false;
		end if;
	end is_leaving;


	
	procedure sort_by_distance (
		vertices	: in out pac_vertices.list;
		reference	: in type_point)
	is
		type type_item is record
			vertex		: type_vertex;
			distance	: type_distance_positive;
		end record;

		
		function "<" (left, right : in type_item) return boolean is begin
			if left.distance < right.distance then
				return true;
			else
				return false;
			end if;
		end;
	
			
		package pac_items is new doubly_linked_lists (type_item);
		use pac_items;
		
		items : pac_items.list;

		
		procedure query_vertex (v : in pac_vertices.cursor) is 
			d : type_distance_polar;
		begin
			d := get_distance (type_point (reference), type_point (element (v).position));
			
			items.append (new_item => (
				vertex		=> element (v),
				distance	=> get_absolute (d)));
		end query_vertex;

		

		package pac_sorting is new pac_items.generic_sorting;
		use pac_sorting;
		

		procedure query_item (i : in pac_items.cursor) is begin
			vertices.append (element (i).vertex);
		end query_item;
		
		
	begin
		-- Collect vertices and their distance to the reference
		-- in list "items":
		vertices.iterate (query_vertex'access);

		-- Sort items by distance to reference:
		sort (items);

		-- The old vertices are no longer required:
		vertices.clear;
		-- New vertices will be appended here.
		

		-- Traverse items and append them one by one to the
		-- list of vertices:
		items.iterate (query_item'access);
	end sort_by_distance;



	function to_polygon (vertices : in pac_vertices.list)
		return type_polygon
	is
		result : type_polygon;

		start : boolean := true;
		l : type_line;
		
		procedure query_vertex (v : in pac_vertices.cursor) is begin
			if start then
				l.start_point := element (v).position;
				start := false;
			else
				l.end_point := element (v).position;
				start := true;

				append (result.contours.segments, (LINE, l));
			end if;
		end query_vertex;
		
			
	begin
		vertices.iterate (query_vertex'access);

		l.start_point := last_element (vertices).position;
		l.end_point := first_element (vertices).position;
		
		append (result.contours.segments, (LINE, l));
		return result;
	end to_polygon;

	

	function clip (
		polygon_A	: in type_polygon'class;
		polygon_B	: in type_polygon'class)
		return pac_clipped.list
	is
		result : pac_clipped.list;

		intersections : pac_intersections.list;
		
		
		procedure query_A_segment (a : in pac_polygon_segments.cursor) is

			procedure query_B_segment (b : in pac_polygon_segments.cursor) is
				I2L : type_intersection_of_two_lines := get_intersection (
				   element (a).segment_line, element (b).segment_line);

				p : type_point;
				Q : type_inside_polygon_query_result;
				IAB : type_intersection;
			begin
				if I2L.status = EXISTS then
					p := to_point (I2L.intersection.vector);

					Q := in_polygon_status (polygon_B, element (a).segment_line.start_point);
					
					if Q.status = INSIDE then
						IAB := (P, LEAVING, element (a).segment_line, element (b).segment_line);
					else
						IAB := (P, ENTERING, element (a).segment_line, element (b).segment_line);
					end if;

					intersections.append (IAB);
					--put_line (to_string (IAB));
				end if;
			end query_B_segment;
			
		begin			
			polygon_B.contours.segments.iterate (query_B_segment'access);
		end query_A_segment;



		

		function get_intersections_on_edge (
			edge	: in type_line;
			AB		: in boolean) -- true -> polygon_A, false => polygon_B
			return pac_vertices.list
		is 
			result : pac_vertices.list;

			
			procedure query_intersection (i : in pac_intersections.cursor) is begin
				case AB is
					when TRUE =>
						if element (i).edge_A = edge then
							result.append (new_item => (
								position	=> element (i).position,
								category	=> INTERSECTION,
								direction	=> element (i).direction));
						end if;

					when FALSE =>
						if element (i).edge_B = edge then
							result.append (new_item => (
								position	=> element (i).position,
								category	=> INTERSECTION,
								direction	=> element (i).direction));

						end if;
				end case;
			end query_intersection;

			
		begin
			intersections.iterate (query_intersection'access);

			sort_by_distance (result, edge.start_point);
			return result;
		end get_intersections_on_edge;


		

		vertices_A, vertices_B : pac_vertices.list;
		

		procedure make_vertices_A is

			procedure query_segment (s : in pac_polygon_segments.cursor) is
				ip : pac_vertices.list;
			begin
				vertices_A.append (new_item => (
					position	=> element (s).segment_line.start_point,
					category	=> NORMAL,
					direction	=> ENTERING)); -- don't care
				
				ip := get_intersections_on_edge (element (s).segment_line, true);
				splice (target => vertices_A, before => pac_vertices.no_element, source => ip);
			end query_segment;
			
		begin
			polygon_A.contours.segments.iterate (query_segment'access);
		end make_vertices_A;
		

		procedure make_vertices_B is

			procedure query_segment (s : in pac_polygon_segments.cursor) is
				ip : pac_vertices.list;
			begin
				vertices_B.append (new_item => (
					position	=> element (s).segment_line.start_point,
					category	=> NORMAL,
					direction	=> ENTERING)); -- don't care
				
				ip := get_intersections_on_edge (element (s).segment_line, false);
				splice (target => vertices_B, before => pac_vertices.no_element, source => ip);
			end query_segment;
			
		begin
			polygon_B.contours.segments.iterate (query_segment'access);
		end make_vertices_B;


		
		vertice_A_cursor : pac_vertices.cursor;

		-- Returns a cursor to the first entering intersection in
		-- vertices_A:
		function get_first_entering return pac_vertices.cursor is
			v : pac_vertices.cursor := vertices_A.first;
		begin
			while v /= pac_vertices.no_element loop
				if is_entering (v) then
					exit;
				end if;
				next (v);
			end loop;

			return v;
		end get_first_entering;


		-- Returns the vertices (in vertices_A) from the entering vertex 
		-- to the next leaving vertex. The vertices are removed from
		-- vertices_A so that they won't be visited again:
		function get_until_leaving (entering : in pac_vertices.cursor)
			return pac_vertices.list 
		is
			v : pac_vertices.cursor;
			result : pac_vertices.list;

			procedure collect is begin
				while v /= pac_vertices.no_element loop
					result.append (element (v));
					
					if is_leaving (v) then
						exit;
					end if;

					next (v);
				end loop;
			end collect;
			
		begin
			-- Preset cursor v to the given entering vertex:
			v := entering;

			-- Collect vertices from entering vertex to
			-- the next leaving vertex:
			collect;

			-- If no leaving vertex found (until end of list),
			-- restart the search from the top of the list:
			if v = pac_vertices.no_element then
				v := vertices_A.first;
				collect;
			end if;
			
			-- Remove the vertices from vertices_A:
			v := entering;
			vertices_A.delete (position => v, count => length (result));
			
			return result;
		end get_until_leaving;
		

		-- Returns the vertices (in vertices_B) between the leaving vertex 
		-- and the next entering vertex. The vertices are removed from
		-- vertices_B so that they won't be visited again:
		function get_until_entering (leaving : in pac_vertices.cursor)
			return pac_vertices.list 
		is
			v : pac_vertices.cursor;
			result : pac_vertices.list;

			procedure collect is begin
				while v /= pac_vertices.no_element loop
					result.append (element (v));
					
					if is_entering (v) then
						exit;
					end if;

					next (v);
				end loop;
			end collect;
			
		begin
			-- Preset cursor v to the given leaving vertex:
			v := leaving;

			-- Collect vertices from leaving vertex to
			-- the next entering vertex:
			collect;

			-- If no entering vertex found (until end of list),
			-- restart the search from the top of the list:
			if v = pac_vertices.no_element then
				v := vertices_B.first;
				collect;
			end if;
			
			-- Remove the collected vertices from vertices_B:
			v := leaving;
			vertices_B.delete (position => v, count => length (result));

			-- The first and the last vertex of the result are 
			-- not required:
			result.delete_first;
			result.delete_last;
			
			return result;
		end get_until_entering;


		v_tmp : type_vertex;
		vertices_tmp_1, vertices_tmp_2 : pac_vertices.list;
		
	begin

		-- Find intersections between the two polygons:
		polygon_A.contours.segments.iterate (query_A_segment'access);


		make_vertices_A;
		--put_line ("A: " & to_string (vertices_A));

		make_vertices_B;
		--put_line ("B: " & to_string (vertices_B));


		vertice_A_cursor := get_first_entering;

		--put_line ("first entering " & to_string (element (vertice_A_cursor)));


		v_tmp := element (vertice_A_cursor);

		vertices_tmp_1 := get_until_leaving (vertice_A_cursor);
		vertices_tmp_2 := get_until_entering (vertices_B.find (vertices_tmp_1.last_element));
		splice (
			target	=> vertices_tmp_1, 
			before	=> pac_vertices.no_element, 
			source 	=> vertices_tmp_2);

		--put_line (to_string (vertices_tmp_1));

		--result.append (to_polygon (vertices_tmp_1));
		put_line (to_string (to_polygon (vertices_tmp_1)));
		
		return result;
	end clip;

	
end et_geometry_2.polygons.clipping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
