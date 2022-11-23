------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             FILL ZONES                                   --
--                                                                          --
--                              B o d y                                     --
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


package body et_fill_zones is


	procedure iterate (
		islands	: in pac_islands.list;
		process	: not null access procedure (position : in pac_islands.cursor);
		proceed	: not null access boolean)
	is
		c : pac_islands.cursor := islands.first;
	begin
		while c /= pac_islands.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;

	
	procedure make_stripes (
		island	: in out type_island;
		style	: in type_style)
	is		
		-- The boundaries of the island (greatest/smallest x/y):
		boundaries : constant type_boundaries := get_boundaries (island.outer_border);

		height : constant type_float_internal_positive := get_height (boundaries);
		bottom : constant type_float_internal := get_bottom (boundaries);

		--effective_width : type_float_internal_positive;

		-- The factor that causes the fill stripes to overlap slightly.
		-- It is required in order to avoid a possible small gap between them
		-- that could occur during manufacturing.
		-- The lower the factor the more overlap. 1.0 means no overlap.
		--overlap_factor : constant type_float_internal_positive := 0.99;

		
		stripe_count_rational : type_float_internal_positive;
		stripe_count_natural : natural;
		stripe_spacing : type_float_internal_positive;

		-- The point at which a stripe starts. We will fill the island from bottom to top.
		-- The lowest left place to start from is outside the island:
		start_point : type_vector := set (
			 x => get_left (boundaries) - 1.0, -- left of the island
			 y => bottom); -- just a default. will be changed while the start_point is moving upward

		-- The status of the start_point relative to the island:
		status : type_point_status (OUTSIDE); -- will/must always be outside

		-- The main collection of x-values where a stripe enters or leaves an outer or inner border:
		x_main : pac_float_numbers.list;
		
		use pac_float_numbers;
		use pac_float_numbers_sorting;


		-- This flag decides whether a stripe starts or ends:
		stripe_start : boolean := true;

		-- This procedure queries an x-value an builds a stripe:
		procedure query_x_intersection (i : in pac_float_numbers.cursor) is begin
			if i /= x_main.last then

				if stripe_start then
					island.stripes.append ((
						start_point	=> set (element (i), start_point.y),
						end_point	=> set (element (next (i)), start_point.y)));

					stripe_start := false;					
				else
					stripe_start := true;
				end if;

			end if;
		end query_x_intersection;

		
		-- This procedure queries an inner border and appends
		-- the x-values of the candidate border to the main collection of
		-- x-values:
		procedure query_inner_border (i : in pac_polygon_list.cursor) is begin
			status := get_point_status (element (i), start_point);

			splice (
				target	=> x_main,
				source	=> status.x_intersections, 
				before	=> pac_float_numbers.no_element);

			exception 
				when others =>
					--put_line ("bottom: " & to_string (bottom));
					--put_line ("height: " & to_string (height));
					put_line ("status : " & to_string (
						get_point_status (element (i), start_point, true)));
					
					raise;
		end query_inner_border;

		
	begin
		--new_line;
		--put_line ("bottom: " & to_string (bottom));
		--put_line ("height: " & to_string (height));
		--put_line ("left: " & to_string (start_point.x));
		
		case style.style is
			when SOLID =>
				-- Since the stripes must overlap slightly the effective
				-- linewidth is smaller than style.linewidth. The effective_width
				-- is used to compute the number of stripes:
				--effective_width := type_float_internal_positive (style.linewidth) / overlap_factor;

				-- Compute the number of stripes in a rational number (like 6.3).
				--stripe_count_rational := height / effective_width;
				stripe_count_rational := height / type_float_internal_positive (style.linewidth);

				-- Round up the number of stripes to the next natural number (like 7)
				stripe_count_natural := natural (type_float_internal_positive'ceiling (
						type_float_internal_positive (stripe_count_rational)));
				

				stripe_spacing := height / type_float_internal_positive (stripe_count_natural);


				-- Compute the rows of stripes from bottom to top:
				for row in 1 .. stripe_count_natural loop
				
					start_point.y := bottom + type_float_internal_positive (row) * stripe_spacing;
					
					--put_line (to_string (get_point_status (island.outer_border, set (x_start, y))));
					--status := get_point_status (island.outer_border, start_point, true); -- debug on
					status := get_point_status (island.outer_border, start_point, false); -- debug off
					x_main := status.x_intersections;

					-- Compute the intersections with the inner borders:
					island.inner_borders.iterate (query_inner_border'access);

					-- Sort the main collection from left to right (ascending order):
					sort (x_main);

					-- Build the stripes for the current row:
					stripe_start := true;
					x_main.iterate (query_x_intersection'access);					
				end loop;

				
			when HATCHED =>
				null;
				-- CS
		end case;

		exception 
			when others =>
				--put_line ("bottom: " & to_string (bottom));
				--put_line ("height: " & to_string (height));
				put_line ("boundaries : " & to_string (boundaries));
				put_line ("start_point: " & to_string (start_point));

				raise;
		
	end make_stripes;

	
	
	procedure fill_island (
		islands		: in out pac_islands.list;
		position	: in pac_islands.cursor;
		style		: in type_style;
		process		: not null access procedure (
						island	: in out type_island;
						style	: in type_style))
	is
		island : type_island := element (position);
	begin
		process (island, style);
		islands.replace_element (position, island);
	end fill_island;



	function between_islands (
		zone	: in type_zone;
		point	: in type_vector;
		debug	: in boolean := false)
		return boolean
	is
		proceed : aliased boolean := true;
		
		procedure query_island (c : in pac_islands.cursor) is
			island : type_island renames element (c);
			
			status : constant type_point_status :=
				get_point_status (island.outer_border, point, debug);
		begin
			case status.location is
				when INSIDE | ON_VERTEX | ON_EDGE => proceed := false;
				when others => null;
			end case;
		end query_island;
		
	begin
		iterate (zone.islands, query_island'access, proceed'access);
		return proceed;
	end between_islands;



	function get_inner_border (
		zone	: in type_zone;
		point	: in type_vector;
		debug	: in boolean := false)
		return pac_polygon_list.cursor
	is
		result : pac_polygon_list.cursor; -- default no_element
		proceed : aliased boolean := true;

		
		procedure query_island (i : in pac_islands.cursor) is
			island : type_island renames element (i);

			procedure query_inner_border (b : in pac_polygon_list.cursor) is
				inner_border : type_polygon renames element (b);
				inner_border_status : constant type_point_status :=
					get_point_status (inner_border, point, debug);
			begin
				case inner_border_status.location is
					when INSIDE =>
						proceed := false;
						result := b;
						
					when others => null; -- ignore this inner border
				end case;
			end query_inner_border;

			
		begin
			iterate (island.inner_borders, query_inner_border'access, proceed'access);
		end query_island;

		
	begin
		iterate (zone.islands, query_island'access, proceed'access);
		return result;
	end get_inner_border;
	

	
	
	function get_location (
		zone	: in type_zone;
		point	: in type_vector;
		debug	: in boolean := false)
		return type_location
	is
		location : type_location := NON_CONDUCTING_AREA;
		proceed : aliased boolean := true;
		
		procedure query_island (i : in pac_islands.cursor) is
			island : type_island renames element (i);
			
			island_status : constant type_point_status :=
				get_point_status (island.outer_border, point, debug);

			procedure query_inner_border (b : in pac_polygon_list.cursor) is
				inner_border : type_polygon renames element (b);
				inner_border_status : constant type_point_status :=
					get_point_status (inner_border, point, debug);
			begin
				case inner_border_status.location is
					when OUTSIDE | ON_VERTEX | ON_EDGE =>
						proceed := false;
						location := CONDUCTING_AREA;
						--distance_to_border := island_status.distance;
						
					when INSIDE => null; -- ignore this inner border
				end case;
			end query_inner_border;

			
		begin -- query_island
			case island_status.location is
				when INSIDE | ON_VERTEX | ON_EDGE =>
					iterate (island.inner_borders, query_inner_border'access, proceed'access);
										
				when OUTSIDE => null; -- ignore this island
			end case;
		end query_island;
		
	begin
		iterate (zone.islands, query_island'access, proceed'access);
		return location;
	end get_location;


	
	function get_distance_to_nearest_island (
		zone		: in type_zone;
		start_point	: in type_vector;
		direction	: in type_angle;
		debug		: in boolean := false)
		return type_distance_to_conducting_area
	is
		result_exists : boolean := true;
		result_distance : type_float_internal_positive := 0.0;
		ray : constant type_ray := (start_point, direction);

		use pac_vectors;
		intersections : pac_vectors.list;
		
		procedure query_island (i : in pac_islands.cursor) is
			island : type_island renames element (i);

			procedure query_edge (e : in pac_edges.cursor) is
				use pac_edges;
				I : constant type_intersection_of_two_lines := 
					get_intersection (ray, element (e));
			begin
				case I.status is
					when EXISTS =>
						intersections.append (I.intersection.vector);

					when others => null;
				end case;
			end query_edge;
			
		begin
			island.outer_border.edges.iterate (query_edge'access);
		end query_island;
		
	begin
		-- Collect the intersections of the ray with the islands
		-- in container "intersections":
		zone.islands.iterate (query_island'access);

		-- Extract from "intersections" the one that is closest to start_point:
		remove_redundant_vectors (intersections);
		-- CS
		
		if result_exists then
			return (exists => true, distance => result_distance);
		else
			return (exists => false);
		end if;
	end get_distance_to_nearest_island;

	
	
	function get_distance_to_conducting_area (
		zone		: in type_zone;
		start_point	: in type_vector;
		direction	: in type_angle;
		debug		: in boolean := false)
		return type_distance_to_conducting_area
	is
		result_exists : boolean := true;
		result_distance : type_float_internal_positive := 0.0;

		location : constant type_location := get_location (zone, start_point, debug);
		inner_border_cursor : pac_polygon_list.cursor;
	begin
		case location is
			when CONDUCTING_AREA => 
				null; 
				-- Start point is already in conducting area.
				-- Return default values (see declarations above).
			
			when NON_CONDUCTING_AREA =>
				-- Start point is either between islands or inside inner borders:
				
				if between_islands (zone, start_point) then
					-- Point is between islands.
					return get_distance_to_nearest_island (zone, start_point, direction, debug);
					-- There might be no island into the given direction. In this case
					-- the returned value would be just "false".
				else
					-- Point is inside an inner border:
					inner_border_cursor := get_inner_border (zone, start_point);
					
					-- There must be a distance to the inner border:					
					result_distance := get_distance_to_border (
						element (inner_border_cursor), start_point, direction);

				end if;
		end case;

		return (exists => true, distance => result_distance);
	end get_distance_to_conducting_area;

	

	
-- 	procedure route_fill_zone_properties (
-- 	-- Logs the properties of the given fill_zone of a route
-- 		cursor			: in pac_conductor_fill_zones_signal.cursor;
-- 		log_threshold 	: in et_string_processing.type_log_level) is
-- 		use pac_conductor_fill_zones_signal;
-- 		use type_fill_zone_points;
-- 		points : type_fill_zone_points.set;
-- 		point_cursor : type_fill_zone_points.cursor;
-- 	begin
-- 		-- general stuff
-- 		log (text => "fill_zone" & 
-- 			 " " & text_fill_zone_signal_layer & to_string (element (cursor).layer) &
-- 			 " " & text_fill_zone_width_min & to_string (element (cursor).width_min) &
-- 			 " " & text_fill_zone_pad_connection & to_string (element (cursor).pad_connection) &
-- 			 " " & text_fill_zone_priority_level & to_string (element (cursor).priority_level) &
-- 			 " " & text_fill_zone_isolation_gap & to_string (element (cursor).isolation_gap) &
-- 			 " " & text_fill_zone_corner_easing & to_string (element (cursor).corner_easing) &
-- 			 " " & text_fill_zone_easing_radius & to_string (element (cursor).easing_radius),
-- 			 level => log_threshold);
-- 
-- 		log_indentation_up;
-- 		
-- 		-- type depended stuff
-- 		case element (cursor).pad_connection is
-- 			when THERMAL =>
-- 				log (text => text_fill_zone_pad_technology & to_string (element (cursor).thermal_technology) &
-- 					" " & text_fill_zone_thermal_width & to_string (element (cursor).thermal_width) &
-- 					" " & text_fill_zone_thermal_gap & to_string (element (cursor).thermal_gap),
-- 					level => log_threshold);
-- 
-- 			when SOLID =>
-- 				log (text => text_fill_zone_pad_technology & to_string (element (cursor).solid_technology),
-- 					level => log_threshold);
-- 				
-- 			when NONE =>
-- 				null;
-- 		end case;
-- 
-- 		-- corner points
-- 		log (text => text_fill_zone_corner_points, level => log_threshold);
-- 		points := element (cursor).corners;
-- 		point_cursor := points.first;
-- 		while point_cursor /= type_fill_zone_points.no_element loop
-- 			log (text => to_string (element (point_cursor)), level => log_threshold);
-- 			next (point_cursor);
-- 		end loop;
-- 		
-- 		log_indentation_down;
-- 	end route_fill_zone_properties;

		
	
end et_fill_zones;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
