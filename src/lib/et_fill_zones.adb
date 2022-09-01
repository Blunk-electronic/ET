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

--with ada.strings.unbounded;
--with ada.exceptions;
--with ada.tags;

--with et_text;					use et_text;

package body et_fill_zones is

	
	procedure make_stripes (
		island	: in out type_island;
		style	: in type_style)
	is		
		-- The boundaries of the island (greatest/smallest x/y):
		boundaries : constant type_boundaries := get_boundaries (island.outer_border, 0.0);

		height : constant type_float_internal_positive := get_height (boundaries);
		bottom : constant type_float_internal := get_bottom (boundaries);

		--effective_width : type_float_internal_positive;
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
		
		use pac_polygon_list;
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
			
		end query_inner_border;

		
	begin
		--new_line;
		--put_line ("bottom: " & to_string (bottom));
		----put_line ("height: " & to_string (height));
		--put_line ("left: " & to_string (x_start));
		
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
		end case;
	end make_stripes;

	
	
	procedure fill_island (
		islands		: in out pac_islands.list;
		position	: in pac_islands.cursor;
		style		: in type_style;
		process		: not null access procedure (
						island	: in out type_island;
						style	: in type_style))
	is
		use pac_islands;
		island : type_island := element (position);
	begin
		process (island, style);
		islands.replace_element (position, island);
	end fill_island;


	


	
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
