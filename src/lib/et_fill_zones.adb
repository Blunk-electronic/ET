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

	procedure dummy is begin null; end;

	
	procedure make_stripes (
		island	: in out type_island;
		style	: in type_style)
	is		
		-- The boundaries of the island (greatest/smallest x/y):
		boundaries : constant type_boundaries := get_boundaries (island.outer_border, 0.0);

		height : constant type_float_internal_positive := get_height (boundaries);
		bottom : constant type_float_internal_positive := boundaries.smallest_x;

		effective_width : type_float_internal_positive;
		stripe_count_rational : type_float_internal_positive;
		stripe_count_natural : natural;

	begin
		case style.style is
			when SOLID =>
				-- Since the stripes must overlap slightly the effective
				-- linewidth is smaller than style.linewidth. The effective_width
				-- is used to compute the number of stripes:
				effective_width := type_float_internal_positive (style.linewidth) / overlap_factor;

				-- Compute the number of stripes in a rational number (like 6.3).
				stripe_count_rational := height / effective_width;

				-- Round up the number of stripes to the next natural number (like 7)
				stripe_count_natural := natural (type_float_internal_positive'ceiling (
						type_float_internal_positive (stripe_count_rational)));
				
				--log (text => "height:" & to_string (height) 
					--& " / line width:" & to_string (width)
					--& " / rows min:" & natural'image (rows_min),
					--level => log_threshold);

				
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


	
	--procedure iterate (
		--h_lines	: in pac_h_lines.list;
		--side	: in type_side;
		--process	: not null access procedure (position : in pac_h_lines.cursor);
		--proceed	: not null access boolean)
	--is
		--use pac_h_lines;
		--c : pac_h_lines.cursor;
	--begin
		--case side is
			--when LEFT => 
				--c := h_lines.first;

				--while c /= no_element and proceed.all = TRUE loop
					--process (c);
					--next (c);
				--end loop;

				
			--when RIGHT => 
				--c := h_lines.last;

				--while c /= no_element and proceed.all = TRUE loop
					--process (c);
					--previous (c);
				--end loop;
				
		--end case;
				
	--end iterate;


	
	--function overlap (
		--hl_1, hl_2 : in pac_h_lines.cursor)
		--return boolean
	--is
		--use pac_h_lines;
		
		--result : boolean := false;

		--L1S : constant type_distance := get_x (element (hl_1).start_point);
		--L1E : constant type_distance := get_x (element (hl_1).end_point);

		--L2S : constant type_distance := get_x (element (hl_2).start_point);
		--L2E : constant type_distance := get_x (element (hl_2).end_point);

	--begin
		---- case 1:
		----
		----        L1S----L1E
		----    L2S------------L2E
		----
		---- case 2:
		----
		----         L1S----L1E
		----    L2S-----L2E

		--if L1S >= L2S and L1S <= L2E then
			--result := true;
		--else

		---- case 3:
		----
		----    L1S----------L1E
		----       L2S----L2E

		---- case 4:
		----
		----    L1S------L1E
		----         L2S------L2E
			
			--if L2S >= L1S and L2S <= L1E then
				--result := true;
			--end if;

		--end if;
		
		--return result;
	--end overlap;






	--function get_adjacent_h_line (
		--row		: in pac_rows.cursor;
		--h_line	: in pac_h_lines.cursor;							 
		--place	: in type_adjacent;
		--side	: in type_side)
		--return pac_h_lines.cursor
	--is
		--result : pac_h_lines.cursor := pac_h_lines.no_element;

		--use pac_rows;
		--row_cursor : pac_rows.cursor := row;


		--procedure query_row (row : in type_row) is
			--proceed : aliased boolean := true;

			--procedure query_h_line (l : in pac_h_lines.cursor) is begin
				--if overlap (h_line, l) then
					--result := l;
					--proceed := false;
				--end if;
			--end query_h_line;
			
		--begin
			--iterate (row.lines, side, query_h_line'access, proceed'access);
		--end query_row;

		
	--begin -- get_adjacent_h_line

		--case place is
			--when ABOVE =>
				--next (row_cursor);
				
				--if row_cursor /= no_element then
					--query_element (row_cursor, query_row'access);
				--end if;
					

			--when BELOW =>
				--previous (row_cursor);
				
				--if row_cursor /= no_element then
					--query_element (row_cursor, query_row'access);
				--end if;
		--end case;

		--return result;
	--end get_adjacent_h_line;






	
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
