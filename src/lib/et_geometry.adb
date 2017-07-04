------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET GEOMETRY                                    --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

--   Please send your questions and comments to:
--
--   Mario.Blunk@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
-- with ada.strings.bounded; 	use ada.strings.bounded;
--with ada.numerics.real_arrays;  use ada.numerics.real_arrays;
with et_schematic;				use et_schematic;
with ada.numerics.generic_elementary_functions;
-- with ada.containers; 		use ada.containers;
-- with ada.containers.vectors;

package body et_geometry is
	-- Computes the shortest distance (perpendicular) of a given point from the given line. If the point outside the
	-- range of the x coordinate, the corresponding flag in the return value is set.
	function distance_of_point_from_line (
		point, line_start, line_end: in type_coordinates;
		line_range : in type_line_range)
		return type_distance_point_from_line is

		s : type_coordinates := line_start;
		e : type_coordinates := line_end;		
		
		package functions is new ada.numerics.generic_elementary_functions (type_grid);

		delta_x : type_grid := e.x - s.x;
		delta_y : type_grid := e.y - s.y;
		zero : constant type_grid := 0.0;		
		line_scratch : type_coordinates;
		
		d : type_distance_point_from_line;
		s1,s2,s3,s4,s5,s6,s7,s8 : type_grid_extended;

	begin
		-- The following computation bases on 
		-- https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
		
		s1 := (e.y - s.y) * point.x;
		s2 := (e.x - s.x) * point.y;
		s3 := e.x * s.y;
		s4 := e.y * s.x;

		s5 := abs(s1 - s2 + s3 - s4);

		s6 := (e.y - s.y)**2;
		s7 := (e.x - s.x)**2;

		s8 := functions.sqrt(s6+s7);

		d.distance := s5 / s8;

		-- If the range check adresses the line end points, the direction of the line
		-- matters. Means if it was drawn from the left to the right or the other way around.
		-- Swap start/end coordinates of line if drawn from right to the left.
		case line_range is
			when inside_end_points | with_end_points =>
				if delta_x < zero then 
					line_scratch := s;
					s := e;
					e := line_scratch;
				end if;
			when others => null;
		end case;

		-- Test range of point in regard of the x position.
		case line_range is
			when inside_end_points =>
				if d.distance = zero then

					if delta_x = zero then -- vertical line
						
						if delta_y > zero then -- line drawn away from x-axis
							if point.y >= e.y or point.y <= s.y then -- point above,below or on end points of line
								d.out_of_range := true;
							else
								d.out_of_range := false;
							end if;
							
						else -- line drawn toward x-axis
							if point.y >= s.y or point.y <= e.y then -- point above,below or on end points of line
								d.out_of_range := true;
							else
								d.out_of_range := false;
							end if;
						end if;
						
					else -- line is a slope or horizontal
							
						if point.x >= e.x or point.x <= s.x then
							d.out_of_range := true;
						else
							d.out_of_range := false;
						end if;
						
					end if;
					
					
-- 					if point.x >= e.x or point.x <= s.x then
-- 						d.out_of_range := true;
-- 					else
-- 						d.out_of_range := false;
-- 					end if;
				end if;
				
			when with_end_points =>
				if d.distance = zero then
					if delta_x = zero then -- vertical line
						
						if delta_y > zero then -- line drawn away from x-axis
							if point.y > e.y or point.y < s.y then -- point above or below end points of line
								d.out_of_range := true;
							else
								d.out_of_range := false;
							end if;
							
						else -- line drawn toward x-axis
							if point.y > s.y or point.y < e.y then -- point above or below end points of line
								d.out_of_range := true;
							else
								d.out_of_range := false;
							end if;
						end if;
						
					else -- line is a slope or horizontal
							
						if point.x > e.x or point.x < s.x then
							d.out_of_range := true;
						else
							d.out_of_range := false;
						end if;
						
					end if;
				end if;
				
			when beyond_end_points =>
				if d.distance = zero then
					d.out_of_range := false;
				end if;
		end case;


				
		return d;
	end distance_of_point_from_line;

end et_geometry;

