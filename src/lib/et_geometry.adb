------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with ada.numerics.generic_elementary_functions;
with et_string_processing;		use et_string_processing;
with et_coordinates;


package body et_geometry is
	
	function distance_of_point_from_line (
	-- Computes the shortest distance (perpendicular) of a given point from the given line. If the point outside the
	-- range of the x coordinate, the corresponding flag in the return value is set.

	-- CS: provide range and accuracy via parameter
	-- CS: type_Y_axis_positive (upwards, downwards) may matter here. 
	
		point, line_start, line_end: in et_coordinates.type_point;
		line_range : in type_line_range) return type_distance_point_from_line is

		use et_coordinates;

		type type_float is digits 11 range -100000000.0 .. 100000000.0; -- CS: probably way too much
		package functions is new ada.numerics.generic_elementary_functions (type_float);
	
		s : type_point := line_start;
		e : type_point := line_end;		
		
		delta_x : type_distance := distance_x (e) - distance_x (s);
		delta_y : type_distance := distance_y (e) - distance_y (s);

		line_scratch : type_point;
		
		result : type_distance_point_from_line;
		s1,s2,s3,s4,s5,s6,s7,s8 : type_float;

	begin -- Distance_from_a_point_to_a_line
-- 		log ("point   " & to_string (point), level => 4);
-- 		log ("start   " & to_string (s), level => 4);
-- 		log ("end     " & to_string (e), level => 4);				

		-- The first an simplest test is to figure out whether
		-- the given point sits at the start or end point of the line.
		-- This test applies for a range that includes the start and end 
		-- points of the line. 
		-- On match we exit this function prematurely and return the result
		-- with the appropiate flags set.
		case line_range is
			when with_end_points | beyond_end_points =>
				
				if point = line_start then
					
					result.sits_on_start := true;
-- 					result.distance := zero_distance;
					return result;

				elsif point = line_end then
					
					result.sits_on_end := true;
-- 					result.distance := zero_distance;
					return result;

				end if;
				
			when others => null;
		end case;

		-- The next test depends on the orientation of the given line.
		-- The line is vertical if delta_x is zero. It is horizontal if
		-- delta_y is zero.
		-- If either delta_x or delta_y is zero, the computation is simple.
		-- Otherwise we must do a bit more as described in 
		-- https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
		if delta_x = zero_distance then
			--log ("delta_x zero -> vertical line", level => 4);
			result.distance := abs (distance_x (point) - distance_x (s));
			
		elsif delta_y = zero_distance then
			--log ("delta_y zero -> horizontal line", level => 4);
			result.distance := abs (distance_y (s) - distance_y (point));
			
		else
			s1 := type_float ((distance_y (e) - distance_y (s)) * distance_x (point));
			s2 := type_float ((distance_x (e) - distance_x (s)) * distance_y (point));
			s3 := type_float (distance_x (e) * distance_y (s));
			s4 := type_float (distance_y (e) * distance_x (s));
			s5 := abs (s1 - s2 + s3 - s4);
			s6 := type_float (distance_y (e) - distance_y (s)) ** 2;
			s7 := type_float (distance_x (e) - distance_x (s)) ** 2;
			s8 := functions.sqrt (s6 + s7);

			result.distance := type_distance (s5 / s8); -- always positive
		end if;
		
		--log ("distance " & type_distance'image (result.distance), level => 4);
		
		-- If the range check adresses the line end points, the direction of the line
		-- matters. Means if it was drawn from the left to the right or the other way around.
		-- Swap start/end coordinates of line if drawn from right to the left.
		case line_range is
			when inside_end_points | with_end_points =>
				if delta_x < zero_distance then 
					line_scratch := s;
					s := e;
					e := line_scratch;
				end if;
			when others => null;
		end case;

		-- Test range of point in regard of the x position.
		case line_range is
			when inside_end_points =>
				if result.distance = zero_distance then

					if delta_x = zero_distance then -- vertical line
						
						if delta_y > zero_distance then -- line drawn away from x-axis
							--if point.y >= e.y or point.y <= s.y then -- point above, below or on end points of line
							if distance_y (point) >= distance_y (e) or distance_y (point) <= distance_y (s) then
							-- point above, below or on end points of line
								result.out_of_range := true;
							else
								result.out_of_range := false;
							end if;
							
						else -- line drawn toward x-axis
							-- if point.y >= s.y or point.y <= e.y then -- point above,below or on end points of line
							if distance_y (point) >= distance_y (s) or distance_y (point) <= distance_y (e) then
							-- point above,below or on end points of line
								result.out_of_range := true;
							else
								result.out_of_range := false;
							end if;
						end if;
						
					else -- line is a slope or horizontal
							
						--if point.x >= e.x or point.x <= s.x then
						if distance_x (point) >= distance_x (e) or distance_x (point) <= distance_x (s) then
							result.out_of_range := true;
						else
							result.out_of_range := false;
						end if;
						
					end if;
					
					
-- 					if point.x >= e.x or point.x <= s.x then
-- 						d.out_of_range := true;
-- 					else
-- 						d.out_of_range := false;
-- 					end if;
				end if;
				
			when with_end_points =>
-- 				log ("with endpoints", level => 4);
				
				if result.distance = zero_distance then
-- 					log ("zero distance", level => 4);
					
					if delta_x = zero_distance then -- vertical line
-- 						log ("vertial line", level => 4);
						
						if delta_y > zero_distance then -- line drawn away from x-axis
							-- if point above or below end points of line
							if distance_y (point) > distance_y (e) or distance_y (point) < distance_y (s) then 
								-- point above or below end points of line
-- 								log ("out of range", level => 4);
								result.out_of_range := true;
							else
-- 								log ("within range", level => 4);
								result.out_of_range := false;
							end if;
							
						else -- line drawn toward x-axis
							-- if point above or below end points of line
							if distance_y (point) > distance_y (s) or distance_y (point) < distance_y (e) then 
							-- point above or below end points of line								
								result.out_of_range := true;
							else
								result.out_of_range := false;
							end if;
						end if;
						
					else -- line is a slope or horizontal
-- 						log ("horizontal line or slope", level => 4);
						
						if distance_x (point) > distance_x (e) or distance_x (point) < distance_x (s) then
							result.out_of_range := true;
						else
							result.out_of_range := false;
						end if;
						
					end if;
				end if;
				
			when beyond_end_points =>
				if result.distance = zero_distance then
					result.out_of_range := false;
				end if;
		end case;
				
		return result;

	end distance_of_point_from_line;

	package body geometry_operations_2d is

		function x (point : in type_point'class) return type_distance is begin
			return point.x;
		end;

		function y (point : in type_point'class) return type_distance is begin
			return point.y;
		end;

		function set (x, y : in type_distance) return type_point'class is
			point : type_point;
		begin
			point.x := x;
			point.y := y;
			return point;
		end;

		procedure set (
			axis 	: in type_axis_2d;
			value	: in type_distance;					 
			point	: in out type_point'class) is
		begin
			case axis is
				when X => point.x := value;
				when Y => point.y := value;
			end case;
		end;

		procedure set (
			point	: in out type_point'class;
			position: in type_point) is
		begin
			point.x := position.x;
			point.y := position.y;
		end;

		
		procedure reset (point : in out type_point'class) is begin
		-- Moves the given point to the origin (0/0).
			point.x := zero;
			point.y := zero;
		end;

		procedure move (
			point	: in out type_point'class;
			offset	: in type_point) 
		is begin
			point.x := point.x + offset.x;
			point.y := point.y + offset.y;
		end;
		
		
-- 		function distance (
-- 			point		: in type_point; 
-- 			line_start	: in type_point;
-- 			line_end 	: in type_point;
-- 			line_range	: in type_line_range) 
-- 			return type_distance_point_line is
-- 			result : type_distance_point_line; -- to be returned
	-- 
	-- 		type type_float is digits 11 range -100000000.0 .. 100000000.0; -- CS: probably way too much
	-- 		package functions is new ada.numerics.generic_elementary_functions (type_float);

	-- 		s : type_point := line_start;
	-- 		e : type_point := line_end;		

	-- 		delta_x : type_distance := x (e) - x (s);
	-- 		delta_y : type_distance := y (e) - y (s);
			
	-- 		line_scratch : type_point;
			
	-- 		result : type_distance_point_from_line;
	-- 		s1,s2,s3,s4,s5,s6,s7,s8 : type_float;
			
-- 		begin
-- 
-- 			return result;
-- 		end distance;

		procedure dummy is begin null; end;
		
	end geometry_operations_2d;
	
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
