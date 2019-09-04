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

-- with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.numerics.generic_elementary_functions;

package body et_geometry is

	package body geometry_operations_2d is

		function to_distance (distance : in string) return type_distance is begin
			return type_distance'value (distance);
		end;
		
		function to_string (distance : in type_distance) return string is begin
			if distance < zero then
				return latin_1.space & type_distance'image (distance);
			else
				return type_distance'image (distance);
			end if;
		end;
		
		function x (point : in type_point'class) return type_distance is begin
			return point.x;
		end;

		function y (point : in type_point'class) return type_distance is begin
			return point.y;
		end;

		function mil_to_distance (mil : in string) return type_distance is
		-- Converts a mil number (given as a string) to millimeters.
			
			-- type type_distance_intermediate is digits 13 range mil_min .. mil_max; -- unit is mil
			-- CS: refine range and delta if required

			-- d_in : type_distance_intermediate;
			distance_mil : float := float'value (mil);
		begin
			-- d_in := type_distance_intermediate'value (mil);
			return type_distance (distance_mil * (25.4 * 0.001));
		end mil_to_distance;
		
		function "<" (left, right : in type_point) return boolean is begin
			if left.x < right.x then
				return true;
			elsif left.x > right.x then
				return false;

			-- left.x = right.x -> compare y:
			elsif left.y < right.y then
				return true;
			else 
				-- if left.y greater or equal right.y
				return false;
			end if;
		end;

		function distance_to_mil (distance : in type_distance) return string is
		-- Returns the given distance as string in mil.
			-- type type_distance_mm is digits 10 range -400_000_000.0 .. 400_000_000.0; 
			-- CS: increase digits if accuracy not sufficient
		
			--scratch : type_distance_mm;
			scratch : float;

			-- This is the output type:
			-- type type_distance_mil is delta 0.1 range -400_000_000.0 .. 400_000_000.0;
			-- type type_distance_mil is range -400_000_000 .. 400_000_000;
		begin
			--scratch := type_distance_mm (distance) * 1000.00 / 25.4;
			scratch := float (distance) * 1000.00 / 25.4;

			--return trim (type_distance_mil'image (type_distance_mil (scratch)), left);
			return to_string (type_distance (scratch));
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

		procedure mirror (
			point	: in out type_point;
			axis	: in type_axis_2d) is
		begin
			case axis is
				when X =>
					point.y := point.y * (-1.0);
				when Y =>
					point.x := point.x * (-1.0);
			end case;
		end mirror;

		function distance (
		-- Returns the absolute distance on the given axis between the given points.
			point_1	: in type_point;
			point_2	: in type_point;
			axis	: in type_axis_2d) 
			return type_distance is
			dis : type_distance;
		begin
			case axis is
				when X =>
					dis := abs (point_2.x - point_1.x);

				when Y =>
					dis := abs (point_2.y - point_1.y);
			end case;
					
			return type_distance (dis);
		end distance;
		
		function distance (point_one, point_two : in type_point) return type_distance is
		-- Computes the total distance between point_one and point_two.	

			package functions is new ada.numerics.generic_elementary_functions (float);
			
			distance : type_distance; -- to be returned
			delta_x, delta_y : float := 0.0;
		begin
			if point_one = point_two then
				distance := zero;
			elsif x (point_one) = x (point_two) then -- points are in a vertical line
				distance := abs (y (point_two) - y (point_one));
			elsif y (point_one) = y (point_two) then -- points are in a horizontal line
				distance := abs (x (point_two) - x (point_one));
			else
				delta_x := float (x (point_one) - x (point_two));
				delta_y := float (y (point_one) - y (point_two));

				-- put_line (float'image (delta_x) & " " & float'image (delta_y));
				
				distance := type_distance (functions.sqrt ((delta_x ** 2) + (delta_y ** 2)));
			end if;
				
			return distance;
		end distance;
				
		function distance_point_line (
		-- Computes the shortest distance (perpendicular) of a given point from the given line. If the point outside the
		-- range of the x coordinate, the corresponding flag in the return value is set.

		-- CS: provide range and accuracy via parameter
		-- CS: type_Y_axis_positive (upwards, downwards) may matter here. 

			point		: in type_point; 
			line_start	: in type_point;
			line_end 	: in type_point;
			line_range	: in type_line_range) 
			return type_distance_point_line is

			result : type_distance_point_line; -- to be returned
	
			type type_float is digits 11 range -100000000.0 .. 100000000.0; -- CS: probably way too much
			package functions is new ada.numerics.generic_elementary_functions (type_float);

			s : type_point := line_start;
			e : type_point := line_end;		

			delta_x : type_distance := x (e) - x (s);
			delta_y : type_distance := y (e) - y (s);
			
			line_scratch : type_point;

			s1,s2,s3,s4,s5,s6,s7,s8 : type_float;
			
		begin
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
						return result;

					elsif point = line_end then
						
						result.sits_on_end := true;
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
			if delta_x = zero then
				result.distance := abs (x (point) - x (s));
				
			elsif delta_y = zero then
				result.distance := abs (y (s) - y (point));
				
			else
				s1 := type_float ((y (e) - y (s)) * x (point));
				s2 := type_float ((x (e) - x (s)) * y (point));
				s3 := type_float (x (e) * y (s));
				s4 := type_float (y (e) * x (s));
				s5 := abs (s1 - s2 + s3 - s4);
				s6 := type_float (y (e) - y (s)) ** 2;
				s7 := type_float (x (e) - x (s)) ** 2;
				s8 := functions.sqrt (s6 + s7);

				result.distance := type_distance (s5 / s8); -- always positive
			end if;
			
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
					if result.distance = zero then

						if delta_x = zero then -- vertical line
							
							if delta_y > zero then -- line drawn away from x-axis
								if y (point) >= y (e) or y (point) <= y (s) then
								-- point above, below or on end points of line
									result.out_of_range := true;
								else
									result.out_of_range := false;
								end if;
								
							else -- line drawn toward x-axis
								if y (point) >= y (s) or y (point) <= y (e) then
								-- point above,below or on end points of line
									result.out_of_range := true;
								else
									result.out_of_range := false;
								end if;
							end if;
							
						else -- line is a slope or horizontal
								
							if x (point) >= x (e) or x (point) <= x (s) then
								result.out_of_range := true;
							else
								result.out_of_range := false;
							end if;
							
						end if;
						
					end if;
					
				when with_end_points =>
					
					if result.distance = zero then
						
						if delta_x = zero then -- vertical line
							
							if delta_y > zero then -- line drawn away from x-axis
								-- if point above or below end points of line
								if y (point) > y (e) or y (point) < y (s) then 
									-- point above or below end points of line
									result.out_of_range := true;
								else
									result.out_of_range := false;
								end if;
								
							else -- line drawn toward x-axis
								-- if point above or below end points of line
								if y (point) > y (s) or y (point) < y (e) then 
								-- point above or below end points of line								
									result.out_of_range := true;
								else
									result.out_of_range := false;
								end if;
							end if;
							
						else -- line is a slope or horizontal
							if x (point) > x (e) or x (point) < x (s) then
								result.out_of_range := true;
							else
								result.out_of_range := false;
							end if;
							
						end if;
					end if;
					
				when beyond_end_points =>
					if result.distance = zero then
						result.out_of_range := false;
					end if;
			end case;
			
			return result;
		end distance_point_line;

		function add (left, right : in type_rotation) return type_rotation is
		-- Adds two angles.
		-- If result greater or equal 360 degree then 360 degree is subtracted from result.
		-- If result less or equal 360 degree then 360 degree is added to the result.
			subtype type_rotation_wide is float range -720.0 .. +720.0;
			scratch : type_rotation_wide;
			result : type_rotation; -- to be returned
		begin
			scratch := float (left) + float (right);
			
			if scratch >= 360.0 then
				scratch := scratch - 360.0;
				
			elsif scratch <= -360.0 then
				scratch := scratch + 360.0;
			end if;

			result := type_rotation (scratch);
			return result;
		end;

		
-- 		function create (
-- 			point		: in type_point'class;
-- 			rotation	: in type_rotation) 
-- 			return type_position is
-- 		begin
-- 			return (point with rotation);
-- 		end;

		function to_rotation (rotation : in string) return type_rotation is begin
			return type_rotation'value (rotation);
		end;

		function to_string (rotation : in type_rotation) return string is begin
			if rotation < zero_rotation then
				return latin_1.space & type_rotation'image (rotation);
			else
				return type_rotation'image (rotation);
			end if;
		end;
		
		procedure set (
		-- Sets the rotation of a position. (position.rotation)
			position	: in out type_position;
			rotation	: in type_rotation) is 
		begin
			position.rotation := rotation;
		end;
					
		function rot (position : in type_position'class) return type_rotation is begin
		-- Returns the rotation of the given position.
			return position.rotation;
		end;

		procedure rotate (
		-- Changes the rotation of the given position by the given offset.
		-- Preserves x/y. Changes position.rotation only.
			position	: in out type_position'class;
			offset		: in type_rotation) is
		begin
			position.rotation := position.rotation + offset;
		end;
		
		procedure rotate (
		-- Rotates the given point by the given angle around the origin.
		-- Changes point.x and point.y only.							 
			point		: in out type_point'class;
			rotation	: in type_rotation) is

			type type_float_distance is digits 7 range -1000.0 .. 1000.0; -- CS: refine
			package functions_distance is new ada.numerics.generic_elementary_functions (type_float_distance);
			use functions_distance;
			
			type type_float_angle is digits 4 range -719.9 .. 719.9; -- CS: refine			
			package functions_angle is new ada.numerics.generic_elementary_functions (type_float_angle);
			use functions_angle;

			angle_out			: type_float_angle;		-- unit is degrees
			distance_to_origin	: type_float_distance;	-- unit is mm
			scratch				: type_float_distance;

		begin -- rotate
			-- Do nothing if the given rotation is zero.
			if rotation /= 0.0 then

				-- compute distance of given point to origin
				if x (point) = zero and y (point) = zero then
					distance_to_origin := type_float_distance (zero);
				elsif x (point) = zero then
					distance_to_origin := type_float_distance (abs (y (point)));
				elsif y (point) = zero then
					distance_to_origin := type_float_distance (abs (x (point)));
				else
					distance_to_origin := sqrt (
						type_float_distance (abs (x (point))) ** type_float_distance (2) 
						+
						type_float_distance (abs (y (point))) ** type_float_distance (2)
						);
				end if;
				
				-- compute the current angle of the given point (in degrees)

				if x (point) = zero then
					if y (point) > zero then
						angle_out := 90.0;
					elsif y (point) < zero then
						angle_out := -90.0;
					else
						angle_out := 0.0;
					end if;

				elsif y (point) = zero then
					if x (point) > zero then
						angle_out := 0.0;
					elsif x (point) < zero then
						angle_out := 180.0;
					else
						angle_out := 0.0;
					end if;

				else
					angle_out := type_float_angle (arctan (
						x => type_float_distance (x (point)),
						y => type_float_distance (y (point)),
						cycle => type_float_distance (units_per_cycle))
						);
				end if;

				-- Compute new angle by adding current angle and given angle.
				-- This computation depends on the Y axis style. The in the conventional style (Y going upwards positive)
				-- we add the given angle to the current angle. In the old fashioned stlyle (Y going downwards positive)
				-- we subtract the given angle from the current angle.
-- 				if Y_axis_positive = upwards then
					angle_out := angle_out + type_float_angle (rotation);
-- 				else
-- 					angle_out := angle_out - type_float_angle (rotation);
-- 				end if;

				-- compute new x   -- (cos angle_out) * distance_to_origin
				scratch := cos (type_float_distance (angle_out), type_float_distance (units_per_cycle));
				set (axis => X, point => point, value => type_distance (scratch * distance_to_origin));

				-- compute new y   -- (sin angle_out) * distance_to_origin
				scratch := sin (type_float_distance (angle_out), type_float_distance (units_per_cycle));
				set (axis => Y, point => point, value => type_distance (scratch * distance_to_origin));
		
			end if; -- if angle not zero
			
		end rotate;

		function arc_end_point (
		-- Computes the end point of an arc.
			center		: in type_point;
			start_point	: in type_point;	
			angle 		: in type_rotation)
			return type_point'class is
			end_point : type_point; -- to be returned			
		begin
			-- CS
			set (X, zero, end_point);
			set (Y, zero, end_point);
			
			return end_point;
		end arc_end_point;


		
		function to_string (point : in type_point) return string is begin
			return point_preamble
				& to_string (point.x)
				& axis_separator
				& to_string (point.y);
		end;

		overriding
		function to_string (point : in type_position) return string is begin
			return point_preamble_with_rotation
				& to_string (point.x)
				& axis_separator
				& to_string (point.y)
				& axis_separator
				& to_string (rot (point));
		end;
		
	end geometry_operations_2d;


	
	package body shapes_2d is
		use geometry;
		
		function to_string (line : in type_line) return string is
		-- Returns the start and end point of the given line as string.
		begin
			return latin_1.space 
				& "start" & to_string (line.start_point) 
				& " end" & to_string (line.end_point);
		end;

		function to_string (arc : in type_arc_2d) return string is
		-- Returns the start, end point and angle of the given arc as string.
		begin
			return latin_1.space 
				& "center" & to_string (arc.center) 
				& " start" & to_string (arc.start_point) 
				& " end" & to_string (arc.end_point);
		end to_string;

		function to_string (circle : in type_circle_2d) return string is
		-- Returns the center and radius of the given circle as string.
		begin
			return latin_1.space
				& "center" & to_string (circle.center) 
				& " radius" & to_string (circle.radius);
		end to_string;

		
	end shapes_2d;

	
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
