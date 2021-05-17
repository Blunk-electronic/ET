------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.unbounded;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with et_string_processing;		use et_string_processing;
with et_exceptions;				use et_exceptions;

package body et_geometry is

	function sgn (x : float) return float is begin
		if x >= 0.0 then
			return 1.0;
		else
			return -1.0;
		end if;
	end sgn;


	
	function to_string (axis : in type_axis) return string is begin
		return to_lower (type_axis'image (axis));
	end;

	function to_axis (axis : in string) return type_axis is begin
		return type_axis'value (axis);
	end;



	function to_string (tool : in type_tool) return string is begin
		return type_tool'image (tool);
	end to_string;

	function to_tool (tool : in string) return type_tool is begin
		return type_tool'value (tool);
	end to_tool;


	

	function to_notches (notches : in string) return type_grid_notches is begin
		return type_grid_notches'value (notches);
	end;

	function to_string (notches : in type_grid_notches) return string is begin
		return type_grid_notches'image (notches);
	end;



	function to_shape (shape : in string) return type_shape is begin
		return type_shape'value (shape);
	end;

	function to_string (shape : in type_shape) return string is begin
		return to_lower (type_shape'image (shape));
	end;

	
	
	
	function to_string (coordinates : in type_coordinates) return string is begin
		return latin_1.space & to_lower (type_coordinates'image (coordinates));
	end;

	function to_coordinates (coordinates : in string) return type_coordinates is begin
		return type_coordinates'value (coordinates);

-- 			exception
-- 				when event: others =>
-- 					log (text => ada.exceptions.exception_information (event), console => true);
-- 					raise;
	end;


	function to_string (filled : in type_filled) return string is begin
		return to_lower (type_filled'image (filled));
	end to_string;

	function to_filled (filled : in string) return type_filled is begin
		return type_filled'value (filled);
	end to_filled;


	
	function to_string (curvature : in type_curvature) return string is begin
		return type_curvature'image (curvature);
	end to_string;
	
	
	package body generic_pac_geometry is

		function to_positive_rotation (
			rotation	: in type_rotation)
			return type_rotation_positive
		is begin
			if rotation < 0.0 then
				return 360.0 + rotation;
			else
				return rotation;
			end if;
		end to_positive_rotation;

		
		procedure scale_grid (
			grid	: in out type_grid;
			scale	: in type_distance_positive)
		is begin
			grid.x := grid.x * scale;
			grid.y := grid.y * scale;
		end scale_grid;
		
		function to_string (grid : in type_grid) return string is begin
			return point_preamble & to_string (grid.x) & axis_separator & to_string (grid.y);
		end;

		function to_string (point : in type_point) return string is begin
			return point_preamble
				& to_string (point.x)
				& axis_separator
				& to_string (point.y);
		end to_string;
		
		function to_distance (distance : in string) return type_distance is begin
			return type_distance'value (distance);

			exception when event: others =>
				raise syntax_error_2 with 
					"ERROR: Expect a distance instead of " 
					& enclose_in_quotes (distance) & " !";
		end to_distance;
		
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

		function rotation (point : in type_point) return type_rotation is
			x : constant float := float (point.x);
			y : constant float := float (point.y);
		begin
			-- NOTE: If x and y are zero then the arctan operation is not possible. 
			-- In this case we assume the resulting angle is zero.
			if x = 0.0 and y = 0.0 then
				return zero_rotation;
			else
				return type_rotation (arctan (y, x, float (units_per_cycle)));
			end if;
		end rotation;

		
		function get_height (boundaries : in type_boundaries)
			return type_distance_positive
		is begin
			return boundaries.greatest_y - boundaries.smallest_y;
		end get_height;

		function get_width (boundaries : in type_boundaries)
			return type_distance_positive
		is begin
			return boundaries.greatest_x - boundaries.smallest_x;
		end get_width;
		
		function to_string (boundaries : in type_boundaries) return string is begin
			return "boundaries: smallest x" & to_string (boundaries.smallest_x) 
				& " greatest x" & to_string (boundaries.greatest_x)
				& " smallest y" & to_string (boundaries.smallest_y)
				& " greatest y" & to_string (boundaries.greatest_y);
		end;

		function intersect (
			boundaries_one : in type_boundaries;
			boundaries_two : in type_boundaries)
			return boolean
		is begin
			if	boundaries_two.greatest_x >= boundaries_one.smallest_x
			and boundaries_two.smallest_x <= boundaries_one.greatest_x
			then -- boundaries intersect in x-direction

				if	boundaries_two.greatest_y >= boundaries_one.smallest_y
				and boundaries_two.smallest_y <= boundaries_one.greatest_y
				then -- boundaries intersect in y-direction
					return true;
				else
					return false;
				end if;
				
			else
				return false;
			end if;

		end intersect;

		
		-- Adds two boundaries.
		procedure add (
			boundaries_one : in out type_boundaries;
			boundaries_two : in type_boundaries) is
		begin
-- 			if boundaries_two.smallest_x < boundaries_one.smallest_x , smallest_y : type_distance := type_distance'last;
-- 			greatest_x, greatest_y : type_distance := type_distance'first;
			null; -- CS
		end; 

		
		function get_boundaries (
			point_one	: in type_point;
			point_two	: in type_point;
			width		: in type_distance_positive) 
			return type_boundaries
		is
			result : type_boundaries;

			half_width : constant type_distance_positive := width * 0.5;
		begin
			-- X axis
			if point_one.x = point_two.x then -- both points on a vertical line

				result.smallest_x := point_one.x;
				result.greatest_x := point_one.x;
				
			elsif point_one.x < point_two.x then
				
				result.smallest_x := point_one.x;
				result.greatest_x := point_two.x;
			else
				result.smallest_x := point_two.x;
				result.greatest_x := point_one.x;
			end if;

			-- Y axis
			if point_one.y = point_two.y then -- both points on a horizontal line

				result.smallest_y := point_one.y;
				result.greatest_y := point_one.y;
				
			elsif point_one.y < point_two.y then
				
				result.smallest_y := point_one.y;
				result.greatest_y := point_two.y;
			else
				result.smallest_y := point_two.y;
				result.greatest_y := point_one.y;
			end if;

			
			-- extend the boundaries by half the line width;
			result.smallest_x := result.smallest_x - half_width;
			result.smallest_y := result.smallest_y - half_width;

			result.greatest_x := result.greatest_x + half_width;
			result.greatest_y := result.greatest_y + half_width;
			
			return result;
		end get_boundaries;

		procedure move_by (
		-- Moves the boundaries by the given offset.
			boundaries	: in out type_boundaries;
			offset		: in type_point) is
		begin
			boundaries.smallest_x := boundaries.smallest_x + offset.x;
			boundaries.greatest_x := boundaries.greatest_x + offset.x;
			
			boundaries.smallest_y := boundaries.smallest_y + offset.y;
			boundaries.greatest_y := boundaries.greatest_y + offset.y;
		end move_by;

		procedure rotate (
			boundaries	: in out type_boundaries;
			rotation	: in type_rotation) is

			-- The boundaries are basically a rectangle with those four corners:
			corners : array (positive range 1 .. 4) of type_point;

			-- backup the position of the topleft corner of the boundaries:
			topleft_before_rotation : constant type_point := (
					x	=> boundaries.smallest_x,
					y	=> boundaries.greatest_y);

			topleft_after_rotation : type_point;
			
		begin -- rotate
			-- Set the corner points according to the given boundaries:
			corners (1)	:= (boundaries.smallest_x, boundaries.greatest_y);
			corners (2) := (boundaries.greatest_x, boundaries.greatest_y);
			corners (3) := (boundaries.smallest_x, boundaries.smallest_y);
			corners (4) := (boundaries.greatest_x, boundaries.smallest_y);

			-- After the rotation the boundaries may become wider than actually
			-- required.
			
			-- The boundaries are always relative to a certain origin that
			-- sits somewhere inside the rectangle. The four corners are now rotated
			-- around the origin by the given angle:
			for c in corners'first .. corners'last loop
				rotate_by (corners (c), rotation);
			end loop;

			-- reset boundaries
			boundaries := boundaries_default;
			
			for c in corners'first .. corners'last loop
				
				-- find the smallest x
				if corners (c).x < boundaries.smallest_x then
					boundaries.smallest_x := corners (c).x;
				end if;

				-- find the greatest x
				if corners (c).x > boundaries.greatest_x then
					boundaries.greatest_x := corners (c).x;
				end if;

				-- find the smallest y
				if corners (c).y < boundaries.smallest_y then
					boundaries.smallest_y := corners (c).y;
				end if;

				-- find the greatest y
				if corners (c).y > boundaries.greatest_y then
					boundaries.greatest_y := corners (c).y;
				end if;
				
			end loop;

			-- After the rotation we get a new topleft position:
			topleft_after_rotation := (
				x	=> boundaries.smallest_x,
				y	=> boundaries.greatest_y);

			-- The difference in x and y between topleft_before_rotation
			-- and topleft_after_rotation:
			boundaries.distance_of_topleft_to_default := type_point 
				(topleft_before_rotation - topleft_after_rotation);
			
		end rotate;
		
		function to_string (rectangle : in type_rectangle) return string is begin
			return "rectangle " & to_string (set (rectangle.x, rectangle.y))
				& " width" & to_string (rectangle.width)
				& " height" & to_string (rectangle.height);
		end;

		procedure move_by (
			rectangle	: in out type_rectangle;
			offset		: in type_point) is
		begin
			rectangle.x := rectangle.x + offset.x;
			rectangle.y := rectangle.y + offset.y;
		end move_by;
		
		function intersects (rect1, rect2 : type_rectangle) return boolean is begin
			return not (
				rect1.x > rect2.x + rect2.width            --  r1 on the right of r2
				or else rect2.x > rect1.x + rect1.width    --  r2 on the right of r1
				or else rect1.y > rect2.y + rect2.height   --  r1 below r2
				or else rect2.y > rect1.y + rect1.height); --  r1 above r2
		end intersects;



		
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

		function quadrant (point : in type_point) return type_quadrant is
		-- Returns the quadrant the point is located in.
		begin
			if point.x >= zero then -- we are right of the y-axis or on top of it
				if point.y >= zero then -- we are above the x-axis or on top of it
					return ONE; 
				else -- we are below the x-axis
					return FOUR;
				end if;
				
			else -- we are left of the y-axis
				if point.y >= zero then -- we are above the x-axis or on top of it
					return TWO;
				else -- we are below the x-axis
					return THREE;
				end if;
			end if;
		end quadrant;
		
		function invert (point : in type_point'class) return type_point'class is
		-- Inverts the given point by multiplying x by -1 and y by -1.
			pi : type_point'class := point;
		begin
			pi.x := - pi.x;
			pi.y := - pi.y;
			return pi;
		end invert;

		function invert (
			point	: in type_point;
			axis	: in type_axis_2d)
			return type_point'class is
			p : type_point := point;
		begin
			case axis is
				when X => p.x := - p.x;
				when Y => p.y := - p.y;
			end case;

			return p;
		end invert;
		
		procedure reset (point : in out type_point'class) is begin
		-- Moves the given point to the origin (0/0).
			point.x := zero;
			point.y := zero;
		end;

		procedure move_by (
		-- Moves a point by the given offset.
			point	: in out type_point'class;
			offset	: in type_point) is
		begin
			point.x := point.x + offset.x;
			point.y := point.y + offset.y;
		end move_by;

		procedure move_to (
		-- Moves a point to the given position.
			point		: in out type_point'class;
			position	: in type_point) is
		begin
			point.x := position.x;
			point.y := position.y;
		end move_to;
		
		function move (
		-- Moves a point into direction at distance.
			point		: in type_point;
			direction	: in type_rotation;
			distance	: in type_distance_positive)
			return type_point'class 
		is 			
			delta_x, delta_y : float := 0.0;
			-- CS could be useful to use a constrained float type
			
			result : type_point;
			
		begin
			-- sin (direction) * distance = delta y
			-- cos (direction) * distance = delty x

			delta_y := sin (float (direction), float (units_per_cycle)) * float (distance);
			delta_x := cos (float (direction), float (units_per_cycle)) * float (distance);

			result.x := point.x + type_distance (delta_x);
			result.y := point.y + type_distance (delta_y);
			
			return result;
		end;
		
		procedure mirror (
		-- If axis is Y then it swaps right x with left x.
		-- If axis is X then it swaps upper y with lower y.
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
			point_1	: in type_point;
			point_2	: in type_point;
			axis	: in type_axis_2d) 
			return type_distance is
			d : type_distance;
		begin
			case axis is
				when X =>
					d := (point_2.x - point_1.x);

				when Y =>
					d := (point_2.y - point_1.y);
			end case;

			return d;
		end distance;
		
		function distance_abs (
			point_1	: in type_point;
			point_2	: in type_point;
			axis	: in type_axis_2d) 
			return type_distance_positive is
			d : type_distance_positive;
		begin
			case axis is
				when X =>
					d := abs (point_2.x - point_1.x);

				when Y =>
					d := abs (point_2.y - point_1.y);
			end case;
					
			return d;
		end distance_abs;

		function "+" (point_one, point_two : in type_point) return type_point'class is
			d : type_point;
		begin
			d.x := point_one.x + point_two.x;
			d.y := point_one.y + point_two.y;
			return d;
		end;
		
		function "-" (point_one, point_two : in type_point) return type_point'class is
			d : type_point;
		begin
			d.x := point_one.x - point_two.x;
			d.y := point_one.y - point_two.y;
			return d;
		end;
		
		function distance_relative (point_one, point_two : in type_point) return type_point'class is
			d : type_point;
		begin
			d.x := point_two.x - point_one.x;
			d.y := point_two.y - point_one.y;
			return d;
		end distance_relative;
		
		function distance_total (
			point_one, point_two : in type_point) 
			return type_distance_positive 
		is
			distance : type_distance_positive; -- to be returned
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
				
				distance := type_distance (sqrt ((delta_x ** 2) + (delta_y ** 2)));
			end if;
				
			return distance;
		end distance_total;

		function in_catch_zone (
			point_1		: in type_point; -- the reference point
			catch_zone	: in type_catch_zone; -- zone around reference point
			point_2 	: in type_point) -- the point being tested
			return boolean 
		is
			d : type_distance_positive := distance_total (point_1, point_2);
		begin
			if d <= catch_zone then
				return true;
			else
				return false;
			end if;
		end in_catch_zone;
				
		function add (left, right : in type_rotation) return type_rotation is
		-- Adds two angles.
		-- If result greater 360 degree then 360 degree is subtracted from result.
		-- If result less than 360 degree then 360 degree is added to the result.
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


		
		function distance_polar (point_one, point_two : in type_point) 
			return type_distance_polar 
		is
			result : type_distance_polar;

			delta_x, delta_y : float := 0.0;
		begin
			result.absolute := distance_total (point_one, point_two);

			-- NOTE: If the total distance between the points is zero then
			-- the arctan operation is not possible. In this case we assume
			-- the resulting angle is zero.
			-- So we do the angle computation only if there is a distance between the points:
			if result.absolute /= zero then
				
				delta_x := float (x (point_two) - x (point_one));
				delta_y := float (y (point_two) - y (point_one));

				result.angle := type_rotation (arctan (
						x 		=> delta_x,
						y		=> delta_y,
						cycle	=> float (units_per_cycle)));
			else
				-- distance is zero
				result.angle := zero_rotation;
			end if;
			
			return result;
		end distance_polar;

		function angle (distance : in type_distance_polar) return type_rotation is begin
			return distance.angle;
		end angle;
	
		function absolute (distance : in type_distance_polar) return type_distance_positive is begin
			return distance.absolute;
		end absolute;


		
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

		function to_string (direction : in type_direction_of_rotation) return string is begin
			return to_lower (type_direction_of_rotation'image (direction));
		end to_string;
			
		function to_direction (direction : in string) return type_direction_of_rotation is begin
			return type_direction_of_rotation'value (direction);
		end to_direction;

		function direction_of_rotation (rotation : in type_rotation) return type_direction_of_rotation is begin
			if rotation < zero_rotation then
				return CW;
			else
				return CCW;
			end if;
		end direction_of_rotation;

		function reverse_direction (direction : in type_direction_of_rotation)
			return type_direction_of_rotation is
		begin
			case direction is 
				when CW => return CCW;
				when CCW => return CW;
			end case;
		end reverse_direction;
		
		function to_radians (degrees : in type_rotation) return float is
		-- Converts degrees to radians.
			use ada.numerics;
		begin
			return (pi * float (degrees)) / (units_per_cycle * 0.5);
		end to_radians;

		function to_degrees (radians : in float) return type_rotation is
		-- Converts radians to degrees.
			use ada.numerics;
		begin
			return type_rotation ((units_per_cycle * 0.5 * radians) / pi);
		end to_degrees;

		function to_position (
			point		: in type_point;
			rotation	: in type_rotation)
			return type_position'class
		is 
			result : type_position;
		begin
			result.x := point.x;
			result.y := point.y;
			result.rotation := rotation;

			return result;
		end to_position;
			
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
			position.rotation := add (position.rotation, offset);
		end;
		
		procedure rotate_by (
			point		: in out type_point'class;
			rotation	: in type_rotation) 
		is			
			type type_float_angle is digits 4 range -719.9 .. 719.9; -- CS: refine			
			package functions_angle is new ada.numerics.generic_elementary_functions (type_float_angle);
			use functions_angle;

			angle_out			: type_float_angle;		-- unit is degrees
			distance_to_origin	: float;	-- unit is mm
			scratch				: float;

		begin -- rotate
			-- Do nothing if the given rotation is zero.
			if rotation /= 0.0 then

				-- compute distance of given point to origin
				if x (point) = zero and y (point) = zero then
					distance_to_origin := float (zero);
				elsif x (point) = zero then
					distance_to_origin := float (abs (y (point)));
				elsif y (point) = zero then
					distance_to_origin := float (abs (x (point)));
				else
					distance_to_origin := sqrt (
						float (abs (x (point))) ** float (2) 
						+
						float (abs (y (point))) ** float (2)
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
					-- neither x nor y of point is zero
					angle_out := type_float_angle (arctan (
						x => float (x (point)),
						y => float (y (point)),
						cycle => float (units_per_cycle))
						);
				end if;

				-- Compute new angle by adding current angle and given angle.
				angle_out := angle_out + type_float_angle (rotation);

				-- compute new x   -- (cos angle_out) * distance_to_origin
				scratch := cos (float (angle_out), float (units_per_cycle));
				set (axis => X, point => point, value => type_distance (scratch * distance_to_origin));

				-- compute new y   -- (sin angle_out) * distance_to_origin
				scratch := sin (float (angle_out), float (units_per_cycle));
				set (axis => Y, point => point, value => type_distance (scratch * distance_to_origin));
		
			end if; -- if angle not zero
			
		end rotate_by;

		procedure rotate_to (
			point		: in out type_point'class;
			rotation	: in type_rotation) is

			-- CS probably way to much stuff here. simplify. use code of procedure rotate_by (see above).
			
			type type_float_angle is digits 4 range -719.9 .. 719.9; -- CS: refine			
			package functions_angle is new ada.numerics.generic_elementary_functions (type_float_angle);
			use functions_angle;

			angle_out			: type_float_angle;		-- unit is degrees
			distance_to_origin	: float;	-- unit is mm
			scratch				: float;

		begin -- rotate

			-- compute distance of given point to origin
			if x (point) = zero and y (point) = zero then
				distance_to_origin := float (zero);
			elsif x (point) = zero then
				distance_to_origin := float (abs (y (point)));
			elsif y (point) = zero then
				distance_to_origin := float (abs (x (point)));
			else
				distance_to_origin := sqrt (
					float (abs (x (point))) ** float (2) 
					+
					float (abs (y (point))) ** float (2)
					);
			end if;

			-- The new angle is the given rotation:
			angle_out := type_float_angle (rotation);

			-- compute new x   -- (cos angle_out) * distance_to_origin
			scratch := cos (float (angle_out), float (units_per_cycle));
			set (axis => X, point => point, value => type_distance (scratch * distance_to_origin));

			-- compute new y   -- (sin angle_out) * distance_to_origin
			scratch := sin (float (angle_out), float (units_per_cycle));
			set (axis => Y, point => point, value => type_distance (scratch * distance_to_origin));
			
		end rotate_to;

-- 		procedure rotate_by (
-- 		-- Rotates the given point BY the given angle around the given center point.
-- 		-- Changes point.x and point.y only.
-- 			point		: in out type_point'class;
-- 			center		: in type_point;
-- 			rotation	: in type_rotation) is
-- 		begin
-- 			null;
-- 		end rotate_by;

		function round (
			distance	: in type_distance;
			grid		: in type_distance_grid)
			return type_distance is
		begin
			return type_distance (integer (distance / grid)) * grid;
		end round;
			
		function round_to_string (
			point 	: in type_point;
			grid	: in type_grid) 
			return string is 
		begin
			return point_preamble
				& to_string (round (point.x, grid.x))
				& axis_separator
				& to_string (round (point.y, grid.y));
		end;

		function round (
			point 	: in type_point;
			grid	: in type_grid) 
			return type_point'class is
			
			p : type_point := (
				x => (round (point.x, grid.x)),
				y => (round (point.y, grid.y)));
		begin
			return p;
		end;

		function to_string (point : in type_position) return string is begin
			return point_preamble_with_rotation
				& to_string (point.x)
				& axis_separator
				& to_string (point.y)
				& axis_separator
				& to_string (rot (point));
		end;
		
	end generic_pac_geometry;


	
	package body generic_pac_shapes is

-- 		function to_string (fill_style : in type_fill_style) return string is begin
-- 			return to_lower (type_fill_style'image (fill_style));
-- 		end;
-- 
-- 		function to_fill_style (fill_style : in string) return type_fill_style is begin
-- 			return type_fill_style'value (fill_style);
-- 		end;
		
		function to_string (status : in type_point_status) return string is begin
			return type_point_status'image (status);
		end to_string;
	
		procedure toggle_status (status : in out type_point_status) is begin
			case status is
				when OUTSIDE	=> status := INSIDE;
				when INSIDE		=> status := OUTSIDE;
			end case;
		end toggle_status;

		
		procedure union (
			boundaries	: in out type_boundaries;
			point		: in type_point) is
		begin
			-- X axis
			if point.x < boundaries.smallest_x then 
				boundaries.smallest_x := point.x; 
			end if;
			
			if point.x > boundaries.greatest_x then
				boundaries.greatest_x := point.x; 
			end if;

			-- Y axis
			if point.y < boundaries.smallest_y then
				boundaries.smallest_y := point.y;
			end if;
			
			if point.y > boundaries.greatest_y then
				boundaries.greatest_y := point.y;
			end if;
		end;
		
		procedure union (
			left	: in out type_boundaries;
			right	: in type_boundaries) is
		begin
			-- X axis
			-- smallest
			if right.smallest_x < left.smallest_x then
				left.smallest_x := right.smallest_x;
			end if;

			-- Y axis
			-- smallest
			if right.smallest_y < left.smallest_y then
				left.smallest_y := right.smallest_y;
			end if;

			-- X axis
			-- greatest
			if right.greatest_x > left.greatest_x then
				left.greatest_x := right.greatest_x;
			end if;

			-- Y axis
			-- greatest
			if right.greatest_y > left.greatest_y then
				left.greatest_y := right.greatest_y;
			end if;
		end union;


	-- VECTOR OPERATIONS		

		function get_x (
			v	: in type_vector)
			return type_distance is
		begin
			return v.x;
		end get_x;
		
		function get_y (
			v	: in type_vector)
			return type_distance is
		begin
			return v.y;
		end get_y;

		function get_z (
			v	: in type_vector)
			return type_distance is
		begin
			return v.z;
		end get_z;

		
		function to_vector (
			point	: in type_point)
			return type_vector is
		begin
			return (
				x => point.x,
				y => point.y,
				z => zero
				);
		end to_vector;

		function to_point (
			v	: in type_vector)
			return type_point is
		begin
			-- Since the return is a 2D point,
			-- the z component of v must be zero:
			if v.z /= zero then
				raise constraint_error;
			end if;
						
			return type_point (set (
				x => v.x,
				y => v.y
				));
		end to_point;
		
		function absolute (
			vector	: in type_vector)
			return type_distance_positive
		is
		begin
			return type_distance_positive (
				sqrt (
					float (vector.x) ** 2 + 
					float (vector.y) ** 2 +
					float (vector.z) ** 2)
				);
		end absolute;

		function scale (
			v	: in type_vector;
			s	: in float)
			return type_vector
		is begin
			return (
				x => type_distance (s * float (v.x)),
				y => type_distance (s * float (v.y)),
				z => type_distance (s * float (v.z))
				);
		end scale;

		
		function add (
			a, b	: in type_vector)
			return type_vector
		is begin
			return (
				x => a.x + b.x,
				y => a.y + b.y,
				z => a.z + b.z);
		end add;
		
		function subtract (
			a, b	: in type_vector)
			return type_vector
		is begin
			return (
				x => a.x - b.x,
				y => a.y - b.y,
				z => a.z - b.z);
		end subtract;
		
		function cross_product (
			a, b	: in type_vector)
			return type_vector
		is begin
			return (
				x => a.y * b.z - a.z * b.y,
				y => a.z * b.x - a.x * b.z,
				z => a.x * b.y - a.y * b.x);
		end cross_product;

		function dot_product (
			a, b	: in type_vector)
			return type_distance
		is begin
			return (a.x * b.x  +  a.y * b.y  +  a.z * b.z);
		end dot_product;

		function mixed_product (
			a, b, c	: in type_vector)
			return type_distance
		is
			cp : type_vector;
		begin
			cp := cross_product (b, c);
			return dot_product (a, cp);
		end mixed_product;		

		
		function divide (
			a, b	: in type_vector)
			return type_distance
		is
			lambda : type_distance;
		begin
			-- It does not matter if we use
			-- the x,y or z component for this calculation.
			-- But we must skip the case when
			-- a division by zero is ahead.
			if b.x /= zero then
				lambda := a.x / b.x;
			elsif b.y /= zero then
				lambda := a.y / b.y;
			elsif b.z /= zero then
				lambda := a.z / b.z;
			else
				put_line ("ERROR while vector division ");
			end if;
			
			return lambda;
		end divide;

		

		function start_vector (ray : in type_ray) 
			return type_vector
		is
			v : type_vector;
		begin
			v.x := ray.start_point.x;
			v.y := ray.start_point.y;
			v.z := zero;

			return v;
		end start_vector;

		
		function direction_vector (ray : in type_ray) 
			return type_vector
		is 
			v : type_vector;
		begin
			-- x = cos (direction) * 1
			v.x := type_distance (cos (float (ray.direction), float (units_per_cycle)));

			-- y = sin (direction) * 1
			v.y := type_distance (sin (float (ray.direction), float (units_per_cycle)));

			v.z := zero; -- we are in a 2D world
			
			return v;
		end direction_vector;


		function get_angle (
			line	: in type_line_vector)
			return type_rotation
		is 
			a : type_rotation;
		begin

			a := type_rotation (arctan (
					y		=> float (line.v_direction.y), 
					x		=> float (line.v_direction.x), 
					cycle	=> float (units_per_cycle)));

			-- dz ignored. we are in a 2D world
			
			return a;
		end get_angle;
		
		
		function to_line_vector (
			ray : in type_ray)
			return type_line_vector
		is begin
			return (
				v_start		=> start_vector (ray),
				v_direction	=> direction_vector (ray));
		
		end to_line_vector;

		function to_perpendicular_line_vector (
			point	: in type_vector;
			angle	: in type_rotation)
			return type_line_vector 
		is
			ap : type_rotation; -- the angle of the resulting line
			r : type_ray;
		begin
			ap := add (angle, 90.0); -- perpendicular

			-- Build a ray that starts at point and travels
			-- in direction ap:
			r := (to_point (point), ap);

			-- Convert the ray to a line vector:
			return to_line_vector (r);
		end to_perpendicular_line_vector;

		
		function to_string (intersection : in type_intersection)
			return string
		is begin
			return to_string (to_point (intersection.point)) 
				& " angle" & to_string (intersection.angle);
		end to_string;
		
		function get_intersection (
			line_1, line_2	: in type_line_vector)
			return type_intersection_of_two_lines
		is 
			-- scratch variables:
			a, b, c, d, e, f, g : float;
			lambda : float;

			-- location vector and angle of intersection to be returned:			
			i : type_intersection;

			function exists_intersection return boolean is
				v1, v2 : type_vector;
			begin
				-- The first condition to be fulfilled is that the
				-- cross product of the direction vectors is not a null vector:
				v1 := cross_product (line_1.v_direction, line_2.v_direction); 

				if v1 /= null_vector then
					
					-- The second condition is:
					-- The mixed product of line_2.v_start, line_1.v_start and
					-- (line_2.v_start - line_1.v_start) must be zero.
					
					v2 := subtract (line_2.v_start, line_1.v_start);

					if mixed_product (line_1.v_direction, line_2.v_direction, v2) = zero then
						return true; -- there is an intersection
					else
						return false;  -- no intersection exists
					end if;
					
				else					
					return false; -- no intersection exists
				end if;
			end exists_intersection;

			function lines_overlap return boolean is
				a, b, distance : type_distance_positive;
				v1 : type_vector;
			begin
				-- The first condition to be fulfilled is that the lines
				-- must run parallel to each other. In this case the cross
				-- product is zero.
				v1 := cross_product (line_1.v_direction, line_2.v_direction); 
				
				if v1 = null_vector then -- the lines run parallel to each other.

					-- Compute the distance between the lines.
					-- If the distance is zero then the lines overlap.
					
					a := absolute (cross_product (line_1.v_direction, subtract (line_2.v_start, line_1.v_start)));
					b := absolute (line_1.v_direction);

					distance := a / b;

					if distance = zero then
						return true; -- lines overlap each other
					else
						return false; -- distance greater zero -> hence no overlap
					end if;
					
				else
					return false; -- not parallel -> hence no overlap
				end if;
			end lines_overlap;
				
		begin -- get_intersection
			--put_line ("");
			--put_line ("line_1 start" & to_string (to_point (line_1.v_start)) & " direction" & to_string (to_point (line_1.v_direction)));
			--put_line ("line_2 start" & to_string (to_point (line_2.v_start)) & " direction" & to_string (to_point (line_2.v_direction)));

			-- Test whether the lines overlap:
			if lines_overlap then
				return (status => OVERLAP);
			else
				-- Test whether there is an intersection:
				if exists_intersection then
					
					-- The direction vector of the first line can be zero in x.
					-- In order to avoid division by zero we must switch between
					-- two ways to find the intersection:
					if line_1.v_direction.x /= zero then
						a := float (line_1.v_start.y);
						b := float (line_2.v_start.x * line_1.v_direction.y) / float (line_1.v_direction.x);
						c := float (line_1.v_start.x * line_1.v_direction.y) / float (line_1.v_direction.x);
						d := float (line_2.v_start.y);
						e := float (line_2.v_direction.y);
						f := float (line_2.v_direction.x * line_1.v_direction.y) / float (line_1.v_direction.x);
						g := 1.0 / (e - f);

						lambda := (a + b - c - d) * g;

						i.point := add (line_2.v_start, scale (line_2.v_direction, lambda));
					else
						a := float (line_2.v_start.y);
						b := float (line_1.v_start.x * line_2.v_direction.y) / float (line_2.v_direction.x);
						c := float (line_2.v_start.x * line_2.v_direction.y) / float (line_2.v_direction.x);
						d := float (line_1.v_start.y);
						e := float (line_1.v_direction.y);
						f := float (line_1.v_direction.x * line_2.v_direction.y) / float (line_2.v_direction.x);
						g := 1.0 / (e - f);

						lambda := (a + b - c - d) * g;

						i.point := add (line_1.v_start, scale (line_1.v_direction, lambda));
					end if;


					i.angle := get_angle_of_itersection (line_1, line_2);
					
					return (status => EXISTS, intersection => i);
				else

					return (status => NOT_EXISTENT);
				end if;
			end if;
			
		end get_intersection;

		
		function get_angle_of_itersection (
			line_1, line_2	: in type_line_vector)
			return type_rotation
		is 
			a, b : float;
		begin
			a := float (dot_product (line_1.v_direction, line_2.v_direction));
			b := float (absolute (line_1.v_direction) * absolute (line_2.v_direction));

			return type_rotation (arccos (X => a / b, cycle => float (units_per_cycle)));
		end get_angle_of_itersection;


		function reverse_line (line : in type_line) return type_line'class is 
			result : type_line'class := line;
		begin
			result.start_point := line.end_point;
			result.end_point := line.start_point;

			return result;
		end reverse_line;
		
		procedure reverse_line (line : in out type_line) is 
			scratch : type_point := line.start_point;
		begin
			line.start_point := line.end_point;
			line.end_point := scratch;
		end reverse_line;
		
		
		function to_string (direction : in type_line_direction)
			return string
		is begin
			return " " & type_line_direction'image (direction);
		end to_string;
		
		
		function get_direction (line : in type_line)
			return type_line_direction
		is
			result : type_line_direction := HORIZONTAL;

			dx : constant type_distance := line.end_point.x - line.start_point.x;
			dy : constant type_distance := line.end_point.y - line.start_point.y;
		begin
			if dx = zero then
				-- no change in x -> line runs vertically
				result := VERTICAL;
				
			elsif dy = zero then
				-- no change in y -> line runs horizontally
				result := HORIZONTAL;
				
			elsif dx > zero then
				-- start point comes before end point
				
				if dy > zero then
					-- start point is lower than end point
					result := RISING;
					
				else -- dy < zero
					-- start point is higher than end point
					result := FALLING;
				end if;

			else -- dx < zero
				-- start point comes after end point
				
				if dy > zero then
					-- start point is higher than end point
					result := FALLING;
					
				else -- dy < zero
					-- start point is lower than end point
					result := RISING;
				end if;

			end if;
			
			return result;
		end get_direction;

		function get_tangent_direction (angle : in type_tangent_angle)
			return type_line_direction
		is
			result : type_line_direction := HORIZONTAL;
		begin
			if angle = 0.0 then
				result := HORIZONTAL;
				
			elsif angle < 90.0 then
				result := FALLING;
				
			elsif angle = 90.0 then
				result := VERTICAL;
				
			else -- angle > 90.0
				result := RISING;
			end if;

			return result;
		end get_tangent_direction;
		
		function crosses_threshold (
			line		: in type_line;	
			y_threshold	: in type_distance)
			return boolean
		is begin
			if	
				Y (line.start_point) >= y_threshold and 
				Y (line.end_point)   <  y_threshold then
				return true;
				
			elsif
				Y (line.end_point)   >= y_threshold and 
				Y (line.start_point) <  y_threshold then
				return true;
				
			else
				return false;
			end if;
		end crosses_threshold;

		
		function get_center (
			line	: in type_line)
			return type_point
		is
			dp : constant type_distance_polar := 
				distance_polar (line.start_point, line.end_point);
		begin
			return type_point (move (
				point		=> line.start_point,
				direction	=> angle (dp),
				distance	=> absolute (dp) * 0.5));

		end get_center;

		
		function get_intersection (
			probe_line		: in type_line_vector;
			candidate_line	: in type_line)
			return type_intersection_of_two_lines
		is
			i : constant type_intersection_of_two_lines := get_intersection (
					line_1	=> probe_line,
					line_2	=> to_line_vector (candidate_line));
		begin
			case i.status is
				when EXISTS =>
				
					-- The intersection must be ON OR AFTER the start point
					-- of probe_line, means in direction of travel.
					if X (to_point (i.intersection.point)) >= X (to_point (probe_line.v_start)) then

						-- The intersection must be between start and end point of
						-- the candidate line (start and end point itself included).
						-- If the intersection is between start and end point
						-- of candidate line, then return the intersection as it is.
						-- If the intersection is before start point or
						-- beyond end point, then return NOT_EXISTENT.
						if on_line (to_point (i.intersection.point), candidate_line) then
							return i;
						else
							return (status => NOT_EXISTENT);
						end if;

					else
						return (status => NOT_EXISTENT);
					end if;

				when others =>		
					return i;
			end case;

		end get_intersection;

		
		function start_vector (
			line	: in type_line)
			return type_vector is
		begin
			return (
				x => line.start_point.x,
				y => line.start_point.y,
				z => zero
				);
		end start_vector;

		function end_vector (
			line	: in type_line)
			return type_vector is
		begin
			return (
				x => line.end_point.x,
				y => line.end_point.y,
				z => zero
				);
		end end_vector;
		
		function direction_vector (
			line	: in type_line)
			return type_vector is
		begin
			return (
				x => line.end_point.x - line.start_point.x,
				y => line.end_point.y - line.start_point.y,
				z => zero
				);
		end direction_vector;

		function to_line_vector (
			line	: in type_line)
			return type_line_vector
		is begin
			return (
				v_start		=> start_vector (line),
				v_direction	=> direction_vector (line));
		end to_line_vector;
		
		function distance (
			line	: in type_line;
			point	: in type_point)
			return type_distance_positive
		is
			dv : constant type_vector := direction_vector (line);
			sv : constant type_vector := start_vector (line);
			pv : constant type_vector := to_vector (point);
			
			d1 : constant type_vector := subtract (pv, sv);
			m, n : type_distance_positive;
		begin
			m := absolute (cross_product (dv, d1));
			n := absolute (dv);
			
			return (m / n);
		end distance;


		
		
		function direction (
			line	: in type_line)
			return type_rotation is

			dx : constant float := float (x (line.end_point) - x (line.start_point));
			dy : constant float := float (y (line.end_point) - y (line.start_point));

			--package pac_functions is new ada.numerics.generic_elementary_functions (float);
			--use pac_functions;
		begin
			-- NOTE: If dx and dy are zero then the arctan operation is not possible. 
			-- In this case we assume the resulting angle is zero.
			if dx = 0.0 and dy = 0.0 then
				return zero_rotation;
			else
				return type_rotation (arctan (dy, dx, float (units_per_cycle)));
			end if;
		end direction;

		procedure move_by (
			line		: in out type_line;
			direction	: in type_rotation;
			distance	: in type_distance_positive) is
		begin
			-- Move start and and point of line into direction by distance.
			line.start_point	:= type_point (move (line.start_point, direction, distance));
			line.end_point		:= type_point (move (line.end_point,   direction, distance));
		end move_by;
		
		procedure move_by (
			line	: in out type_line;
			offset	: in type_point)
		is begin
			move_by (point	=> line.start_point,	offset => offset);
			move_by (point	=> line.end_point,		offset => offset);
		end move_by;

		procedure mirror (
			line		: in out type_line;
			axis		: in type_axis_2d)
		is begin
			mirror (line.start_point, axis);
			mirror (line.end_point, axis);
		end mirror;
		
		procedure rotate_by (
			line		: in out type_line;
			rotation	: in type_rotation) 
		is begin
			rotate_by (line.start_point, rotation);
			rotate_by (line.end_point, rotation);
		end rotate_by;

		function to_route (
			start_point, end_point	: in type_point;
			style					: in type_bend_style)
			return type_route is

			-- The area required for the route is a rectangle.
			-- We will need to figure out whether it is wider than tall:
			dx : constant type_distance := distance (start_point, end_point, X);
			dy : constant type_distance := distance (start_point, end_point, Y);

			sup_start, sup_end : type_point; -- support points near given start and end point

			-- distance of support points from given start or end point:
			ds : constant type_distance_positive := 1.0;

			bended : type_bended := YES;
			bend_point : type_point;

			-- CS this procedure should be made public as "intersection" or similar
			-- CS use function get_intersection with S1, R1, S2, R2 as input
			-- to compute intersection I.
			procedure compute_bend_point is 
				type type_line_here is new type_line with null record;
				first_line	: constant type_line_here := (start_point, sup_start);
				second_line	: constant type_line_here := (end_point, sup_end);

				-- first line start vector:
				S1 : constant type_vector := start_vector (first_line);

				-- first line direction vector:
				R1 : constant type_vector := direction_vector (first_line);

				-- second line start vector:
				S2 : constant type_vector := start_vector (second_line);

				-- second line direction vector
				R2 : constant type_vector := direction_vector (second_line);

				-- scratch variables:
				a, b, c, d, e, f, g : type_distance;
				lambda_1, lambda_2 : type_distance;

				-- location vector of intersection
				I : type_vector;
			begin
				-- The direction vector of the first line can be zero in x (R1.x).
				-- In order to avoid division by zero we must switch between
				-- two ways to find the intersection:
				if R1.x /= zero then
					a := S1.y;
					b := type_distance (S2.x * R1.y) / R1.x;
					c := type_distance (S1.x * R1.y) / R1.x;
					d := S2.y;
					e := R2.y;
					f := type_distance (R2.x * R1.y) / R1.x;
					g := 1.0 / (e - f);

					lambda_2 := (a + b - c - d) * g;

					I := add (S2, scale (R2, float (lambda_2)));
				else
					a := S2.y;
					b := type_distance (S1.x * R2.y) / R2.x;
					c := type_distance (S2.x * R2.y) / R2.x;
					d := S1.y;
					e := R1.y;
					f := type_distance (R1.x * R2.y) / R2.x;
					g := 1.0 / (e - f);

					lambda_1 := (a + b - c - d) * g;

					I := add (S1, scale (R1, float (lambda_1)));
				end if;
				
				bend_point := to_point (I);
			end compute_bend_point;
			
		begin -- to_route
			
			-- If start and end point are equally then do nothing
			-- and return given start and end point as they are:
			if start_point = end_point then
				bended := NO;
			else
			
				-- If start and end point have same x or y position, then we
				-- have a straight direct line between them.
				if dx = zero or dy = zero then
					bended := NO;
				else

					case style is
						when STRAIGTH_THEN_ANGLED =>
							if abs (dx) = abs (dy) then -- diagonal line from start to end
								bended := NO;
							else
								
								-- compute support point near start point:
								-- The first line must run straight from start point:
								--if wider_than_tall then
								if abs (dx) > abs (dy) then -- wider than tall
									sup_start := type_point (set (start_point.x + ds, start_point.y));
								else -- taller than wide
									sup_start := type_point (set (start_point.x, start_point.y + ds));
								end if;

								-- compute support point near end point:
								-- The second line must run angled from end point:
								if dx > zero then -- to the right
									if dy > zero then -- upwards
										sup_end := type_point (set (end_point.x + ds, end_point.y + ds));
										--  45 degree
									else
										sup_end := type_point (set (end_point.x + ds, end_point.y - ds));
										-- -45 degree
									end if;
								else -- to the left
									if dy > zero then -- upwards
										sup_end := type_point (set (end_point.x - ds, end_point.y + ds));
										-- 135 degree
									else
										sup_end := type_point (set (end_point.x - ds, end_point.y - ds));
										-- 225 degree
									end if;
								end if;

								compute_bend_point;

							end if;
							
						when DIRECT => bended := NO;

						when ANGLED_THEN_STRAIGHT =>
							if abs (dx) = abs (dy) then -- diagonal line from start to end
								bended := NO;
							else
								
								-- Compute support point near start point:
								-- The first line must run angled from start point:
								if dx > zero then -- to the right
									if dy > zero then -- upwards
										sup_start := type_point (set (start_point.x + ds, start_point.y + ds));
										--  45 degree
									else -- downwards
										sup_start := type_point (set (start_point.x + ds, start_point.y - ds));
										-- -45 degree
									end if;
								else -- to the left
									if dy > zero then -- upwards
										sup_start := type_point (set (start_point.x - ds, start_point.y + ds));
										-- 135 degree
									else -- downwards
										sup_start := type_point (set (start_point.x - ds, start_point.y - ds));
										-- 225 degree
									end if;
								end if;

								-- compute support point near end point:
								-- The second line must run straight from end point:
								if abs (dx) > abs (dy) then -- wider than tall
									sup_end := type_point (set (end_point.x + ds, end_point.y));
									-- horizontally
								else -- taller than wide
									sup_end := type_point (set (end_point.x, end_point.y + ds));
									-- vertically
								end if;

								compute_bend_point;
							end if;
							
						when VERTICAL_THEN_HORIZONTAL =>
							-- Compute support point near start point:
							-- The first line must run vertically from start point:
							sup_start := type_point (set (start_point.x, start_point.y + ds));
							-- vertically

							-- The second line must run horizontally from end point:
							sup_end := type_point (set (end_point.x + ds, end_point.y));
							-- horizontally

							compute_bend_point;
							
						when HORIZONTAL_THEN_VERTICAL =>
							-- Compute support point near start point:
							-- The first line must run horizontal from start point:
							sup_start := type_point (set (start_point.x + ds, start_point.y));
							-- horizontally

							-- compute support point near end point:
							-- The second line must run vertically from end point:
							sup_end := type_point (set (end_point.x, end_point.y + ds));
							-- vertically

							compute_bend_point;
							
					end case;
				end if;

			end if;
				
			if bended = NO then
				return (NO, start_point, end_point);
			else
				return (YES, start_point, end_point, bend_point);
			end if;

		end to_route;

		procedure next_bend_style (route : in out type_route_live) is
			i : constant natural := type_bend_style'pos (route.bend_style);
			-- i points now to the current bend style

			-- get the index of the last available bend style:
			max : constant natural := type_bend_style'pos (type_bend_style'last);
		begin
			if i < max then
				-- jump to next bend style
				route.bend_style := type_bend_style'succ (type_bend_style'val (i));
			else 
				-- After the last bend style, jump back to the first bend style:
				route.bend_style := type_bend_style'first;
			end if;
		end next_bend_style;
		
		function get_boundaries (
			line	: in type_line;	
			width	: in type_distance_positive)
			return type_boundaries 
		is begin
			return get_boundaries (line.start_point, line.end_point, width);
		end get_boundaries;
		
		function which_zone (
			point	: in type_point'class;
			line	: in type_line'class) 
			return type_line_zone is

			zone : type_line_zone; -- to be returned
		
			line_length : type_distance;
			zone_border : type_distance;
			
		begin -- which_zone
			-- CS: The algorithm used here is not the best. Improve using vector algebra ?
			
			-- The greater distance from start to end point in X or Y determines 
			-- whether the line is handled like a horizontal or vertical drawn line.
			if distance_abs (line.start_point, line.end_point, X) > 
			   distance_abs (line.start_point, line.end_point, Y) then

				-- distance in X greater -> decision will be made along the X axis.
				-- The line will be handled like a horizontal drawn line.
				
				-- calculate the zone border. This depends on the line length in X direction.
				line_length := distance_abs (line.start_point, line.end_point, X);
				zone_border := line_length / type_distance (line_zone_division_factor);

				if x (line.start_point) < x (line.end_point) then 
				-- DRAWN FROM LEFT TO THE RIGHT
					if x (point) < x (line.start_point) + zone_border then
						zone := START_POINT; -- point is in the zone of line.start_point
					elsif x (point) > x (line.end_point) - zone_border then
						zone := END_POINT; -- point is in the zone of line.end_point
					else
						zone := CENTER;
					end if;

				else 
				-- DRAWN FROM RIGHT TO THE LEFT
					if x (point) > x (line.start_point) - zone_border then
						zone := START_POINT; -- point is in the zone of line.start_point
					elsif x (point) < x (line.end_point) + zone_border then
						zone := END_POINT; -- point is in the zone of line.end_point
					else
						zone := CENTER;
					end if;
				end if;

				
			else
				-- distance in Y greater or equal distance in X -> decision will be made along the Y axis.
				-- The line will be handled like a vertical drawn line.

				-- calculate the zone border. This depends on the line length in Y direction.
				line_length := distance_abs (line.start_point, line.end_point, Y);
				zone_border := line_length / type_distance (line_zone_division_factor);

				if y (line.start_point) < y (line.end_point) then 
				-- DRAWN UPWARDS
					if y (point) < y (line.start_point) + zone_border then
						zone := START_POINT; -- point is in the zone of line.start_point
					elsif y (point) > y (line.end_point) - zone_border then
						zone := END_POINT; -- point is in the zone of line.end_point
					else
						zone := CENTER;
					end if;
						
				else 
				-- DRAWN DOWNWARDS
					if y (point) > y (line.start_point) - zone_border then
						zone := START_POINT; -- point is in the zone of line.start_point
					elsif y (point) < y (line.end_point) + zone_border then
						zone := END_POINT; -- point is in the zone of line.end_point
					else
						zone := CENTER;
					end if;
				end if;
				
				
			end if;
			
			return zone;
		end which_zone;

		function out_of_range (d : in type_distance_point_line) return boolean is begin
			return d.out_of_range;
		end out_of_range;

		function distance (d : in type_distance_point_line) return type_distance is begin
			return d.distance;
		end distance;

		function on_start_point (d : in type_distance_point_line) return boolean is begin
			return d.sits_on_start;
		end on_start_point;
	
		function on_end_point (d : in type_distance_point_line) return boolean is begin
			return d.sits_on_end;
		end on_end_point;
		
		function distance_point_line (
			point		: in type_point;
			line		: in type_line;
			line_range	: in type_line_range;
			catch_zone	: in type_catch_zone := zero)
			return type_distance_point_line is

			result : type_distance_point_line; -- to be returned

			line_direction : type_rotation;
			line_direction_vector : type_vector;
			line_start_vector, line_end_vector : type_vector;

			exact_point : type_point;
			exact_point_vector : type_vector;
			
			lambda_forward, lambda_backward : type_distance;
		begin
			-- The first and simplest test is to figure out whether
			-- the given point sits at the start or end point of the line.
			-- Mind: result.distance has default zero.
			-- This test applies for a range that includes the start and end 
			-- points of the line. 
			-- On match we exit this function prematurely and return the result
			-- with the appropiate flags set.
			case line_range is
				when WITH_END_POINTS | BEYOND_END_POINTS =>
					
					if point = line.start_point then
						
						result.sits_on_start := true;
						result.out_of_range := false;
						return result;

					elsif point = line.end_point then
						
						result.sits_on_end := true;
						result.out_of_range := false;
						return result;

					end if;
					
				when others => null;
			end case;

			-- If the ends of the line are included,
			-- test whether the point is in the vicinity of the line start or end point.
			-- Exit this function prematurely in that case.
			if line_range = WITH_END_POINTS then

				-- compute distance of point to start of line:
				result.distance := distance_total (point, line.start_point);
				
				if result.distance <= catch_zone then
					result.out_of_range := false;
					return result;
				end if;

				-- compute distance of point to end of line:
				result.distance := distance_total (point, line.end_point);
				
				if result.distance <= catch_zone then
					result.out_of_range := false;
					return result;
				end if;

			end if;
			
			-- Compute the distance of the point from the line.
			-- This computation does not care about end or start point of the line.
			-- It assumes an indefinite long line without start or end point.
			result.distance := distance (line, point);

			--put_line ("distance " & to_string (result.distance));
			
			-- If the point sits somewhere on the line, we must figure out
			-- where exactly it is. If it is not on the line, then there is
			-- nothing to do.
			if result.distance <= catch_zone then -- on the line

				line_direction := direction (line);
				line_direction_vector := direction_vector (line);
				
				-- Compute the exact point on the line: The intersection of a line that runs
				-- from the given point perpendicular to the given line.
				-- At this stage we do not know in which direction to go. So we just try
				-- to go in 90 degree direction. If the distance of exact_point from the line
				-- is not zero, then we try in -90 degree direction.
				exact_point := type_point (move (point, line_direction + 90.0, result.distance));
				
				if distance (line, exact_point) /= zero then
					exact_point := type_point (move (point, line_direction - 90.0, result.distance));
				end if;
			
				exact_point_vector := to_vector (exact_point);

				-- Any point on a line can be computed by this formula (see textbook on vector algebra):
				-- exact_point = line.start_point + lambda_forward  * line_direction_vector
				-- exact_point = line.end_point   + lambda_backward * line_direction_vector

				-- Using these formula we can calculate whether exact_point sits between 
				-- (or at) the start and end point of the line:
				
				line_start_vector := start_vector (line);
				lambda_forward := divide (subtract (exact_point_vector, line_start_vector), line_direction_vector);

				if lambda_forward < zero then -- point sits before start of line
-- 					log (text => "before start point");
					case line_range is
						when BEYOND_END_POINTS => result.out_of_range := false;
						when others => result.out_of_range := true;
					end case;

					return result; -- no more computations required
				end if;
				
				if lambda_forward = zero then -- point sits at start of line
-- 					log (text => "at start point");
					result.sits_on_start := true;
					case line_range is
						when BETWEEN_END_POINTS => result.out_of_range := true;
						when others => result.out_of_range := false;
					end case;

					return result; -- no more computations required
				end if;

-- 				log (text => "after start point");
				
				line_end_vector := end_vector (line);
				lambda_backward := divide (subtract (exact_point_vector, line_end_vector), line_direction_vector);

				if lambda_backward > zero then -- point sits after end of line
-- 					log (text => "after end point");
					case line_range is
						when BEYOND_END_POINTS => result.out_of_range := false;
						when others => result.out_of_range := true;
					end case;

					return result; -- no more computations required
				end if;

				if lambda_backward = zero then -- point sits at end of line
-- 					log (text => "at end point");
					result.sits_on_end := true;
					case line_range is
						when BETWEEN_END_POINTS => result.out_of_range := true;
						when others => result.out_of_range := false;
					end case;

					return result; -- no more computations required
				end if;

-- 				log (text => "before end point");

				result.out_of_range := false;
			end if;
			
			return result;
		end distance_point_line;

		function on_line (
			point		: in type_point;
			line		: in type_line;
			catch_zone	: in type_catch_zone := zero)
			return boolean is
			distance : type_distance_point_line;
		begin
			--distance := distance_point_line (point, line, BETWEEN_END_POINTS);
			distance := distance_point_line (point, line, WITH_END_POINTS, catch_zone);

			if not distance.out_of_range and distance.distance <= catch_zone then
				return true;
			else
				return false;
			end if;
		end;


		function reverse_arc (arc : in type_arc) return type_arc'class is
			result : type_arc'class := arc;
		begin
			result.start_point := arc.end_point;
			result.end_point := arc.start_point;

			case arc.direction is
				when CW  => result.direction := CCW;
				when CCW => result.direction := CW;
			end case;
			
			return result;
		end reverse_arc;
			
		procedure reverse_arc (arc : in out type_arc) is
			scratch : type_point := arc.start_point;
		begin
			arc.start_point := arc.end_point;
			arc.end_point := scratch;

			case arc.direction is
				when CW	 => arc.direction := CCW;
				when CCW => arc.direction := CW;
			end case;
		end reverse_arc;

		
		function crosses_threshold (
			arc			: in type_arc;
			y_threshold	: in type_distance)
			return boolean
		is begin
			if	
				Y (arc.start_point) >= y_threshold and 
				Y (arc.end_point)   <  y_threshold then
				return true;
				
			elsif
				Y (arc.end_point)   >= y_threshold and 
				Y (arc.start_point) <  y_threshold then
				return true;
				
			else
				return false;
			end if;
		end crosses_threshold;

		
		function get_curvature (
			arc		: in type_arc)
			return type_curvature_of_arc
		is
			c : type_curvature_of_arc;
		begin
			case arc.direction is
				when CW =>
					if  Y (arc.start_point) > Y (arc.end_point) then
						c := CONCAVE; 
					else
						c := CONVEX; 
					end if;
					
				when CCW =>
					if Y (arc.start_point) > Y (arc.end_point) then
						c := CONVEX;								
					else
						c := CONCAVE; 
					end if;
			end case;

			return c;
		end get_curvature;
		
		
		function radius_start (arc : in type_arc) return type_distance_positive is begin
			return distance_total (arc.center, arc.start_point);
		end radius_start;

		function radius_end (arc : in type_arc) return type_distance_positive is begin
			return distance_total (arc.center, arc.end_point);
		end radius_end;

		function is_valid (arc : in type_arc) return boolean is begin
			if radius_start (arc) = radius_end (arc) then
				return true;
			else
				return false;
			end if;
		end is_valid;

		function to_arc_angles (arc : in type_arc) return type_arc_angles is
		-- Returns the start and end angles of an arc.
		-- The angles may be negative. For example instead of 270 degree
		-- the angle will be -90 degree.
			result : type_arc_angles;
						
			-- Take a copy of the given arc in arc_tmp.
			type type_arc_tmp is new type_arc with null record;
			arc_tmp : type_arc_tmp := (arc with null record);

		begin
			-- move arc_tmp so that its center is at 0/0
			move_to (arc_tmp, origin);

			-- the center is not changed:
			result.center := arc.center;
			
			-- calculate the radius of the arc
			result.radius := distance_total (arc_tmp.center, arc_tmp.start_point);

			-- calculate the angles where the arc begins and ends:

			-- NOTE: If x and y are zero then the arctan operation is not possible. 
			-- In this case we assume the resulting angle is zero.
			
			if arc_tmp.start_point.x = zero and arc_tmp.start_point.y = zero then
				result.angle_start := zero_rotation;
			else
				result.angle_start := to_degrees (arctan (
						y => float (arc_tmp.start_point.y),
						x => float (arc_tmp.start_point.x)));
			end if;

			if arc_tmp.end_point.x = zero and arc_tmp.end_point.y = zero then
				result.angle_end := zero_rotation;
			else
				result.angle_end := to_degrees (arctan (
						y => float (arc_tmp.end_point.y),
						x => float (arc_tmp.end_point.x)));
			end if;
	
			-- make sure start and end angle are not equal
			if result.angle_start = result.angle_end then
				raise constraint_error; -- CS warning instead ?
			end if;
			
			-- direction is not changed:
			result.direction := arc.direction;
			
			return result;
		end to_arc_angles;
		
		function get_boundaries (
			arc			: in type_arc;
			line_width	: in type_distance_positive) 
			return type_boundaries 
		is
			half_width : constant type_distance_positive := line_width * 0.5;
			
			-- The current implementation is probably not the best solution.
			-- CS: A more professional approach is required here.
			
			result : type_boundaries; -- to be returned

			-- Take a copy of the given arc in arc_tmp.
			type type_arc_tmp is new type_arc with null record;
			arc_tmp : type_arc_tmp := (arc with null record);

			-- Calculate the radius of the arc:
			radius : type_distance_positive := distance_total (arc.center, arc.start_point);

			-- The quadrant where start and end point are in:
			q_start : type_quadrant;
			q_end   : type_quadrant;

			procedure set_sx is begin result.smallest_x := - radius; end;
			procedure set_gx is begin result.greatest_x :=   radius; end;
			procedure set_sy is begin result.smallest_y := - radius; end;
			procedure set_gy is begin result.greatest_y :=   radius; end;
			
		begin -- boundaries
			-- move arc_tmp so that its center is at 0/0
			move_to (arc_tmp, origin);

			-- Calculate the quadrant where start and end point are in:
			q_start := quadrant (arc_tmp.start_point);
			q_end   := quadrant (arc_tmp.end_point);
			
			-- Calculate the boundaries of start and end point.
			-- For the moment we regard start and end point of the arc being
			-- connected with a straight line, ignoring the line width:
			result := get_boundaries (arc_tmp.start_point, arc_tmp.end_point, zero);

			-- Depending on the quadrants of start and end point, other quadrants may
			-- be crossed. The boundaries (held in result) must be pushed away into x
			-- or y direction if start and end point are not in the same quadrant.
			case q_start is
				when ONE =>
					case q_end is
						when ONE => null; -- same quadrants, leave result as it is

						when TWO => 
							if arc.direction = CCW then
								set_gy;
							else
								set_sy;
							end if;

						when THREE =>
							if arc.direction = CCW then
								set_gy;
								set_sx;
							else
								set_sy;
								set_gx;
							end if;

						when FOUR =>
							if arc.direction = CCW then
								set_gy;
								set_sx;
								set_sy;
							else
								set_gx;
							end if;
					end case;

				when TWO =>
					case q_end is
						when ONE => 
							if arc.direction = CCW then
								set_sx;
								set_sy;
								set_gx;
							else
								set_gy;
							end if;

						when TWO => null; -- same quadrants, leave result as it is

						when THREE =>
							if arc.direction = CCW then
								set_sx;
							else
								set_gy;
								set_gx;
								set_sy;
							end if;

						when FOUR =>
							if arc.direction = CCW then
								set_sx;
								set_sy;
							else
								set_gy;
								set_gx;
							end if;
					end case;
					
				when THREE =>
					case q_end is
						when ONE =>
							if arc.direction = CCW then
								set_sy;
								set_gx;
							else
								set_sx;
								set_gy;
							end if;

						when TWO =>
							if arc.direction = CCW then
								set_sy;
								set_gx;
								set_gy;
							else
								set_sx;
							end if;

						when THREE => null; -- same quadrants, leave result as it is

						when FOUR =>
							if arc.direction = CCW then
								set_sy;
							else
								set_sx;
								set_gy;
								set_gx;
							end if;

					end case;

				when FOUR =>
					case q_end is
						when ONE =>
							if arc.direction = CCW then
								set_gx;
							else
								set_sy;
								set_sx;
								set_gy;
							end if;

						when TWO =>
							if arc.direction = CCW then
								set_gx;
								set_gy;
							else
								set_sy;
								set_sx;
							end if;

						when THREE =>
							if arc.direction = CCW then
								set_gx;
								set_gy;
								set_sx;
							else
								set_sy;
							end if;

						when FOUR => null; -- same quadrants, leave result as it is
					end case;
					
			end case;

			-- The boundaries held in "result" are still relative to the origin (0/0).
			-- They must be moved back to where the given arc is positioned.
			move_by (result, arc.center);

			-- extend the boundaries by half the line width;
			result.smallest_x := result.smallest_x - half_width;
			result.smallest_y := result.smallest_y - half_width;

			result.greatest_x := result.greatest_x + half_width;
			result.greatest_y := result.greatest_y + half_width;
			
			return result;
		end get_boundaries;
		
		function on_arc (
			point		: in type_point;
			arc			: in type_arc;
			catch_zone	: in type_catch_zone := type_distance'small)
			return boolean 
		is
			-- The angle of the given point relative to the
			-- center of the given arc:
			P : type_rotation_positive;

			-- A representation of the given arc in angles:
			arc_angles : constant type_arc_angles := to_arc_angles (arc);
			
			-- make the angles of the arc positive:
			S : type_rotation_positive := to_positive_rotation (arc_angles.angle_start);
			E : type_rotation_positive := to_positive_rotation (arc_angles.angle_end);

			procedure offset_ccw is 
				T : type_rotation_positive;
			begin
				T := 360.0 - S;
				--log (text => "offset" & to_string (T));
				
				S := 0.0;
				E := E + T;
				P := add (P, T);
			end offset_ccw;

			procedure offset_cw is 
				T : type_rotation_positive;
			begin
				T := 360.0 - E;
				E := 0.0;
				S := S + T;
				P := add (P, T);
			end offset_cw;

			distance_center_to_point : constant type_distance_positive := distance_total (point, arc.center);
			
		begin
			--log (text => "center" & to_string (arc.center) 
				 --& " radius" & to_string (arc_angles.radius)
				 --& " start" & to_string (arc.start_point)
				 --& " end" & to_string (arc.end_point)
				 --& " point" & to_string (point)
				 --& " distance center to point" & to_string (distance_center_to_point));

			-- First test whether the given point is on the circumfence of
			-- a virtual circle. The circle has the same radius as the arc.
			if abs (distance_center_to_point - arc_angles.radius) <= catch_zone then
				-- Due to unavoidable rounding errors, a minimal error occurs.

				-- Point is on circumfence of virtual circle.
				--log (text => "on circumfence");

				--log (text => "start" & to_string (S));
				--log (text => "end  " & to_string (E));
				
				-- Compute the angle of the point relative to the center
				-- of the given arc:
				P := to_positive_rotation (angle (distance_polar (arc.center, point)));
				--log (text => "P" & to_string (P));
				
				-- The angle of the point must be between start and end point
				-- of the arc to be considered as "on the arc".
				-- Special problem: The arc may run across the ZDG ("zero degree mark").
				--  In that case the start and end angle and the point angle must first be
				--  rotated so that the arc no longer crossed the ZDG.
				case arc.direction is
					when CW =>
						if S <= E then -- arc crosses the ZDG
							offset_cw;
						end if;
						
						if P <= S and P >= E then
							--log (text => "on cw arc");
							return true;
						else
							--log (text => "not on cw arc");
							return false;
						end if;

					when CCW =>
						if S >= E then -- arc crosses the ZDG
							offset_ccw;
						end if;

						--log (text => "start" & to_string (S));
						--log (text => "end  " & to_string (E));
						--log (text => "point" & to_string (P));

						
						if P >= S and P <= E then
							--log (text => "on ccw arc");
							return true;
						else
							--log (text => "not on ccw arc");
							return false;
						end if;
				end case;
				
			else
				return false; 
			end if;

		end on_arc;


		function get_intersection (
			line	: in type_line_vector;
			arc		: in type_arc)
			return type_intersection_of_line_and_circle
		is
			-- We assume the arc is a virtual circle and compute the
			-- intersections of the line with the virtual circle.
			
			-- Build a virtual circle from the given arc:
			type type_virtual_circle is new type_circle with null record;
			vc : constant type_virtual_circle := (
					center => arc.center, 
					radius => radius_start (arc));

			-- Compute the virtual intersections of line with circle:
			vi : constant type_intersection_of_line_and_circle := 
				get_intersection (line, vc);
			
		begin
			case vi.status is
				when NONE_EXIST => 
					-- line does not meet the virtual circle
					-- and does not meet the given arc either.
					return (status => NONE_EXIST);

				when ONE_EXISTS => 
					-- line is a tangent to the virtual circle

					-- Test whether the point where the tangent meets the
					-- circle is on the given arc:
					if on_arc (to_point (vi.intersection.point), arc) then
						return (ONE_EXISTS, vi.intersection, TANGENT);
					else
						return (status => NONE_EXIST);
					end if;
					
				when TWO_EXIST => 
					-- line is a secant to the virtual circle:

					-- Test whether the points where the secant meets the
					-- circle are on the given arc:

					--log (text => "p1" & to_string (to_point ((vi.intersection_1.point))));
					--log (text => "p2" & to_string (to_point ((vi.intersection_2.point))));
					
					if	on_arc (to_point (vi.intersection_1.point), arc) 
					and on_arc (to_point (vi.intersection_2.point), arc) then
						-- both intersections are on the arc
						return (TWO_EXIST, vi.intersection_1, vi.intersection_2);
						
					elsif on_arc (to_point (vi.intersection_1.point), arc) then
						-- only intersection 1 is on the arc
						return (ONE_EXISTS, vi.intersection_1, SECANT);
						
					elsif on_arc (to_point (vi.intersection_2.point), arc) then
						-- only intersection 2 is on the arc
						return (ONE_EXISTS, vi.intersection_2, SECANT);
						
					else
						--log (text => "x none");
						raise constraint_error;
						--return (status => NONE_EXIST); -- CS should never happen
					end if;					
			end case;
			
		end get_intersection;

		
		function arc_end_point (
			center		: in type_point;
			start_point	: in type_point;	
			angle 		: in type_rotation) -- unit is degrees
			return type_point'class
		is						
			-- Take a copy of the given arc in arc.
			type type_arc_tmp is new type_arc with null record;
			arc : type_arc_tmp; -- := (arc with null record);

			radius : float;
			angle_start, angle_end : float; -- unit is radians
			end_x, end_y : float;
			
		begin -- arc_end_point
			
			-- build an arc from the information available
			arc := (
				center		=> center,
				start_point	=> start_point,
				end_point	=> origin, -- not determined yet
				direction	=> direction_of_rotation (angle)
				);
			
			-- move arc so that its center is at 0/0
			move_to (arc, origin);

			-- calculate the radius of the arc
			radius := float (distance_total (arc.center, arc.start_point));

			-- calculate the angle where the arc begins:

			-- NOTE: If x and y are zero then the arctan operation is not possible. 
			-- In this case we assume the resulting angle is zero.
			if arc.start_point.x = zero and arc.start_point.y = zero then
				angle_start := 0.0;
			else
				angle_start := arctan (
						y => float (arc.start_point.y),
						x => float (arc.start_point.x));
			end if;
			
			-- the angle where the arc ends:
			angle_end := angle_start + to_radians (angle);

			-- The end point of the arc:
			end_y := sin (angle_end) * radius;
			end_x := cos (angle_end) * radius;

			return set (
				x	=> type_distance (end_x),
				y	=> type_distance (end_y));
			
		end arc_end_point;

		procedure move_by (
		-- Moves an arc by the given offset.
			arc		: in out type_arc;
			offset	: in type_point) is
		begin
			move_by (point => arc.center,      offset => offset);
			move_by (point => arc.start_point, offset => offset);
			move_by (point => arc.end_point,   offset => offset);
		end move_by;

		procedure move_to (
			arc			: in out type_arc;
			position	: in type_point) is

			-- compute the offset:
			offset : type_point := type_point (distance_relative (arc.center, position));
		begin
			-- move the center of the arc to the given position
			move_to (arc.center, position);

			-- move start and end point of the arc by the computed offset
			move_by (point => arc.start_point, offset => offset);
			move_by (point => arc.end_point,   offset => offset);
		end move_to;

		procedure mirror (
			arc			: in out type_arc;
			axis		: in type_axis_2d)
		is begin
			mirror (arc.center, axis);
			mirror (arc.start_point, axis);
			mirror (arc.end_point, axis);
			arc.direction := reverse_direction (arc.direction);
		end mirror;

		procedure rotate_by (
			arc			: in out type_arc;
			rotation	: in type_rotation) is
		begin
			rotate_by (arc.center, rotation);
			rotate_by (arc.start_point, rotation);
			rotate_by (arc.end_point, rotation);
		end;
		
		procedure move_by (
			circle	: in out type_circle;
			offset	: in type_point)
		is begin
			move_by (point	=> circle.center,	offset => offset);
		end move_by;
		
		procedure mirror (
			circle		: in out type_circle;
			axis		: in type_axis_2d) is
		begin
			mirror (circle.center, axis);
		end mirror;
		
		procedure rotate_by (
			circle		: in out type_circle;
			rotation	: in type_rotation) is
		begin
			rotate_by (circle.center, rotation);
		end;
		
		function get_boundaries (
			circle		: in type_circle;
			line_width	: in type_distance_positive)						
			return type_boundaries 
		is
			result : type_boundaries;

			half_width : constant type_distance_positive := line_width * 0.5;
		begin
			-- X axis
			result.smallest_x := circle.center.x - circle.radius;
			result.greatest_x := circle.center.x + circle.radius;

			-- Y axis
			result.smallest_y := circle.center.y - circle.radius;
			result.greatest_y := circle.center.y + circle.radius;

			
			-- extend the boundaries by half the line width;
			result.smallest_x := result.smallest_x - half_width;
			result.smallest_y := result.smallest_y - half_width;

			result.greatest_x := result.greatest_x + half_width;
			result.greatest_y := result.greatest_y + half_width;
			
			return result;
		end get_boundaries;

		
		function on_circle (
			point		: in type_point;
			circle		: in type_circle;
			catch_zone	: in type_catch_zone := type_distance'small)
			return boolean 
		is begin
			if distance_total (point, circle.center) - circle.radius <= catch_zone then
				return true;
			else
				return false; 
			end if;
		end on_circle;


		function get_point_to_circle_status (
			point		: in type_point;
			circle		: in type_circle)
			return type_point_status
		is begin
			if distance_total (point, circle.center) < circle.radius then
				return INSIDE;
			else
				return OUTSIDE; 
			end if;
		end get_point_to_circle_status;
		
		
		function get_tangent_angle (p : in type_point) 
			return type_tangent_angle_circle
		is
			a : type_rotation := angle (distance_polar (origin, p));
		begin
			--put_line (to_string (a));

			-- The angle a ranges from -180 to 180 degrees.
			
			case quadrant (p) is
				when ONE	=> a := a - 90.0;
				when TWO	=> a := a - 90.0;
				when THREE	=> a := a + 90.0;
				when FOUR	=> a := a + 90.0;
			end case;
			
			return a;
		end get_tangent_angle;

		
		
		function get_intersection (
			line	: in type_line_vector;
			circle	: in type_circle)
			return type_intersection_of_line_and_circle
		is
			-- This function bases on the approach by
			-- Weisstein, Eric W. "Circle-Line Intersection." 
			-- From MathWorld--A Wolfram Web Resource. 
			-- <https://mathworld.wolfram.com/Circle-LineIntersection.html>.
			-- It has been further-on extended so that the angles
			-- of intersections are computed along with the actual points
			-- of intersection.

			-- The appoach assumes the circle center at 0/0.
			-- So we must first move the line by
			-- the given center of the circle. The intersections,
			-- if any exist, must finally be moved back by this offset:
			offset : constant type_point := circle.center;
			
			-- The circle radius is:
			r : constant float := float (circle.radius);
			
			-- The line starts here:
			ps : type_point;
			x1, y1 : float;

			-- The line ends here:
			pe : type_point;
			x2, y2 : float;
			
			x, y, dx, dy, dr, DI : float;

			-- scratch variables:
			v1 : type_vector;
			a, b, c, d : float;

			zero : constant float := 0.0;

			s : type_intersection_status_of_line_and_circle;
			intersection_1, intersection_2 : type_point;

			line_angle : constant type_rotation := get_angle (line);

			intersection_angle_1, intersection_angle_2 : type_rotation;

			-- Computes the angle of intersection of the given line with
			-- the circle at point p.
			-- NOTE: Since we assume a secant, the angle
			-- line_angle is travelling with, must not be a multiple of 90 degrees !
			function compute_intersection_angle (p : in type_point) 
				return type_rotation
			is
				result : type_rotation;

				-- Compute the tangent at the intersection:
				tangent_angle : type_rotation := get_tangent_angle (p);
			begin
				--log (text => "line angle:" & to_string (line_angle));
				--log (text => "tangent angle A:" & to_string (tangent_angle));
				
				if tangent_angle < 0.0 then
					tangent_angle := abs (tangent_angle);
				else
					tangent_angle := 180.0 - tangent_angle;
				end if;

				--log (text => "tangent angle B:" & to_string (tangent_angle));
				
				result := line_angle + tangent_angle;

				if result > 180.0 then
					result := result - 180.0;
				end if;
				
				--log (text => "intersection angle:" & to_string (result));
				
				return result;
			end compute_intersection_angle;

			
		begin -- get_intersection
			
			-- compute start and end point of given line:
			ps := to_point (line.v_start);
			--put_line ("start " & to_string (ps));
			
			v1 := scale (line.v_direction, 1.0);
			pe := to_point (add (line.v_start, v1));
			--put_line ("end   " & to_string (pe));
			
			-- Move start and end point of line by offset 
			-- (which is the center of the given circle):
			move_by (ps, type_point (invert (offset)));
			move_by (pe, type_point (invert (offset)));

			--put_line ("o start " & to_string (ps));
			--put_line ("o end   " & to_string (pe));

			x1 := float (ps.x);
			y1 := float (ps.y);
			
			x2 := float (pe.x);
			y2 := float (pe.y);
			
			dx := x2 - x1;
			dy := y2 - y1;
			--put_line ("dx" & float'image (dx) & " dy" & float'image (dy));

			dr := sqrt (dx ** 2 + dy ** 2);
			--put_line ("dr" & float'image (dr));
			
			DI := x1 * y2 - x2 * y1;
			--put_line ("DI" & float'image (DI));
			
			b := dr ** 2;
			a := r ** 2;
			c := DI ** 2;
			
			d := a * b - c; -- incidence of line and circle

			if d < zero then
				s := NONE_EXIST;
				
				return (status => NONE_EXIST);
				
			elsif d = zero then
				s := ONE_EXISTS; -- tangent

				x := (DI * dy) / b;
				y := (-DI * dx) / b;

				intersection_1 := type_point (set (type_distance (x), type_distance (y)));
				
				-- Move computed intersection back by offset
				-- (Which is the center of the given circle):
				move_by (intersection_1, offset);

				return (ONE_EXISTS, 
						(point => to_vector (intersection_1), angle => line_angle),
						TANGENT);

				-- NOTE: The angle of the travel direction of the given line
				-- is now the angle of the tangent at this single intersection point.
				
			else -- d > zero
				s := TWO_EXIST; -- two intersections

				-- COMPUTE 1ST INTERSECTION:
				x := ( DI * dy + sgn (dy) * dx * sqrt (d)) / b;
				y := (-DI * dx + abs (dy) * sqrt (d))      / b;

				-- Compose the point of intersection 1:
				intersection_1 := type_point (set (type_distance (x), type_distance (y)));

				intersection_angle_1 := compute_intersection_angle (intersection_1);
					
				-- Move computed intersection 1 back by offset
				-- (Which is the center of the given circle):
				move_by (intersection_1, offset);


				
				-- COMPUTE 2ND INTERSECTION:				
				x := ( DI * dy - sgn (dy) * dx * sqrt (d)) / b;
				y := (-DI * dx - abs (dy) * sqrt (d))      / b;
					  
				-- Compose the point of intersection 2:
				intersection_2 := type_point (set (type_distance (x), type_distance (y)));

				intersection_angle_2 := compute_intersection_angle (intersection_2);
				
				-- Move computed intersection 2 back by offset
				-- (Which is the center of the given circle):
				move_by (intersection_2, offset);				


				
				return (TWO_EXIST, 
						(point => to_vector (intersection_1), angle => intersection_angle_1),
						(point => to_vector (intersection_2), angle => intersection_angle_2)
					   );

				
			end if;

		end get_intersection;


		function order_intersections (
			-- The start point of the line that intersects the circle.
			-- The start point must be outside the circle and will
			-- be passed through to the result unchanged.
			start_point		: in type_point;

			intersections	: in type_intersection_of_line_and_circle)
			return type_ordered_line_circle_intersections
		is
			result : type_ordered_line_circle_intersections := 
				(start_point => start_point, others => <>); -- pass start point through

			i : constant type_intersection_of_line_and_circle (TWO_EXIST) := intersections;

			ip1, ip2 : type_point;
			
			d1, d2 : type_distance_positive;

		begin
			-- get intersection point 1 and 2:
			ip1 := to_point (i.intersection_1.point);
			ip2 := to_point (i.intersection_2.point);
			
			-- the distance from start point to intersection point 1:
			d1 := distance_total (start_point, ip1);

			-- the distance from start point to intersection point 2:
			d2 := distance_total (start_point, ip2);

			if d1 < d2 then -- point ip1 is closer to start point that ip2
				result.entry_point := i.intersection_1;
				result.exit_point  := i.intersection_2;
				
			elsif d1 > d2 then -- point ip2 is closer to start point than ip1
				result.entry_point := i.intersection_2;
				result.exit_point  := i.intersection_1;

			else -- point ip1 has same distance to start point as ip2
				raise constraint_error;
			end if;
				
			return result;
		end order_intersections;


		
		
		function to_string (line : in type_line) return string is begin
			return latin_1.space 
				& "start" & to_string (line.start_point) 
				& " end" & to_string (line.end_point);
		end;

		function to_string (arc : in type_arc) return string is
		-- Returns the start, end point and angle of the given arc as string.
		begin
			return latin_1.space 
				& "center" & to_string (arc.center) 
				& " start" & to_string (arc.start_point) 
				& " end" & to_string (arc.end_point);
		end to_string;

		function to_string (circle : in type_circle) return string is
		-- Returns the center and radius of the given circle as string.
		begin
			return latin_1.space
				& "center" & to_string (circle.center) 
				& " radius" & to_string (circle.radius);
		end to_string;


		function get_nearest_corner_point (
			polygon		: in type_polygon_base;
			reference	: in type_point)
			return type_point
		is
			use pac_polygon_segments;
		
			result : type_point := origin;
			
			d1 : type_distance_positive := zero;
			d2 : type_distance_positive := absolute (distance_polar (reference, far_upper_right));

			procedure query_segment (c : in pac_polygon_segments.cursor) is
				s : constant type_polygon_segment := element (c);
			begin
				case s.shape is
					when LINE =>

						-- test start point
						d1 := absolute (distance_polar (reference, s.segment_line.start_point));
						
						if d1 < d2 then
							d2 := d1;
							
							result := s.segment_line.start_point;
						end if;

						-- test end point
						d1 := absolute (distance_polar (reference, s.segment_line.end_point));
						
						if d1 < d2 then
							d2 := d1;
							
							result := s.segment_line.end_point;
						end if;

					when ARC =>
						-- test start point
						d1 := absolute (distance_polar (reference, s.segment_arc.start_point));
						
						if d1 < d2 then
							d2 := d1;
							
							result := s.segment_arc.start_point;
						end if;

						-- test end point
						d1 := absolute (distance_polar (reference, s.segment_arc.end_point));
						
						if d1 < d2 then
							d2 := d1;
							
							result := s.segment_arc.end_point;
						end if;
						
				end case;
			end query_segment;
			
		begin
			if polygon.contours.circular then
				raise constraint_error with 
				"Polygon consists of a single circle and thus nas no corners !";
			else
				polygon.contours.segments.iterate (query_segment'access);				
			end if;			
			
			return result;
		end get_nearest_corner_point;


		function get_left_end (
			line		: in type_line;
			boundaries	: in type_boundaries := boundaries_default)
			return type_point
		is
			p : type_point; -- to be returned
			b : type_boundaries := boundaries;								   
		begin
			-- If no boundaries provided, compute them.
			-- Otherwise use provided boundaries as they are:
			if b = boundaries_default then
				b := get_boundaries (line, zero); -- a polygon line has zero width
			end if;
			
			if b.smallest_x = line.start_point.x then
				p := line.start_point;
				
			elsif b.smallest_x = line.end_point.x then
				p := line.end_point;

			else
				-- If boundaries where provided and neither start nor end point
				-- of line matches, issue exception:
				raise constraint_error with to_string (b) 
					& " invalid for line" & to_string (line) & " !";
			end if;
				
			return p;
		end get_left_end;

		function get_right_end (
			line		: in type_line;
			boundaries	: in type_boundaries := boundaries_default)
			return type_point
		is
			p : type_point; -- to be returned
			b : type_boundaries := boundaries;								   
		begin
			-- If no boundaries provided, compute them.
			-- Otherwise use provided boundaries as they are:
			if b = boundaries_default then
				b := get_boundaries (line, zero); -- a polygon line has zero width
			end if;
			
			if b.greatest_x = line.start_point.x then
				p := line.start_point;
				
			elsif b.greatest_x = line.end_point.x then
				p := line.end_point;

			else
				-- If boundaries where provided and neither start nor end point
				-- of line matches, issue exception:
				raise constraint_error with to_string (b) 
					& " invalid for line" & to_string (line) & " !";
			end if;
				
			return p;
		end get_right_end;

		function get_lower_end (
			line		: in type_line;
			boundaries	: in type_boundaries := boundaries_default)
			return type_point
		is
			p : type_point; -- to be returned
			b : type_boundaries := boundaries;								   
		begin
			-- If no boundaries provided, compute them.
			-- Otherwise use provided boundaries as they are:
			if b = boundaries_default then
				b := get_boundaries (line, zero); -- a polygon line has zero width
			end if;
			
			if b.smallest_y = line.start_point.x then
				p := line.start_point;
				
			elsif b.smallest_y = line.end_point.x then
				p := line.end_point;

			else
				-- If boundaries where provided and neither start nor end point
				-- of line matches, issue exception:
				raise constraint_error with to_string (b) 
					& " invalid for line" & to_string (line) & " !";
			end if;
				
			return p;
		end get_lower_end;

		function get_upper_end (
			line		: in type_line;
			boundaries	: in type_boundaries := boundaries_default)
			return type_point
		is
			p : type_point; -- to be returned
			b : type_boundaries := boundaries;								   
		begin
			-- If no boundaries provided, compute them.
			-- Otherwise use provided boundaries as they are:
			if b = boundaries_default then
				b := get_boundaries (line, zero); -- a polygon line has zero width
			end if;
			
			if b.greatest_y = line.start_point.x then
				p := line.start_point;
				
			elsif b.greatest_y = line.end_point.x then
				p := line.end_point;

			else
				-- If boundaries where provided and neither start nor end point
				-- of line matches, issue exception:
				raise constraint_error with to_string (b) 
					& " invalid for line" & to_string (line) & " !";
			end if;
				
			return p;
		end get_upper_end;

		
		
		--procedure append_segment_line (
			--polygon	: in out type_polygon_base'class;
			--segment	: in type_polygon_line)
		--is begin
			--polygon.segments.lines.append (segment);

			--polygon.segments_total := polygon.segments_total + 1;
		--end append_segment_line;

		--procedure append_segment_arc (
			--polygon	: in out type_polygon_base'class;
			--segment	: in type_polygon_arc)
		--is begin
			--polygon.segments.arcs.append (segment);

			--polygon.segments_total := polygon.segments_total + 1;
		--end append_segment_arc;

		--procedure append_segment_circle (
			--polygon	: in out type_polygon_base'class;
			--segment	: in type_polygon_circle)
		--is begin
			--polygon.segments.circles.append (segment);

			--polygon.segments_total := polygon.segments_total + 1;
		--end append_segment_circle;


		--function get_segments_on_corner_point (
			--polygon	: in type_polygon_base;
			--corner	: in type_point)
			--return type_polygon_segments
		--is
			--use pac_polygon_lines;
			--use pac_polygon_arcs;
			
			--result : type_polygon_segments;

			--procedure query_line (c : in pac_polygon_lines.cursor) is
				--line : constant type_polygon_line := element (c);
			--begin
				--if line.start_point = corner then
					--result.lines.append (line);
					
				--elsif line.end_point = corner then
					--result.lines.append (line);
				--end if;
			--end query_line;

			--procedure query_arc (c : in pac_polygon_arcs.cursor) is
				--arc : constant type_polygon_arc := element (c);
			--begin
				--if arc.start_point = corner then
					--result.arcs.append (arc);
					
				--elsif arc.end_point = corner then
					--result.arcs.append (arc);
				--end if;
			--end query_arc;
			
		--begin
			--polygon.segments.lines.iterate (query_line'access);
			--polygon.segments.arcs.iterate (query_arc'access);
			
			---- circles are not tested, because a circle does not have corners
			
			--return result;
		--end get_segments_on_corner_point;


		
		--procedure load_lines (
			--polygon		: in out type_polygon_base'class;
			--lines		: in pac_polygon_lines.list)
		--is
			--use pac_polygon_lines;
			--l : constant count_type := length (lines);
		--begin
			---- set the total number of segments:
			--polygon.segments_total := type_polygon_segment_count (l);

			---- Clear existing segments and 
			---- assign the given list of lines:
			--polygon.segments := (others => <>);
			--polygon.segments.lines := lines;
		--end load_lines;

		--procedure load_circles (
			--polygon		: in out type_polygon_base'class;
			--circles		: in pac_polygon_circles.list)
		--is
			--use pac_polygon_circles;
			--c : constant count_type := length (circles);
		--begin
			---- set the total number of segments:
			--polygon.segments_total := type_polygon_segment_count (c);

			---- Clear existing segments and 
			---- assign the given list of circles:
			--polygon.segments := (others => <>);
			--polygon.segments.circles := circles;
		--end load_circles;
		
		procedure load_segments (
			polygon		: in out type_polygon_base;
			segments	: in type_polygon_segments_2)
		is begin
			polygon.contours := segments;
		end load_segments;

		procedure delete_segments (polygon : in out type_polygon_base) 
		is begin
			polygon.contours := (others => <>);
		end delete_segments;			

		procedure append_segment (
			polygon	: in out type_polygon_base;
			segment	: in type_polygon_segment)
		is begin
			-- CS check discriminant and issue helpful error message ?
			
			polygon.contours.segments.append (segment);			
		end append_segment;

		procedure set_circle (
			polygon	: in out type_polygon_base;
			circle	: in type_circle'class)
		is begin
			-- CS check discriminant and issue helpful error message ?
			--polygon := (
				   --contours	=> (circular => true, circle => circle));
			
			polygon.contours.circle := type_circle (circle);
		end set_circle;
					
		
		function get_segments (
			polygon : in type_polygon_base) 
			return type_polygon_segments_2
		is begin
			return polygon.contours;
		end get_segments;

		--function get_segments_total (polygon : in type_polygon_base)
			--return type_polygon_segment_count
		--is begin
			--return polygon.segments_total;
		--end get_segments_total;

		procedure transpose_polygon (
			polygon	: in out type_polygon_base'class;
			offset	: in type_distance)
		is 
			procedure move (point : in out type_point) is
				new_y : type_distance;
			begin
				new_y := offset - y (point);
				set (Y, new_y, point);
			end move;

			use pac_polygon_segments;

			procedure move_segment (c : in pac_polygon_segments.cursor) is

				procedure do_line (s : in out type_polygon_segment) is begin 
					move (s.segment_line.start_point);
					move (s.segment_arc.end_point);
				end;
				
				procedure do_arc (s : in out type_polygon_segment) is begin
					move (s.segment_arc.start_point);
					move (s.segment_arc.end_point); 
					move (s.segment_arc.center); 
				end;

			begin -- move_segment
				case element (c).shape is
					
					when LINE =>
						update_element (
							container	=> polygon.contours.segments,
							position	=> c,
							process		=> do_line'access);

					when ARC =>
						update_element (
							container	=> polygon.contours.segments,
							position	=> c,
							process		=> do_arc'access);

				end case;
			end move_segment;
			
		begin -- transpose_polygon			
			if polygon.contours.circular then

				-- move the single circle that forms the polygon:
				move (polygon.contours.circle.center);
			else
				-- move lines and arcs:
				polygon.contours.segments.iterate (move_segment'access);
			end if;
			
		end transpose_polygon;
		
		function to_polygon (
			arguments : in type_fields_of_line)
			return type_polygon_base'class
		is
			result : type_polygon; -- will be converted back to class type on return
			-- NOTE: The default discriminant if the result makes it a
			-- polygon consisting of lines an arcs.
			-- If a circle is to be added to the polygon then the discriminant
			-- changes accordingly.

			function f (place : in count_type) return string is begin
				return to_lower (get_field (arguments, place));
			end;
			
			l : type_line;
			a : type_arc;
			c : type_circle;

			-- The shape of the segment being processed:
			shape : type_shape;

			-- The place in arguments at which we fetch a field from:
			p : count_type := 1;

		begin
			-- Iterate all fields of given list of arguments:
			while p <= field_count (arguments) loop

				-- If a keyword like "line", "arc" or "circle" occurs,
				-- then set shape accordingly:
				if f (p) = to_string (LINE) then
					--put_line ("line");
					shape := LINE;
					
				elsif f (p) = to_string (ARC) then
					--put_line ("arc");
					shape := ARC;

				elsif f (p) = to_string (CIRCLE) then
					--put_line ("circle");
					shape := CIRCLE;

					-- Once a circle occurs, only a single
					-- circle can be assigned to the result.
					-- The discriminant of the result mutates here.
					-- An attempt to append a line or an arc to the
					-- polygon segments will cause a exception.
					result.contours := (
						circular	=> true,
						others		=> <>);
					
				end if;

				-- Fetch the parameters for the shape by
				-- looking ahead of p:
				case shape is
					when LINE => -- line 0 0 100 0
						l.start_point := type_point (set (
								x => to_distance (f (p + 1)),
								y => to_distance (f (p + 2))));

						l.end_point := type_point (set (
								x => to_distance (f (p + 3)),
								y => to_distance (f (p + 4))));

						result.contours.segments.append (new_item => (LINE, l));

						-- fast forward p to next shape:
						p := p + 5;

						
					when ARC => -- arc 50 100 100 100 0 100 ccw
						a.center := type_point (set (
								x => to_distance (f (p + 1)),
								y => to_distance (f (p + 2))));
							
						a.start_point := type_point (set (
								x => to_distance (f (p + 3)),
								y => to_distance (f (p + 4))));

						a.end_point := type_point (set (
								x => to_distance (f (p + 5)),
								y => to_distance (f (p + 6))));

						a.direction := to_direction (f (p + 7));
						
						result.contours.segments.append (new_item => (ARC, a));

						-- fast forward p to next shape:						
						p := p + 8;

						
					when CIRCLE => -- circle 40 40 10
						c.center := type_point (set (
								x => to_distance (f (p + 1)),
								y => to_distance (f (p + 2))));

						c.radius := to_distance (f (p + 3));

						result.contours.circle := c;
						
						-- NOTE: No further shape must follow.
						-- So we do not move p further.
				end case;
			end loop;

			
			return type_polygon_base (result);

			-- CS exception handler required for invalid fields:
			
			--exception when event: others =>
				--put_line (exception_message);
				--return p;
		
		end to_polygon;
		

		
		function get_boundaries (
			polygon		: in type_polygon_base;
			line_width	: in type_distance_positive)
			return type_boundaries 
		is
			result : type_boundaries; -- to be returned

			half_width : constant type_distance_positive := line_width * 0.5;
			
			use pac_polygon_segments;

			procedure query_segment (c : in pac_polygon_segments.cursor) is begin
				case element (c).shape is
					when LINE =>
						union (result, get_boundaries (element (c).segment_line, zero));

					when ARC =>
						union (result, get_boundaries (element (c).segment_arc, zero));
				end case;						
			end query_segment;
			
		begin -- get_boundaries
			if polygon.contours.circular then

				-- Get the boundaries of the single circle:
				union (result, get_boundaries (polygon.contours.circle, zero));
				
			else
				-- Iterate lines and arcs:
				polygon.contours.segments.iterate (query_segment'access);
			end if;

						
			-- Extend the boundaries by half the line width;
			result.smallest_x := result.smallest_x - half_width;
			result.smallest_y := result.smallest_y - half_width;

			result.greatest_x := result.greatest_x + half_width;
			result.greatest_y := result.greatest_y + half_width;
			
			return result;
		end get_boundaries;
		
		function to_string (gaps : in pac_polygon_gaps.list) return string is
			use pac_polygon_gaps;
			use ada.strings.unbounded;
			result : unbounded_string;
			
			procedure query_gap (g : in pac_polygon_gaps.cursor) is 
				scratch : unbounded_string;
			begin
				if next (g) /= pac_polygon_gaps.no_element then
					scratch := to_unbounded_string (to_string (element (g)) & comma);
				else
					scratch := to_unbounded_string (to_string (element (g)));
				end if;

				result := result & scratch;
			end query_gap;
		
		begin
			iterate (gaps, query_gap'access);

			return to_string (result);
		end to_string;
		
		function is_closed (
			polygon	: in type_polygon_base)
			return type_polygon_status is

			use pac_polygon_segments;
			
			--use pac_polygon_lines;
			--use pac_polygon_arcs;
			--use pac_polygon_circles;

			-- The functions get_line, get_arc and get_circle search for a polygon segment (by its id)
			-- and return a cursor to the segment if it exists. Otherwise they return no_element:
			
			--function get_line (segment : in type_polygon_segment_id) return pac_polygon_lines.cursor is
				--c : pac_polygon_lines.cursor := polygon.segments.lines.first;
				--found : boolean := false;

				--procedure query_line (l : in type_polygon_line) is begin
					--if l.id = segment then
						--found := true;
					--end if;
				--end query_line;

			--begin -- get_line
				--while c /= pac_polygon_lines.no_element loop
					--query_element (c, query_line'access);

					--if found = true then exit; end if;
					
					--next (c);
				--end loop;

				--return c; -- should be no_element if not found. points to the segment if found.
			--end get_line;

			--function get_arc (segment : in type_polygon_segment_id) return pac_polygon_arcs.cursor is 
				--c : pac_polygon_arcs.cursor := polygon.segments.arcs.first;
				--found : boolean := false;

				--procedure query_arc (l : in type_polygon_arc) is begin
					--if l.id = segment then
						--found := true;
					--end if;
				--end query_arc;
				
			--begin -- get_arc
				--while c /= pac_polygon_arcs.no_element loop
					--query_element (c, query_arc'access);

					--if found = true then exit; end if;
					
					--next (c);
				--end loop;

				--return c; -- should be no_element if not found. points to the segment if found.
			--end get_arc;
			
			--function get_circle (segment : in type_polygon_segment_id) return pac_polygon_circles.cursor is 
				--c : pac_polygon_circles.cursor := polygon.segments.circles.first;
				--found : boolean := false;

				--procedure query_circle (l : in type_polygon_circle) is begin
					--if l.id = segment then
						--found := true;
					--end if;
				--end query_circle;
				
			--begin -- get_circle
				--while c /= pac_polygon_circles.no_element loop
					--query_element (c, query_circle'access);

					--if found = true then exit; end if;
					
					--next (c);
				--end loop;

				--return c; -- should be no_element if not found. points to the segment if found.
			--end get_circle;

			-- Here the result of get_line, get_arc and get_circle is stored temporarily:
			--cl : pac_polygon_lines.cursor;
			--ca : pac_polygon_arcs.cursor;
			--cc : pac_polygon_circles.cursor;

			-- Goes false once a gap has been detected:
			closed : boolean := true;

			-- The point where the polyon outline starts:
			start_point		: type_point;

			-- The end point of a segment. Once the last segment has been processed,
			-- this point must match the start point:
			last_end_point	: type_point;

			-- Goes true once a start point has been set:
			started : boolean := false;

			-- Here we collect the points where a gap begins:
			use pac_polygon_gaps;
			gaps : pac_polygon_gaps.list;

			-- Sets the start point of the outline if the start point
			-- has not been set already.
			-- Clears the closed-flag if the given point does NOT
			-- match the last_end_point and appends the last_end_point to
			-- the list of gaps.
			procedure set_start_point (p : in type_point) is begin
				if not started then
					start_point := p;
					last_end_point := start_point;
					started := true;
				end if;

				if p /= last_end_point then
					closed := false;
					append (gaps, last_end_point);
				end if;

			end set_start_point;

			procedure query_segment (c : in pac_polygon_segments.cursor) is begin
				case element (c).shape is
					when LINE =>
						set_start_point (element (c).segment_line.start_point);
						last_end_point := element (c).segment_line.end_point;

					when ARC =>
						set_start_point (element (c).segment_arc.start_point);
						last_end_point := element (c).segment_arc.end_point;
						
				end case;
			end query_segment;
			
		begin -- is_closed
			
			-- Iterate segments of given polygon. For each iteration s indicates the
			-- segment to be checked. It can be among lines (most likely), among arcs (less likely)
			-- and among circles (least likely). The functions get_line, get_arc and get_circle
			-- return a cursor to the segment if it is among lines, arcs or circles.
			-- Otherwise get_line, get_arc or get_circle return no_element.
			--for s in type_polygon_segment_id'first .. polygon.segments_total loop

				---- Search the segment among the lines:
				--cl := get_line (s);
				--if cl /= pac_polygon_lines.no_element then

					--set_start_point (element (cl).start_point);
					--last_end_point := element (cl).end_point;

				---- If segment not found among lines, search among arcs:
				--else 
					--ca := get_arc (s);
					--if ca /= pac_polygon_arcs.no_element then
						
						--set_start_point (element (ca).start_point);
						--last_end_point := element (ca).end_point;
						
					---- If segment not found among arcs, search among circles:
					--else
						--cc := get_circle (s);
						--if cc /= pac_polygon_circles.no_element then

							--set_start_point (element (cc).center);
							--last_end_point := element (cc).center;
							
						--else
							---- If segment is not among circles, we have a problem:
							--raise constraint_error; -- CS should never happen.
						--end if;
						
					--end if;
				--end if;

			--end loop;

			
			if polygon.contours.circular then
				closed := true; -- because this is a single circle
			else
				-- Iterate lines and arcs:
				polygon.contours.segments.iterate (query_segment'access);
			end if;

			
			-- Start point and end point of polygon outline must match:
			if last_end_point /= start_point then
				closed := false;
				append (gaps, last_end_point);
			end if;
			
			-- Return the polygon status:
			if closed then
				return (closed => true);
			else
				return (closed => false, gaps => gaps);
			end if;
		end is_closed;
		
		procedure move_by (
			polygon	: in out type_polygon_base;
			offset	: in type_point) 
		is
			use pac_polygon_segments;

			procedure move_segment (c : in pac_polygon_segments.cursor) is

				procedure do_line (s : in out type_polygon_segment) is begin 
					move_by (s.segment_line, offset);
				end;
				
				procedure do_arc (s : in out type_polygon_segment) is begin
					move_by (s.segment_arc, offset);
				end;

			begin -- move_segment
				case element (c).shape is
					
					when LINE =>
						update_element (
							container	=> polygon.contours.segments,
							position	=> c,
							process		=> do_line'access);

					when ARC =>
						update_element (
							container	=> polygon.contours.segments,
							position	=> c,
							process		=> do_arc'access);

				end case;
			end move_segment;
			
		begin -- move_by
			if polygon.contours.circular then

				-- move the single circle that forms the polygon:
				move_by (polygon.contours.circle, offset);
			else
				-- move lines and arcs:
				polygon.contours.segments.iterate (move_segment'access);
			end if;
		end move_by;

		procedure mirror (
			polygon	: in out type_polygon_base;
			axis	: in type_axis_2d) 
		is
			use pac_polygon_segments;

			procedure mirror_segment (c : in pac_polygon_segments.cursor) is

				procedure do_line (s : in out type_polygon_segment) is begin 
					mirror (s.segment_line, axis);
				end;
				
				procedure do_arc (s : in out type_polygon_segment) is begin
					mirror (s.segment_arc, axis);
				end;

			begin -- move_segment
				case element (c).shape is
					
					when LINE =>
						update_element (
							container	=> polygon.contours.segments,
							position	=> c,
							process		=> do_line'access);

					when ARC =>
						update_element (
							container	=> polygon.contours.segments,
							position	=> c,
							process		=> do_arc'access);

				end case;
			end mirror_segment;
			
		begin -- mirror
			if polygon.contours.circular then

				-- mirror the single circle that forms the polygon:
				mirror (polygon.contours.circle, axis);
			else
				-- move lines and arcs:
				polygon.contours.segments.iterate (mirror_segment'access);
			end if;
		end mirror;

		procedure rotate_by (
			polygon		: in out type_polygon_base;
			rotation	: in type_rotation) 
		is
			use pac_polygon_segments;

			procedure rotate_segment (c : in pac_polygon_segments.cursor) is

				procedure do_line (s : in out type_polygon_segment) is begin 
					rotate_by (s.segment_line, rotation);
				end;
				
				procedure do_arc (s : in out type_polygon_segment) is begin
					rotate_by (s.segment_arc, rotation);
				end;

			begin -- rotate_segment
				case element (c).shape is
					
					when LINE =>
						update_element (
							container	=> polygon.contours.segments,
							position	=> c,
							process		=> do_line'access);

					when ARC =>
						update_element (
							container	=> polygon.contours.segments,
							position	=> c,
							process		=> do_arc'access);

				end case;
			end rotate_segment;
			
		begin -- rotate_by
			if polygon.contours.circular then

				-- rotate the single circle that forms the polygon:
				rotate_by (polygon.contours.circle.center, rotation);
			else
				-- rotate lines and arcs:
				polygon.contours.segments.iterate (rotate_segment'access);
			end if;			
		end rotate_by;



		
		--procedure reorder_segments ( -- experimental
			--polygon	: in out type_polygon_base)
		--is
			--lower_left_corner : type_lower_left_corner := get_lower_left_corner (polygon);
			--polygon_start_point : type_point;
			
			--use pac_polygon_lines;
			--use pac_polygon_arcs;
			
			--function get_smallest_x (row : in type_distance) -- CS no need
				--return type_distance 
			--is
				--sx : type_distance := type_distance'last;

				--line_found : boolean := false;
				--first_line : type_polygon_line;
				
				--procedure query_line (c : in pac_polygon_lines.cursor) is
					--line : constant type_polygon_line := element (c);
				--begin
					---- skip a vertical line:
					--if line.start_point.x /= line.end_point.x then

						---- start or end point must be on the given row:
						
						--if line.start_point.y = row then
							--if line.start_point.x < sx then
								--sx := line.start_point.x;

								--first_line := line;
								--line_found := true;
							--end if;
						--end if;
						
						--if line.end_point.y = row then
							--if line.end_point.x < sx then
								--sx := line.end_point.x;

								--first_line := type_polygon_line (reverse_line (line));
								--line_found := true;
							--end if;
						--end if;

					--end if;
				--end query_line;

				--arc_found : boolean := false;
				--first_arc : type_polygon_arc;

				--procedure query_arc (c : in pac_polygon_arcs.cursor) is
					--arc : constant type_polygon_arc := element (c);

					--start_or_end_on_row : boolean := false;

					--procedure compare_start is begin
						--if arc.start_point.x < sx then
							--sx := arc.start_point.x;
							--first_arc := arc;
							--arc_found := true;
							
							---- If a line was found earlier, then it does not matter anymore:
							--line_found := false;
						--end if;
					--end compare_start;

					--procedure compare_end is begin
						--if arc.end_point.x < sx then
							--sx := arc.end_point.x;
							--first_arc := type_polygon_arc (reverse_arc (arc));
							--arc_found := true;
							
							---- If a line was found earlier, then it does not matter anymore:
							--line_found := false;
						--end if;					
					--end compare_end;
					
				--begin -- query_arc
					---- skip a "vertical" arc:
					--if arc.start_point.x /= arc.end_point.x then

						---- Start or end point of arc must be on the given row.
						--if arc.start_point.y = row then
							--start_or_end_on_row := true;
							--compare_start;
						--end if;
						
						--if arc.end_point.y = row then
							--start_or_end_on_row := true;
							--compare_end;
						--end if;

						---- If none of them is on the given row, then
						---- the lower edge of the arc boundaries must be on the row:
						--if not start_or_end_on_row then
							--if get_boundaries (arc, zero).smallest_y = row then
								--compare_start;
								--compare_end;							
							--end if;
						--end if;

					--end if;
				--end query_arc;
				
			--begin
				--polygon.segments.lines.iterate (query_line'access);
				--polygon.segments.arcs.iterate (query_arc'access);

				----if arc_found then
				----if line_found then
				--return zero;
			--end get_smallest_x;

			--type type_segment_category is (LINE, ARC);
			--type type_first_segment (cat : type_segment_category) is record
				--case cat is
					--when LINE	=> line_segment : type_polygon_line;
					--when ARC	=> arc_segment : type_polygon_arc;
				--end case;
			--end record;

			--function get_lowest_right_segment (
				--corner		: in type_point;
				--segments	: in type_polygon_segments)
				--return type_first_segment
			--is
				--result : type_first_segment (cat => LINE) := (cat => LINE, others => <>);

				--subtype type_direction is type_rotation range 
					--(-90.0 + type_rotation'small) .. (+90.0 - type_rotation'small);
					
				--d1 : type_rotation := zero_rotation;
				--d2 : type_direction := type_direction'last;

				--selected_line : type_polygon_line;
				--line_found : boolean := false;
				
				--procedure query_line (c : in pac_polygon_lines.cursor) is
					--line : constant type_polygon_line := element (c);
				--begin
					--if line.start_point = corner then
						--d1 := direction (line);

						--if d1 in type_direction then
							--if d1 < d2 then
								--d2 := d1;

								--selected_line := line;
								--line_found := true;
							--end if;
						--end if;
					--end if;
				--end query_line;
				
				--procedure query_arc (c : in pac_polygon_arcs.cursor) is
					--arc : constant type_polygon_arc := element (c);
				--begin
					--null;
				--end query_arc;

					
			--begin
				--segments.lines.iterate (query_line'access);
				--segments.arcs.iterate (query_arc'access);

				--return result;
			--end get_lowest_right_segment;
			
			--segments_on_start_point : type_polygon_segments;

			
			
		--begin -- reorder_segments
			--case lower_left_corner.status is
				--when REAL =>
					--polygon_start_point := lower_left_corner.point;
					
				--when VIRTUAL =>

					----set (
						----axis	=> Y,
						----point	=> polygon_start_point,
						----value	=> lower_left_corner.point.y);

					----set (
						----axis	=> X,
						----point	=> polygon_start_point,
						----value	=> get_smallest_x (polygon_start_point.y));

					--polygon_start_point := get_nearest_corner_point (polygon, lower_left_corner.point);
			--end case;

			--segments_on_start_point := get_segments_on_corner_point (polygon, polygon_start_point);
			
		--end reorder_segments;

		
		function to_string (scale : in type_polygon_scale) return string is begin
			return type_polygon_scale'image (scale);
		end to_string;
	
		function to_scale (scale : in string) return type_polygon_scale is begin
			return type_polygon_scale'value (scale);
		end to_scale;
		
		procedure offset_polygon (
			polygon		: in out type_polygon_base;
			offset		: in type_offset) 
		is
			use pac_polygon_segments;

			function scale_point (point	: in type_point) return type_point is
				x_new : type_distance := x (point) * offset.scale;
				y_new : type_distance := y (point) * offset.scale;
			begin
				return type_point (set (x_new, y_new));
			end scale_point;

			procedure offset_segment (c : in pac_polygon_segments.cursor) is
				procedure do_line (s : in out type_polygon_segment) is begin 
					case offset.style is
						when BY_DISTANCE => null; -- CS
						
						when BY_SCALE => null;
							s.segment_line.start_point	:= scale_point (s.segment_line.start_point);
							s.segment_line.end_point	:= scale_point (s.segment_line.end_point);

					end case;
				end;
				
				procedure do_arc (s : in out type_polygon_segment) is begin
					null; -- CS
				end;

			begin -- offset_segment
				case element (c).shape is
					
					when LINE =>
						update_element (
							container	=> polygon.contours.segments,
							position	=> c,
							process		=> do_line'access);

					when ARC =>
						update_element (
							container	=> polygon.contours.segments,
							position	=> c,
							process		=> do_arc'access);

				end case;
			end offset_segment;

			
		begin -- offset_polygon

			if polygon.contours.circular then

				-- scale the single circle that forms the polygon:
				-- CS change radius of  polygon.contours.circle
				null;
			else
				-- move lines and arcs:
				polygon.contours.segments.iterate (offset_segment'access);
			end if;
			
		end offset_polygon;
		
		
-- 		function to_corner_easing (easing : in string) return type_corner_easing is begin
-- 			return type_corner_easing'value (easing);
-- 		end;
-- 
-- 		function to_string (easing : in type_corner_easing) return string is begin
-- 			return to_lower (type_corner_easing'image (easing));
-- 		end to_string;


-- 		procedure move (
-- 			polygon : in out type_polygon_base;
-- 			offset	: in type_point) is
-- 		begin
-- 			-- CS move segments of polygon
-- 			null;
-- 		end;


		
		function "<" (left, right : in type_probe_line_intersection)
			return boolean
		is
			result : boolean := false;
		begin
			if left.x_position < right.x_position then
				result := true;
			else
				result := false;
			end if;

			-- CS compare angles ?
			
			return result;
		end "<";
		

		
		function subtract_180_if_greater_90 (
			angle : in type_rotation)
			return type_rotation
		is begin
			if angle > 90.0 then
				return abs (angle - 180.0);
			else
				return angle;
			end if;
		end subtract_180_if_greater_90;


		
		function to_string (
			i : in type_inside_polygon_query_result)
			return string
		is
			use ada.strings.unbounded;
			use pac_probe_line_intersections;

			result : unbounded_string;
			
			procedure query_intersection (c : pac_probe_line_intersections.cursor) is begin
				result := result & to_string (element (c).x_position) 
						  & "/" & trim (to_string (element (c).angle), left);
			end query_intersection;

		begin
			case i.status is
				when OUTSIDE =>
					result := to_unbounded_string ("Point" 
						& to_string (i.start) 
						& " is OUTSIDE of polygon. ");

				when INSIDE =>
					result := to_unbounded_string ("Point" 
						& to_string (i.start)
						& " is INSIDE polygon. ");
			end case;

			if is_empty (i.intersections) then
				result := result & "no intersections";
			else
				result := result & "intersection(s) x/angle:";
			end if;
			
			iterate (i.intersections, query_intersection'access);
			
			return to_string (result);
		end to_string;


		
		function in_polygon_status (
			polygon		: in type_polygon_base;	
			point		: in type_point)
			return type_inside_polygon_query_result 
		is
			-- This function bases on the algorithm published at
			-- <http://www.alienryderflex.com/polygon//>
			-- The algorithm has further been extended to detect intersections
			-- with arcs and even circles.
			
			-- The approach to detect whether the given point lies inside or outside 
			-- the polygon area is as follows:
			-- 1. Build a probe line (starting at point) that runs at zero degrees
			--    to the right. The probe line divides the area in two: an upper half and a
			--    lower half. Special situations arise if objects start or end exactly at
			--    the probe line.
			-- 2. The number of intersections then tells us:
			--    - odd -> point is inside the polygon area
			--    - zero or even -> point is outside the polygon area
			
			result : type_inside_polygon_query_result := (start => point, others => <>);

			line_pre : constant type_line := (
					start_point	=> point,
					end_point	=> type_point (set (X (point) + 1.0, Y (point))));
			
			probe_line : constant type_line_vector := to_line_vector (line_pre);

			-- For segments that end or start exactly on the Y value of the probe line
			-- we define a threshold:
			y_threshold : constant type_distance := Y (point);
			
			-- This is the variable for the number of intersections detected.
			-- From this number we will later deduce the position of the given point,
			-- means whether it is inside or outside the polygon:
			it : count_type := 0;

			use pac_probe_line_intersections;
			
			-- This procedure collects the intersection in the return value.
			procedure collect_intersection (
				intersection: in type_intersection; -- incl. point and angle
				curvature	: in type_curvature := STRAIGHT;
				center		: in type_point := origin;
				radius		: in type_distance_positive := zero)
			is begin	
				case curvature is
					when STRAIGHT =>
						
						append (result.intersections, (
							x_position	=> X (to_point (intersection.point)),
							angle		=> intersection.angle,
							curvature	=> STRAIGHT
							));

					when CONVEX =>

						append (result.intersections, (
							x_position	=> X (to_point (intersection.point)),
							angle		=> intersection.angle,
							curvature	=> CONVEX,
							center		=> center,
							radius		=> radius
							));

					when CONCAVE =>

						append (result.intersections, (
							x_position	=> X (to_point (intersection.point)),
							angle		=> intersection.angle,
							curvature	=> CONCAVE,
							center		=> center,
							radius		=> radius
							));
				end case;
			end collect_intersection;
			
			procedure query_line (l : in type_line) is 
				-- Find out whether there is an intersection of the probe line
				-- and the candidate line of the polygon.
				i : constant type_intersection_of_two_lines := get_intersection (probe_line, l);				
			begin
				if i.status = EXISTS then

					-- If the candidate line segment crosses the y_threshold then 
					-- count the intersection:
					if crosses_threshold (l, y_threshold) then
						
						-- Add the intersection to the result:
						collect_intersection (i.intersection);
					end if;
				end if;				
			end query_line;

			procedure query_arc (a : in type_arc) is

				-- the radius of the arc:
				radius : constant type_distance_positive := radius_start (a);
				
				-- Find out whether there is an intersection of the probe line
				-- and the candidate arc of the polygon.
				i : constant type_intersection_of_line_and_circle := 
					get_intersection (probe_line, a);

				-- In case we get two intersections (which speaks for a secant)
				-- then they need to be ordered according to their distance to
				-- the start point of the probe line (starts at given point);
				ordered_intersections : type_ordered_line_circle_intersections;
				
				procedure count_two is begin
					-- Add the two intersections to the result:
					collect_intersection (
						intersection=> ordered_intersections.entry_point,	
						curvature	=> CONVEX, -- entry point is always convex
						center		=> a.center,
						radius		=> radius);

					collect_intersection (
						intersection=> ordered_intersections.exit_point,	
						curvature	=> CONCAVE, -- exit point is always concave
						center		=> a.center,
						radius		=> radius);

				end count_two;
				
			begin -- query_arc		
				case i.status is
					when NONE_EXIST => null;
					
					when ONE_EXISTS =>
						case i.tangent_status is
							when TANGENT => null; -- not counted
							
							when SECANT =>
								if crosses_threshold (a, y_threshold) then
									-- The line intersects the arc at one point.
									-- Start and end point of the arc are opposide 
									-- of each other with the probe line betweeen them:

									collect_intersection (
										intersection	=> i.intersection,	

										-- If there is only one intersection, deduce
										-- the curvature at the point of intersection:
										curvature		=> get_curvature (a), -- depends on CW/CCW
										
										center			=> a.center,
										radius			=> radius);
									
								end if;
						end case;

					when TWO_EXIST =>
						-- Order the intersections by their distance to the start point
						-- of the probe line:
						ordered_intersections := order_intersections (
							start_point		=> point,
							intersections	=> i);
						
						if Y (a.start_point) /= y_threshold then
							-- Since we have TWO intersections, the end point of the arc
							-- must be in the same half as the start point of the arc.
							-- The arc crosses the threshold line twice:
							count_two;
							
						else
							-- Special case: Start or end point of arc lies exactly
							-- at the probe line.
							
							-- If start and end point of the candidate arc is ABOVE-OR-ON the 
							-- threshold then we consider the arc to be threshold-crossing.
							-- The remaining question is: How often does the arc intersect
							-- the probe line ?

							-- If start point at probe line:
							if Y (a.start_point) = y_threshold then

								-- If the arc starts ON the probe line and ends ABOVE
								-- the probe line, then it runs first downwards through the lower half,
								-- goes up, crosses the threshold at point P and ends somewhere 
								-- in the upper half:
								if Y (a.end_point) > y_threshold then

									-- Count the point P as intersection:
									case a.direction is
										when CCW => 
											collect_intersection (
												intersection=> ordered_intersections.exit_point,	
												curvature	=> CONCAVE, -- exit point is always concave
												center		=> a.center,
												radius		=> radius);

										when CW => 
											collect_intersection (
												intersection=> ordered_intersections.entry_point,	
												curvature	=> CONVEX, -- entry point is always convex
												center		=> a.center,
												radius		=> radius);
									end case;
											
								-- If the arc starts ON the probe line and ends BELOW
								-- the probe line, then it runs first upwards through the upper half,
								-- goes down, crosses the threshold at point P1 and ends somewhere 
								-- in the lower half.
								elsif Y (a.end_point) < y_threshold then

									-- Count the start point and point P1 as intersections:
									count_two;
								end if;

								
							-- If end point at probe line:
							elsif Y (a.end_point) = y_threshold then

								-- If the arc starts somewhere in the upper half, then it runs
								-- down, crosses the threshold at point P, runs through the 
								-- lower half, goes up and ends ON the threshold line:
								if Y (a.start_point) > y_threshold then

									-- Count the point P as intersection:
									case a.direction is
										when CCW => 
											collect_intersection (
												intersection=> ordered_intersections.entry_point,	
												curvature	=> CONVEX, -- entry point is always convex
												center		=> a.center,
												radius		=> radius);

										when CW => 
											collect_intersection (
												intersection=> ordered_intersections.exit_point,	
												curvature	=> CONCAVE, -- exit point is always concave
												center		=> a.center,
												radius		=> radius);
									end case;
								

								-- If the arc starts somewhere in the lower half, then it runs
								-- up, crosses the threshold at point P, runs through the
								-- upper half, goes down and ends ON the threshold line:
								elsif Y (a.start_point) < y_threshold then

									-- Count the end point and point P as intersections:
									count_two;
								end if;
								
							end if;
						end if;
					
				end case;
			end query_arc;

			use pac_polygon_segments;
			
			procedure query_segment (c : in pac_polygon_segments.cursor) is
			begin
				case element (c).shape is					
					when LINE	=> query_line (element (c).segment_line);
					when ARC	=> query_arc (element (c).segment_arc);
				end case;
			end query_segment;

			
			procedure query_circle (c : in type_circle) is
				-- Find out whether there is an intersection of the probe line
				-- and the candidate circle of the polygon.
				i : constant type_intersection_of_line_and_circle := 
					get_intersection (probe_line, c);

				-- In case we get two intersections (which speaks for a secant)
				-- then they need to be ordered according to their distance to
				-- the start point of the probe line (starts at given point);
				ordered_intersections : type_ordered_line_circle_intersections;

			begin				
				case i.status is
					when NONE_EXIST | ONE_EXISTS => null;
						-- NOTE: If the probe line is a tangent to the
						-- circle, then we threat this NOT as intersection.
					
					when TWO_EXIST =>
						-- The probe line intersects the circle at two points:

						-- Order the intersections by their distance to the start point:
						ordered_intersections := order_intersections (
							start_point		=> point,
							intersections	=> i);

						
						-- Add the intersections to the result:
						collect_intersection (
							intersection=> ordered_intersections.entry_point,	
							curvature	=> CONVEX, -- entry point is always convex
							center		=> c.center,
							radius		=> c.radius);

						collect_intersection (
							intersection=> ordered_intersections.exit_point,	
							curvature	=> CONCAVE, -- exit point is always concave
							center		=> c.center,
							radius		=> c.radius);

				end case;
			end query_circle;

			procedure sort_x_values is
				package pac_sort_x_values is new pac_probe_line_intersections.generic_sorting;
				use pac_sort_x_values;
			begin
				sort (result.intersections);
			end sort_x_values;
			
		begin -- in_polygon_status

			if polygon.contours.circular then
				query_circle (polygon.contours.circle);
			else
				polygon.contours.segments.iterate (query_segment'access);
			end if;

			
			-- The x-values are not sorted yet. We need them sorted with the
			-- smallest x first:
			sort_x_values;

			-- get the total number of intersections
			it := pac_probe_line_intersections.length (result.intersections);
			
			-- If the total number of intersections is an odd number, then the given point
			-- is inside the polygon.
			-- If the total is even, then the point is outside the polygon.
			if (it rem 2) = 1 then
				result.status := INSIDE;
			else 
				result.status := OUTSIDE;
			end if;
			
			return result;
		end in_polygon_status;

		function intersections_found (
			i : in type_inside_polygon_query_result)
			return boolean
		is
			use pac_probe_line_intersections;
		begin
			if length (i.intersections) = 0 then -- no intersections with polygon
				return false;
			else
				return true; -- at least one intersection found
			end if;
		end intersections_found;

		function get_first_intersection (
			i : in type_inside_polygon_query_result)
			return type_probe_line_intersection
		is
			use pac_probe_line_intersections;
		begin
			return element (i.intersections.first);
		end get_first_intersection;
		
		
		function get_lower_left_corner (polygon	: in type_polygon_base)
			return type_lower_left_corner
		is
			result : type_lower_left_corner;

			boundaries : constant type_boundaries := get_boundaries (polygon, zero);
			
		begin
			-- compose the lower left corner point:
			result.point := type_point (set (boundaries.smallest_x, boundaries.smallest_y));

			-- figure out whether the point is real or virtual:
			case in_polygon_status (polygon, result.point).status is
				when INSIDE =>
					result.status := REAL;
					
				when OUTSIDE => -- or on edge of polygon
					result.status := VIRTUAL;
			end case;
			
			return result;
		end get_lower_left_corner;
		
		
	end generic_pac_shapes;

	
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
