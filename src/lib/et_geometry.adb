------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017-2020 Mario Blunk, Blunk electronic            --
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

with ada.numerics.generic_elementary_functions;

with et_string_processing;		use et_string_processing;
with et_exceptions;				use et_exceptions;

package body et_geometry is

	function to_string (axis : in type_axis) return string is begin
		return to_lower (type_axis'image (axis));
	end;

	function to_axis (axis : in string) return type_axis is begin
		return type_axis'value (axis);
	end;

	

	function to_notches (notches : in string) return type_grid_notches is begin
		return type_grid_notches'value (notches);
	end;

	function to_string (notches : in type_grid_notches) return string is begin
		return type_grid_notches'image (notches);
	end;



	function to_shape (shape : in string) return type_shape is begin
		return type_shape'value (shape);
	end;

	function to_shape (shape : in type_shape) return string is begin
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


	
	package body generic_pac_geometry is

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

			package functions is new ada.numerics.generic_elementary_functions (float);
			use functions;
		begin
			-- NOTE: If x and y are zero then the arctan operation is not possible. 
			-- In this case we assume the resuling angle is zero.
			if x = 0.0 and y = 0.0 then
				return zero_rotation;
			else
				return type_rotation (arctan (y, x, float (units_per_cycle)));
			end if;
		end rotation;
		
		function to_string (boundaries : in type_boundaries) return string is begin
			return "boundaries: smallest x" & to_string (boundaries.smallest_x) 
				& " greatest x" & to_string (boundaries.greatest_x)
				& " smallest y" & to_string (boundaries.smallest_y)
				& " greatest y" & to_string (boundaries.greatest_y);
		end;
		
		-- Adds two boundaries.
		procedure add (
			boundaries_one : in out type_boundaries;
			boundaries_two : in type_boundaries) is
		begin
-- 			if boundaries_two.smallest_x < boundaries_one.smallest_x , smallest_y : type_distance := type_distance'last;
-- 			greatest_x, greatest_y : type_distance := type_distance'first;
			null; -- CS
		end; 

		
		function boundaries (point_one, point_two : in type_point) return type_boundaries is
		-- Calculates the boundaries of the given points.
			result : type_boundaries;
		begin
			-- X axis
			if point_one.x = point_two.x then -- both points on a vertical line

				-- In order to ensure a minimum width of the boundaries:
				result.smallest_x := point_one.x - type_distance'small;
				result.greatest_x := point_one.x + type_distance'small;
				
			elsif point_one.x < point_two.x then
				
				result.smallest_x := point_one.x;
				result.greatest_x := point_two.x;
			else
				result.smallest_x := point_two.x;
				result.greatest_x := point_one.x;
			end if;

			-- Y axis
			if point_one.y = point_two.y then -- both points on a horizontal line

				-- In order to ensure a minimum height of the boundaries:
				result.smallest_y := point_one.y - type_distance'small;
				result.greatest_y := point_one.y + type_distance'small;
			
			elsif point_one.y < point_two.y then
				
				result.smallest_y := point_one.y;
				result.greatest_y := point_two.y;
			else
				result.smallest_y := point_two.y;
				result.greatest_y := point_one.y;
			end if;

			return result;
		end boundaries;

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
			return type_point'class is 

			package functions is new ada.numerics.generic_elementary_functions (float);
			-- CS could be useful to use a constrained float type			
			use functions;
			
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
		-- Returns the relative distance of point_two from point_one.	
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
			package functions is new ada.numerics.generic_elementary_functions (float);
			
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
				
				distance := type_distance (functions.sqrt ((delta_x ** 2) + (delta_y ** 2)));
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


		
		function distance_polar (point_one, point_two : in type_point) return type_distance_polar is
			result : type_distance_polar;

			package functions is new ada.numerics.generic_elementary_functions (float);
			use functions;
			delta_x, delta_y : float := 0.0;
			
		begin
			result.absolute := distance_total (point_one, point_two);

			-- NOTE: If the total distance between the points is zero then
			-- the arctan operation is not possible. In this case we assume
			-- the resuling angle is zero.
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
			rotation	: in type_rotation) is

			package functions_distance is new ada.numerics.generic_elementary_functions (float);
			use functions_distance;
			
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

			package functions_distance is new ada.numerics.generic_elementary_functions (float);
			use functions_distance;
			
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
			package functions is new ada.numerics.generic_elementary_functions (float);
			use functions;
		begin
			return type_distance_positive (
				sqrt (
					float (vector.x) ** 2 + 
					float (vector.y) ** 2 +
					float (vector.z) ** 2)
				);
		end absolute;


		function multiply (
			v	: in type_vector;
			a	: in type_distance)
			return type_vector
		is begin
			return (
				x => a * v.x,
				y => a * v.y,
				z => a * v.z);
		end multiply;
				
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

			package pac_functions is new ada.numerics.generic_elementary_functions (float);
			use pac_functions;
		begin
			-- NOTE: If dx and dy are zero then the arctan operation is not possible. 
			-- In this case we assume the resuling angle is zero.
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

					I := add (S2, multiply (R2, lambda_2));
				else
					a := S2.y;
					b := type_distance (S1.x * R2.y) / R2.x;
					c := type_distance (S2.x * R2.y) / R2.x;
					d := S1.y;
					e := R1.y;
					f := type_distance (R1.x * R2.y) / R2.x;
					g := 1.0 / (e - f);

					lambda_1 := (a + b - c - d) * g;

					I := add (S1, multiply (R1, lambda_1));
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
		
		function boundaries (line : in type_line) return type_boundaries is begin
			return boundaries (line.start_point, line.end_point);
		end boundaries;
		
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

			package functions is new ada.numerics.generic_elementary_functions (float);
			-- CS could be useful to use a constrained float type
			use functions;
						
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
			-- In this case we assume the resuling angle is zero.
			
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
			
			-- direction is not changed:
			result.direction := arc.direction;
			
			return result;
		end to_arc_angles;
		
		function boundaries (arc : in type_arc) return type_boundaries is
		-- This function calculates the boundaries of an arc.
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
			
			-- calculate the boundaries of start and end point
			result := boundaries (arc_tmp.start_point, arc_tmp.end_point);

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
			
			return result;
		end boundaries;
		
		function on_arc (
		-- Returns true if the given point sits on the given arc.
		-- The optional parameter accuracy may be used to specifiy the range at
		-- which the point is regarded as sitting on the arc.
			point		: in type_point;
			arc			: in type_arc;
			accuracy	: in type_catch_zone := zero)
			return boolean is
		begin
			return false; 
			-- CS math required
		end;
		
		function arc_end_point (
		-- Computes the end point of an arc.
			center		: in type_point;
			start_point	: in type_point;	
			angle 		: in type_rotation) -- unit is degrees
			return type_point'class is

			package functions is new ada.numerics.generic_elementary_functions (float);
			-- CS could be useful to use a constrained float type
			use functions;
						
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
			-- In this case we assume the resuling angle is zero.
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
		
		function boundaries (circle : in type_circle) return type_boundaries is
			result : type_boundaries;
		begin
			-- X axis
			result.smallest_x := circle.center.x - circle.radius;
			result.greatest_x := circle.center.x + circle.radius;

			-- Y axis
			result.smallest_y := circle.center.y - circle.radius;
			result.greatest_y := circle.center.y + circle.radius;
			
			return result;
		end boundaries;
		
		function on_circle (
		-- Returns true if the given point sits on the given circle circumfence.
		-- The optional parameter accuracy may be used to specifiy the range at
		-- which the point is regarded as sitting on the circle.
			point		: in type_point;
			circle		: in type_circle;
			accuracy	: in type_catch_zone := zero)
			return boolean is
		begin
			return false; 
			-- CS math required
		end;
		
		function to_string (line : in type_line) return string is
		-- Returns the start and end point of the given line as string.
		begin
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

		function boundaries (polygon : in type_polygon_base) return type_boundaries is
			b : type_boundaries; -- to be returned

			use pac_polygon_lines;
			use pac_polygon_arcs;
			use pac_polygon_circles;
			
			procedure query_line (c : in pac_polygon_lines.cursor) is begin
				union (b, boundaries (element (c)));
			end query_line;

			procedure query_arc (c : in pac_polygon_arcs.cursor) is begin
				union (b, boundaries (element (c)));
			end query_arc;

			procedure query_circle (c : in pac_polygon_circles.cursor) is begin
				union (b, boundaries (element (c)));
			end query_circle;
			
		begin
			iterate (polygon.segments.lines, query_line'access);
			iterate (polygon.segments.arcs, query_arc'access);
			iterate (polygon.segments.circles, query_circle'access);

			return b;
		end boundaries;

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

			use pac_polygon_lines;
			use pac_polygon_arcs;
			use pac_polygon_circles;

			-- The functions get_line, get_arc and get_circle search for a polygon segment (by its id)
			-- and return a cursor to the segment if it exists. Otherwise they return no_element:
			
			function get_line (segment : in type_polygon_segment_id) return pac_polygon_lines.cursor is
				c : pac_polygon_lines.cursor := polygon.segments.lines.first;
				found : boolean := false;

				procedure query_line (l : in type_polygon_line) is begin
					if l.id = segment then
						found := true;
					end if;
				end query_line;

			begin -- get_line
				while c /= pac_polygon_lines.no_element loop
					query_element (c, query_line'access);

					if found = true then exit; end if;
					
					next (c);
				end loop;

				return c; -- should be no_element if not found. points to the segment if found.
			end get_line;

			function get_arc (segment : in type_polygon_segment_id) return pac_polygon_arcs.cursor is 
				c : pac_polygon_arcs.cursor := polygon.segments.arcs.first;
				found : boolean := false;

				procedure query_arc (l : in type_polygon_arc) is begin
					if l.id = segment then
						found := true;
					end if;
				end query_arc;
				
			begin -- get_arc
				while c /= pac_polygon_arcs.no_element loop
					query_element (c, query_arc'access);

					if found = true then exit; end if;
					
					next (c);
				end loop;

				return c; -- should be no_element if not found. points to the segment if found.
			end get_arc;
			
			function get_circle (segment : in type_polygon_segment_id) return pac_polygon_circles.cursor is 
				c : pac_polygon_circles.cursor := polygon.segments.circles.first;
				found : boolean := false;

				procedure query_circle (l : in type_polygon_circle) is begin
					if l.id = segment then
						found := true;
					end if;
				end query_circle;
				
			begin -- get_circle
				while c /= pac_polygon_circles.no_element loop
					query_element (c, query_circle'access);

					if found = true then exit; end if;
					
					next (c);
				end loop;

				return c; -- should be no_element if not found. points to the segment if found.
			end get_circle;

			-- Here the result of get_line, get_arc and get_circle is stored temporarily:
			cl : pac_polygon_lines.cursor;
			ca : pac_polygon_arcs.cursor;
			cc : pac_polygon_circles.cursor;

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
	
		begin
			-- Iterate segments of given polygon. For each iteration s indicates the
			-- segment to be checked. It can be among lines (most likely), among arcs (less likely)
			-- and among circles (least likely). The functions get_line, get_arc and get_circle
			-- return a cursor to the segment if it is among lines, arcs or circles.
			-- Otherwise get_line, get_arc or get_circle return no_element.
			for s in type_polygon_segment_id'first .. polygon.segments_total loop

				-- Search the segment among the lines:
				cl := get_line (s);
				if cl /= pac_polygon_lines.no_element then

					set_start_point (element (cl).start_point);
					last_end_point := element (cl).end_point;

				-- If segment not found among lines, search among arcs:
				else 
					ca := get_arc (s);
					if ca /= pac_polygon_arcs.no_element then
						
						set_start_point (element (ca).start_point);
						last_end_point := element (ca).end_point;
						
					-- If segment not found among arcs, search among circles:
					else
						cc := get_circle (s);
						if cc /= pac_polygon_circles.no_element then

							set_start_point (element (cc).center);
							last_end_point := element (cc).center;
							
						else
							-- If segment is not among circles, we have a problem:
							raise constraint_error; -- CS should never happen.
						end if;
						
					end if;
				end if;

			end loop;

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
		-- Moves a polygon by the given offset. 
			polygon	: in out type_polygon_base;
			offset	: in type_point) is

			use pac_polygon_lines;
			use pac_polygon_arcs;
			use pac_polygon_circles;

			cl : pac_polygon_lines.cursor := polygon.segments.lines.first;
			ca : pac_polygon_arcs.cursor := polygon.segments.arcs.first;
			cc : pac_polygon_circles.cursor := polygon.segments.circles.first;

			procedure move_line (l : in out type_polygon_line) is begin
				move_by (l, offset);
			end move_line;

			procedure move_arc (a : in out type_polygon_arc) is begin
				move_by (a, offset);
			end move_arc;

			procedure move_circle (c : in out type_polygon_circle) is begin
				move_by (c, offset);
			end move_circle;
			
		begin -- move_by
			while cl /= pac_polygon_lines.no_element loop
				update_element (
					container	=> polygon.segments.lines,
					position	=> cl,
					process		=> move_line'access);

				next (cl);
			end loop;

			while ca /= pac_polygon_arcs.no_element loop
				update_element (
					container	=> polygon.segments.arcs,
					position	=> ca,
					process		=> move_arc'access);

				next (ca);
			end loop;

			while cc /= pac_polygon_circles.no_element loop
				update_element (
					container	=> polygon.segments.circles,
					position	=> cc,
					process		=> move_circle'access);

				next (cc);
			end loop;
		end move_by;

		procedure mirror (
		-- Mirrors a polygon along the given axis.
			polygon	: in out type_polygon_base;
			axis	: in type_axis_2d) is
			
			use pac_polygon_lines;
			use pac_polygon_arcs;
			use pac_polygon_circles;

			cl : pac_polygon_lines.cursor := polygon.segments.lines.first;
			ca : pac_polygon_arcs.cursor := polygon.segments.arcs.first;
			cc : pac_polygon_circles.cursor := polygon.segments.circles.first;

			procedure mirror_line (l : in out type_polygon_line) is begin
				mirror (l, axis);
			end mirror_line;

			procedure mirror_arc (a : in out type_polygon_arc) is begin
				mirror (a, axis);
			end mirror_arc;

			procedure mirror_circle (c : in out type_polygon_circle) is begin
				mirror (c, axis);
			end mirror_circle;
			
		begin -- mirror
			while cl /= pac_polygon_lines.no_element loop
				update_element (
					container	=> polygon.segments.lines,
					position	=> cl,
					process		=> mirror_line'access);

				next (cl);
			end loop;

			while ca /= pac_polygon_arcs.no_element loop
				update_element (
					container	=> polygon.segments.arcs,
					position	=> ca,
					process		=> mirror_arc'access);

				next (ca);
			end loop;

			while cc /= pac_polygon_circles.no_element loop
				update_element (
					container	=> polygon.segments.circles,
					position	=> cc,
					process		=> mirror_circle'access);

				next (cc);
			end loop;
		end mirror;

		procedure rotate_by (
		-- Rotates a polygon about the origin by the given rotation.
			polygon		: in out type_polygon_base;
			rotation	: in type_rotation) is

			use pac_polygon_lines;
			use pac_polygon_arcs;
			use pac_polygon_circles;

			cl : pac_polygon_lines.cursor := polygon.segments.lines.first;
			ca : pac_polygon_arcs.cursor := polygon.segments.arcs.first;
			cc : pac_polygon_circles.cursor := polygon.segments.circles.first;

			procedure rotate_line (l : in out type_polygon_line) is begin
				rotate_by (l, rotation);
			end rotate_line;

			procedure rotate_arc (a : in out type_polygon_arc) is begin
				rotate_by (a, rotation);
			end rotate_arc;

			procedure rotate_circle (c : in out type_polygon_circle) is begin
				rotate_by (c, rotation);
			end rotate_circle;
			
		begin -- mirror
			while cl /= pac_polygon_lines.no_element loop
				update_element (
					container	=> polygon.segments.lines,
					position	=> cl,
					process		=> rotate_line'access);

				next (cl);
			end loop;

			while ca /= pac_polygon_arcs.no_element loop
				update_element (
					container	=> polygon.segments.arcs,
					position	=> ca,
					process		=> rotate_arc'access);

				next (ca);
			end loop;

			while cc /= pac_polygon_circles.no_element loop
				update_element (
					container	=> polygon.segments.circles,
					position	=> cc,
					process		=> rotate_circle'access);

				next (cc);
			end loop;
		end rotate_by;

		function to_string (scale : in type_polygon_scale) return string is begin
			return type_polygon_scale'image (scale);
		end to_string;
	
		function to_scale (scale : in string) return type_polygon_scale is begin
			return type_polygon_scale'value (scale);
		end to_scale;
		
		procedure offset_polygon (
			polygon		: in out type_polygon_base;
			offset		: in type_offset) is

			use pac_polygon_lines;
			use pac_polygon_arcs;
			use pac_polygon_circles;

			-- The functions get_line, get_arc and get_circle search for a polygon segment (by its id)
			-- and return a cursor to the segment if it exists. Otherwise they return no_element:
			
			function get_line (segment : in type_polygon_segment_id) return pac_polygon_lines.cursor is 
				c : pac_polygon_lines.cursor := polygon.segments.lines.first;
				found : boolean := false;

				procedure query_line (l : in type_polygon_line) is begin
					if l.id = segment then
						found := true;
					end if;
				end query_line;
				
			begin -- get_line
				while c /= pac_polygon_lines.no_element loop
					query_element (c, query_line'access);

					if found = true then exit; end if;
					
					next (c);
				end loop;

				return c; -- should be no_element if not found. points to the segment if found.
			end get_line;
			
			function get_arc (segment : in type_polygon_segment_id) return pac_polygon_arcs.cursor is 
				c : pac_polygon_arcs.cursor := polygon.segments.arcs.first;
				found : boolean := false;

				procedure query_arc (l : in type_polygon_arc) is begin
					if l.id = segment then
						found := true;
					end if;
				end query_arc;
				
			begin -- get_arc
				while c /= pac_polygon_arcs.no_element loop
					query_element (c, query_arc'access);

					if found = true then exit; end if;
					
					next (c);
				end loop;

				return c; -- should be no_element if not found. points to the segment if found.
			end get_arc;
			
			function get_circle (segment : in type_polygon_segment_id) return pac_polygon_circles.cursor is 
				c : pac_polygon_circles.cursor := polygon.segments.circles.first;
				found : boolean := false;

				procedure query_circle (l : in type_polygon_circle) is begin
					if l.id = segment then
						found := true;
					end if;
				end query_circle;
				
			begin -- get_circle
				while c /= pac_polygon_circles.no_element loop
					query_element (c, query_circle'access);

					if found = true then exit; end if;
					
					next (c);
				end loop;

				return c; -- should be no_element if not found. points to the segment if found.
			end get_circle;

			-- Here the result of get_line, get_arc and get_circle is stored temporarily:
			cl : pac_polygon_lines.cursor;
			ca : pac_polygon_arcs.cursor;
			cc : pac_polygon_circles.cursor;

			function scale_point (point	: in type_point) return type_point is
				x_new : type_distance := x (point) * offset.scale;
				y_new : type_distance := y (point) * offset.scale;
			begin
				return type_point (set (x_new, y_new));
			end scale_point;
			
			procedure move_line (l : in out type_polygon_line) is begin
				case offset.style is
					when BY_DISTANCE => null; -- CS
					
					when BY_SCALE => null;
						l.start_point	:= scale_point (l.start_point);
						l.end_point		:= scale_point (l.end_point);

				end case;
			end move_line;
			
		begin -- offset_polygon
			
			-- Iterate segments of given polygon. For each iteration s indicates the
			-- segment to be processed. It can be among lines (most likely), among arcs (less likely)
			-- and among circles (least likely). The functions get_line, get_arc and get_circle
			-- return a cursor to the segment if it is among lines, arcs or circles.
			-- Otherwise get_line, get_arc or get_circle return no_element.
			for s in type_polygon_segment_id'first .. polygon.segments_total loop

				-- Search the segment among the lines:
				cl := get_line (s);
				if cl /= pac_polygon_lines.no_element then

					update_element (polygon.segments.lines, cl, move_line'access);

				-- If segment not found among lines, search among arcs:
				else 
					ca := get_arc (s);
					if ca /= pac_polygon_arcs.no_element then
						
						null;

					-- If segment not found among arcs, search among circles:
					else
						cc := get_circle (s);
						if cc /= pac_polygon_circles.no_element then

							null;
						else
							-- If segment is not among circles, we have a problem:
							raise constraint_error; -- CS should never happen. log message !
						end if;
						
					end if;
				end if;
				

			end loop;
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
		
	end generic_pac_shapes;

	
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
