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
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.numerics.generic_elementary_functions;

package body et_geometry is

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

	
	package body geometry_operations_2d is
		
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

		function rotation (point : in type_point) return type_rotation is
			x : constant float := float (point.x);
			y : constant float := float (point.y);

			package functions is new ada.numerics.generic_elementary_functions (float);
			use functions;
		begin
			return type_rotation (arctan (y, x, float (units_per_cycle)));
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
				rotate (corners (c), rotation);
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

		procedure move (
		-- Moves a point by the given offset.
			point	: in out type_point'class;
			offset	: in type_point) is
		begin
			point.x := point.x + offset.x;
			point.y := point.y + offset.y;
		end move;

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
			position.rotation := position.rotation + offset;
		end;
		
		procedure rotate (
		-- Rotates the given point BY the given angle around the origin.
		-- Changes point.x and point.y only.							 
			point		: in out type_point'class;
			rotation	: in type_rotation) is

			-- CS probably way to much stuff here. simplify. instead of type_float_distance use just float ?
			
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
				angle_out := angle_out + type_float_angle (rotation);

				-- compute new x   -- (cos angle_out) * distance_to_origin
				scratch := cos (type_float_distance (angle_out), type_float_distance (units_per_cycle));
				set (axis => X, point => point, value => type_distance (scratch * distance_to_origin));

				-- compute new y   -- (sin angle_out) * distance_to_origin
				scratch := sin (type_float_distance (angle_out), type_float_distance (units_per_cycle));
				set (axis => Y, point => point, value => type_distance (scratch * distance_to_origin));
		
			end if; -- if angle not zero
			
		end rotate;

		procedure rotate_to (
			point		: in out type_point'class;
			rotation	: in type_rotation) is

			-- CS probably way to much stuff here. simplify. use code of procedure rotate_by (see above).
			
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

			-- The new angle is the given rotation:
			angle_out := type_float_angle (rotation);

			-- compute new x   -- (cos angle_out) * distance_to_origin
			scratch := cos (type_float_distance (angle_out), type_float_distance (units_per_cycle));
			set (axis => X, point => point, value => type_distance (scratch * distance_to_origin));

			-- compute new y   -- (sin angle_out) * distance_to_origin
			scratch := sin (type_float_distance (angle_out), type_float_distance (units_per_cycle));
			set (axis => Y, point => point, value => type_distance (scratch * distance_to_origin));
			
		end rotate_to;

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
		
	end geometry_operations_2d;


	
	package body shapes_2d is
		use geometry;

-- 		function to_string (fill_style : in type_fill_style) return string is begin
-- 			return to_lower (type_fill_style'image (fill_style));
-- 		end;
-- 
-- 		function to_fill_style (fill_style : in string) return type_fill_style is begin
-- 			return type_fill_style'value (fill_style);
-- 		end;

		function to_string (filled : in type_filled) return string is begin
			return to_lower (type_filled'image (filled));
		end to_string;

		function to_filled (filled : in string) return type_filled is begin
			return type_filled'value (filled);
		end to_filled;

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

		-- Rotates a line around the origin.
		procedure rotate (
			line		: in out type_line;
			rotation	: in type_rotation) 
		is begin
			rotate (line.start_point, rotation);
			rotate (line.end_point, rotation);
		end rotate;

		
		function boundaries (line : in type_line) return type_boundaries is begin
			return boundaries (line.start_point, line.end_point);
		end boundaries;
		
		function which_zone (
		-- Calculates the zone of the line where point is nearest.
			point	: in type_point'class;
			line	: in type_line'class) 
			return type_line_zone is

			zone : type_line_zone; -- to be returned
		
			line_length : type_distance;
			zone_border : type_distance;
			
		begin -- which_zone
			-- The greater distance from start to end point in X or Y determines 
			-- whether the line is handled like a horizontal or vertical drawn line.
			if distance (line.start_point, line.end_point, X) > distance (line.start_point, line.end_point, Y) then

				-- distance in X greater -> decision will be made along the X axis.
				-- The line will be handled like a horizontal drawn line.
				
				-- calculate the zone border. This depends on the line length in X direction.
				line_length := distance (line.start_point, line.end_point, X);
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

				-- calculate the zone border. This depends on the line length in X direction.
				line_length := distance (line.start_point, line.end_point, Y);
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
			
			return center;
		end which_zone;

		function distance_point_line (
		-- Computes the shortest distance (perpendicular) of a given point from the given line. If the point outside the
		-- range of the x coordinate, the corresponding flag in the return value is set.

		-- CS: provide range and accuracy via parameter
		-- CS: type_Y_axis_positive (upwards, downwards) may matter here. 

			point		: in type_point;
			line		: in type_line;
			line_range	: in type_line_range) 
			return type_distance_point_line is

			result : type_distance_point_line; -- to be returned
	
			type type_float is digits 11 range -100000000.0 .. 100000000.0; -- CS: probably way too much
			package functions is new ada.numerics.generic_elementary_functions (type_float);

			s : type_point := line.start_point;
			e : type_point := line.end_point;		

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
					
					if point = line.start_point then
						
						result.sits_on_start := true;
						return result;

					elsif point = line.end_point then
						
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
				when BETWEEN_END_POINTS | WITH_END_POINTS =>
					if delta_x < zero then 
						line_scratch := s;
						s := e;
						e := line_scratch;
					end if;
				when others => null;
			end case;

			-- Test range of point in regard of the x position.
			case line_range is
				when BETWEEN_END_POINTS =>
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

		function on_line (
		-- Returns true if the given point sits on the given line.
		-- The optional parameter accuracy may be used to specifiy the range at
		-- which the point is regarded as sitting on the line.
			point		: in type_point;
			line		: in type_line;
			accuracy	: in type_accuracy := zero)
			return boolean is
			distance : type_distance_point_line;
		begin
			distance := distance_point_line (point, line, BETWEEN_END_POINTS);

			if not distance.out_of_range and distance.distance <= accuracy then
				return true;
			else
				return false;
			end if;
		end;

		-- Rotates an arc around the origin.
		procedure rotate (
			arc			: in out type_arc;
			rotation	: in type_rotation) is
		begin
			rotate (arc.center, rotation);
			rotate (arc.start_point, rotation);
			rotate (arc.end_point, rotation);
		end;
		
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
			result.radius := distance (arc_tmp.center, arc_tmp.start_point);

			-- calculate the angles where the arc begins and ends:
			result.angle_start := to_degrees (arctan (
							y => float (arc_tmp.start_point.y),
							x => float (arc_tmp.start_point.x)));

			result.angle_end := to_degrees (arctan (
							y => float (arc_tmp.end_point.y),
							x => float (arc_tmp.end_point.x)));

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
			radius : type_distance_positive := distance (arc.center, arc.start_point);

			-- Calculate the quadrant where start and end point are in:
			q_start : type_quadrant := quadrant (arc_tmp.start_point);
			q_end   : type_quadrant := quadrant (arc_tmp.end_point);

			procedure set_sx is begin result.smallest_x := - radius; end;
			procedure set_gx is begin result.greatest_x :=   radius; end;
			procedure set_sy is begin result.smallest_y := - radius; end;
			procedure set_gy is begin result.greatest_y :=   radius; end;
			
		begin -- boundaries
			-- move arc_tmp so that its center is at 0/0
			move_to (arc_tmp, origin);

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
			accuracy	: in type_accuracy := zero)
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
			radius := float (distance (arc.center, arc.start_point));

			-- calculate the angle where the arc begins:
			angle_start := arctan (
							y => float (arc.start_point.y),
							x => float (arc.start_point.x));

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
			arc		: in out type_arc'class;
			offset	: in type_point) is
		begin
			move (point => arc.center,      offset => offset);
			move (point => arc.start_point, offset => offset);
			move (point => arc.end_point,   offset => offset);
		end move_by;

		procedure move_to (
		-- Moves an arc to the given position.
			arc			: in out type_arc'class;
			position	: in type_point) is

			-- compute the offset:
			offset : type_point := type_point (distance_relative (arc.center, position));
		begin
			-- move the center of the arc to the given position
			move_to (arc.center, position);

			-- move start and end point of the arc by the computed offset
			move (point => arc.start_point, offset => offset);
			move (point => arc.end_point,   offset => offset);
		end move_to;

		-- Rotates a circle around the origin.
		procedure rotate (
			circle		: in out type_circle;
			rotation	: in type_rotation) is
		begin
			rotate (circle.center, rotation);
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
			accuracy	: in type_accuracy := zero)
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
		
	end shapes_2d;

	
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
