------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET COORDINATES                          --
--                                                                          --
--                                 ET                                       --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;


with system.assertions;
with ada.exceptions;

with ada.numerics.generic_elementary_functions;
with et_string_processing;
with et_general;

package body et_coordinates is
	pragma assertion_policy (check);
	
	function to_distance (distance : in string) return type_distance_xy is begin
		return type_distance'value (distance);
	end to_distance;
	
	function to_string (distance : in type_distance) return string is
	-- Returns the given distance to a string.
	begin
		if distance < zero_distance then
			return latin_1.space & type_distance'image (distance);
		else
			return type_distance'image (distance);
		end if;
	end to_string;


	function to_string (angle : in type_angle) return string is
	-- Returns the the given angle as string.
		--preamble	: constant string := " angle ";
		preamble	: constant string := " ";
		suffix		: constant string := " deg";
	begin
		return (preamble & type_angle'image (angle) & suffix);
	end to_string;

	function to_angle (angle : in string) return type_angle is 
		use et_string_processing;
		r : type_angle;
	begin
		r := type_angle'value (angle);
		return r;

		exception 
			when constraint_error => 
				log (message_error & "Rotation " & angle & " outside range" & 
					 et_coordinates.to_string (-270) &
					 " .." & 
					 et_coordinates.to_string (270) &
					 " (must be an integer) !",
					 console => true
					);
				raise;
				
			when system.assertions.assert_failure =>
				log (message_error & "Rotation " & angle & " is not a multiple of" &
					 et_coordinates.to_string (rotation_delta) & " !",
					 console => true
					);
				raise;
				
			when others =>
				raise;
	end to_angle;

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

	function to_string (point : in type_point'class) return string is
	-- Returns the given point coordinates to a string.
		use et_string_processing;
	begin
		return position_preamble
		-- 	& trim (type_distance'image (point.x), left)
			& to_string (point.x)
			& latin_1.space & axis_separator & latin_1.space
		--	& trim (type_distance'image (point.y), left);
			& to_string (point.y);
	
	end to_string;

	function distance_x (point : in type_point) return type_distance_xy is
	-- Returns the x distance of point from the drawing origin.		
	-- CS remove and use function distance instead.
	begin
		--return et_math.round (point.x, accuracy_schematic);
		return point.x;
	end distance_x;

	function distance_y (point : in type_point) return type_distance_xy is
	-- Returns the y distance of point from the drawing origin.
	-- CS remove and use function distance instead.
	begin
		--return et_math.round (point.y, accuracy_schematic);
		return point.y;
	end distance_y;

	function distance (
	-- Returns the distance of the point in x or y from the origin.
		axis	: in type_axis;
		point	: in type_point'class)
		return type_distance_xy is
	begin
		case axis is
			when X => return point.x;
			when Y => return point.y;
		end case;
	end distance;
	
	procedure set_x (point : in out type_point; x : in type_distance_xy) is
	-- Assigns a point the given x position.
		use et_string_processing;
	begin
		point.x := x;
	end set_x;
	
	procedure set_y (point : in out type_point'class; y : in type_distance_xy) is
	-- Assigns a point the given y position.
	begin
		point.y := y;
	end set_y;

	procedure set_xy (
		point 		: in out type_point;
		position	: in type_point'class) is
	begin
		point.x := position.x;
		point.y := position.y;
	end set_xy;

	function set_point (x, y : in type_distance_xy) return type_point'class is 
		point : type_point;
	begin
		point.x := x;
		point.y := y;
		return point;
	end set_point;
	
	procedure mirror (
		point	: in out type_point;
		axis	: in type_axis)
		is
	begin
		case axis is
			when X =>
				point.y := point.y * (-1.0);
			when Y =>
				point.x := point.x * (-1.0);
		end case;
	end mirror;

	procedure move (
		point	: in out type_point;
		offset	: in type_point'class)
		is
	begin
		point.x := point.x + offset.x;
		point.y := point.y + offset.y;
	end move;

	procedure rotate (
	-- Rotates the given point by the given angle with the origin as center.
		point	: in out type_point;
		angle	: in type_angle) is

		type type_float_distance is digits 7 range -1000.0 .. 1000.0; -- CS: refine
		package functions_distance is new ada.numerics.generic_elementary_functions (type_float_distance);
		use functions_distance;
		
		type type_float_angle is digits 4 range -719.9 .. 719.9; -- CS: refine			
		package functions_angle is new ada.numerics.generic_elementary_functions (type_float_angle);
		use functions_angle;

		angle_out			: type_float_angle;		-- unit is degrees
		distance_to_origin	: type_float_distance;	-- unit is mm
		scratch				: type_float_distance;

		--use et_string_processing;
	begin
		-- Do nothing if the given rotation is zero.
		if angle /= 0 then

			-- compute distance of given point to origin
			if point.x = zero_distance and point.y = zero_distance then
				distance_to_origin := type_float_distance (zero_distance);
			elsif point.x = zero_distance then
				distance_to_origin := type_float_distance (abs (point.y));
			elsif point.y = zero_distance then
				distance_to_origin := type_float_distance (abs (point.x));
			else
				distance_to_origin := sqrt (
					type_float_distance (abs (point.x)) ** type_float_distance (2) 
					+
					type_float_distance (abs (point.y)) ** type_float_distance (2)
					);
			end if;
			
			-- compute the current angle of the given point (in degrees)

			if point.x = zero_distance then
				if point.y > zero_distance then
					angle_out := 90.0;
				elsif point.y < zero_distance then
					angle_out := -90.0;
				else
					angle_out := 0.0;
				end if;

			elsif point.y = zero_distance then
				if point.x > zero_distance then
					angle_out := 0.0;
				elsif point.x < zero_distance then
					angle_out := 180.0;
				else
					angle_out := 0.0;
				end if;

			else
				angle_out := type_float_angle (arctan (
					x => type_float_distance (point.x),
					y => type_float_distance (point.y),
					cycle => type_float_distance (units_per_cycle))
					);
			end if;

			-- Compute new angle by adding current angle and given angle.
			-- This computation depends on the Y axis style. The in the conventional style (Y going upwards positive)
			-- we add the given angle to the current angle. In the old fashioned stlyle (Y going downwards positive)
			-- we subtract the given angle from the current angle.
-- 			log_indentation_up;
-- 			log ("angle in  " & to_string (type_angle (angle_out)), log_threshold);
			if Y_axis_positive = upwards then
				angle_out := angle_out + type_float_angle (angle);
			else
				angle_out := angle_out - type_float_angle (angle);
			end if;
-- 			log ("angle out " & type_float_angle'image (angle_out), log_threshold);
			
			-- compute new x   -- (cos angle_out) * distance_to_origin
			scratch := cos (type_float_distance (angle_out), type_float_distance (units_per_cycle));
			point.x := type_distance (scratch * distance_to_origin);
			--log ("x in sch. " & to_string (point.x), log_threshold);

			-- compute new y   -- (sin angle_out) * distance_to_origin
			scratch := sin (type_float_distance (angle_out), type_float_distance (units_per_cycle));
			point.y := type_distance (scratch * distance_to_origin);
-- 			log ("point out " & to_string (point), log_threshold);
-- 			log_indentation_down;
	
		end if; -- if angle not zero
	end rotate;

	function distance (point_1, point_2 : in type_point) return type_distance is
	-- Returns the total distance between the given points.
		dis : float;
		package functions_distance is new ada.numerics.generic_elementary_functions (float);
		use functions_distance;
	begin
		-- To save computing time a few simple checks:
		
		if point_1 = point_2 then -- points have same x/y position -> zero difference
			dis := float (zero_distance);
			
		elsif point_1.x = point_2.x then -- points are in a vertical line
			dis := float (abs (point_2.y - point_1.y));
			
		elsif point_1.y = point_2.y then -- points are in a horizontal line
			dis := float (abs (point_2.x - point_1.x));

		else -- distance = sqrt (delta_x^2 + delta_y^2)
			dis := sqrt (
					(float (abs (point_2.x - point_1.x))) ** 2
					+
					(float (abs (point_2.y - point_1.y))) ** 2 
					);

		end if;

		return type_distance (dis);
	end distance;

	function distance (
	-- Returns the absolute distance on the given axis between the given points.
		point_1	: in type_point;
		point_2	: in type_point;
		axis	: in type_axis) 
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
	
	function to_sheet (sheet : in type_sheet) return string is begin
		return type_sheet'image (sheet);
	end;

	function to_sheet (sheet : in string) return type_sheet is begin
		return type_sheet'value (sheet);
	end;

	function to_sheet_relative (sheet : in type_sheet_relative) return string is begin
		return type_sheet_relative'image (sheet);
	end;
	
	function to_sheet_relative (sheet : in string) return type_sheet_relative is begin
		return type_sheet_relative'value (sheet);
	end;

	
	procedure move (
		position	: in out type_coordinates'class;
		offset		: in type_coordinates_relative) is
		use et_string_processing;
	begin
		position.x := position.x + offset.x;
		position.y := position.y + offset.y;

		-- Constraint error will arise here if resulting sheet number is less than 1.
		position.sheet := type_sheet (type_sheet_relative (position.sheet) + offset.sheet);
	end;
	
	function to_coordinates (
		point 	: in type_point'class;
		sheet	: in type_sheet)
		return type_coordinates is
	begin
		return (
			x		=> point.x,
			y		=> point.y,
			sheet	=> sheet
			);
	end;

	function to_coordinates_relative (
		point 	: in type_point'class;
		sheet	: in type_sheet_relative)
		return type_coordinates_relative is
	begin
		return (
			x		=> point.x,
			y		=> point.y,
			sheet	=> sheet
			);
	end;
	
	function to_string (position : in type_coordinates) return string is
		use et_string_processing;
	begin
		return coordinates_preamble_sheet
				& to_sheet (position.sheet) 
				& latin_1.space & axis_separator & latin_1.space
				& to_string (distance_x (position))
				& latin_1.space & axis_separator & latin_1.space
				& to_string (distance_y (position));
	end to_string;

	function sheet (position : in type_coordinates) return type_sheet is
	begin
		return position.sheet;
	end sheet;

	procedure set_sheet (position : in out type_coordinates; sheet : in type_sheet) is
	-- Sets the sheet number in given position.
	begin
		position.sheet := sheet;
	end set_sheet;

	function paper_dimension (
	-- Returns for the given paper size, orientation and axis the correspoinding size in mm.
		paper_size	: in et_general.type_paper_size;
		orientation	: in et_general.type_paper_orientation := et_general.LANDSCAPE;
		axis		: in type_axis)
		return type_distance_xy is

		dimension : type_distance;
		use et_general;
	
	begin
		case orientation is
			when LANDSCAPE =>
				case paper_size is 
					when A3 =>
						case axis is
							when X => dimension := paper_size_A3_x;
							when Y => dimension := paper_size_A3_y;
						end case;

					when A4 =>
						case axis is
							when X => dimension := paper_size_A4_x;
							when Y => dimension := paper_size_A4_y;
						end case;
				end case;

			when PORTRAIT =>
				case paper_size is 
					when A3 =>
						case axis is
							when X => dimension := paper_size_A3_y;
							when Y => dimension := paper_size_A3_x;
						end case;

					when A4 =>
						case axis is
							when X => dimension := paper_size_A4_y;
							when Y => dimension := paper_size_A4_x;
						end case;
				end case;

		end case;

		return dimension;
	end paper_dimension;

	
end et_coordinates;

-- Soli Deo Gloria
