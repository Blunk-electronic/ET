------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        SCHEMATIC COORDINATES                             --
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
	
-- 	function to_distance (distance : in string) return type_distance_xy is begin
-- 		return type_distance'value (distance);
-- 	end to_distance;
	
-- 	function to_string (distance : in type_distance) return string is begin
-- 		if distance < zero then
-- 			return latin_1.space & type_distance'image (distance);
-- 		else
-- 			return type_distance'image (distance);
-- 		end if;
-- 	end to_string;


-- 	function to_string (angle : in type_rotation) return string is
-- 	-- Returns the the given angle as string.
-- 		suffix : constant string := " deg";
-- 	begin
-- 		return type_rotation'image (angle) & suffix;
-- 	end to_string;

	function to_angle (angle : in string) return type_rotation is 
		use et_string_processing;
		r : type_rotation;
	begin
		r := type_rotation'value (angle);
		return r;

		exception 
			when constraint_error => 
				log (ERROR, "Rotation " & angle & " outside range" & 
					 to_string (type_rotation'first) &
					 " .." & 
					 to_string (type_rotation'last) &
					 " (must be an integer) !",
					 console => true
					);
				raise;

			-- CS check for multiple of 90 degree
			when system.assertions.assert_failure =>
				log (ERROR, "Rotation " & angle & " is not a multiple of" &
					 to_string (rotation => type_rotation'small) & " !",
					 console => true
					);
				raise;
				
			when others =>
				raise;
	end to_angle;

	function add (left, right : in type_rotation) return type_rotation is
	-- Adds two angles.
	-- If result greater or equal 360 degree then 360 degree is subtracted from result.
	-- If reuslt less or equal 360 degree then 360 degree is added to the result.
		scratch : integer;
		result : type_rotation; -- to be returned
	begin
		scratch := integer (left) + integer (right);
		
		if scratch >= 360 then
			scratch := scratch - 360;
			
		elsif scratch <= -360 then
			scratch := scratch + 360;
		end if;

		result := type_rotation (scratch);
		return result;
	end;
	
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

-- 	function to_string (point : in type_point'class) return string is
-- 	-- Returns the given point coordinates to a string.
-- 		use et_string_processing;
-- 	begin
-- 		return position_preamble
-- 		-- 	& trim (type_distance'image (point.x), left)
-- 			& to_string (point.x)
-- 			& latin_1.space & axis_separator & latin_1.space
-- 		--	& trim (type_distance'image (point.y), left);
-- 			& to_string (point.y);
-- 	
-- 	end to_string;

-- 	procedure rotate (
-- 	-- Rotates the given point by the given angle with the origin as center.
-- 		point	: in out type_point'class;
-- 		angle	: in type_rotation) is
-- 
-- 		type type_float_distance is digits 7 range -1000.0 .. 1000.0; -- CS: refine
-- 		package functions_distance is new ada.numerics.generic_elementary_functions (type_float_distance);
-- 		use functions_distance;
-- 		
-- 		type type_float_angle is digits 4 range -719.9 .. 719.9; -- CS: refine			
-- 		package functions_angle is new ada.numerics.generic_elementary_functions (type_float_angle);
-- 		use functions_angle;
-- 
-- 		angle_out			: type_float_angle;		-- unit is degrees
-- 		distance_to_origin	: type_float_distance;	-- unit is mm
-- 		scratch				: type_float_distance;
-- 
-- 		units_per_cycle : constant float := 360.0; -- CS type_float_angle ?
-- 
-- 		use geometry;
-- 	begin -- rotate
-- 		-- Do nothing if the given rotation is zero.
-- 		if angle /= geometry.zero_rotation then
-- 
-- 			-- compute distance of given point to origin
-- 			if point.x = zero and point.y = zero then
-- 				distance_to_origin := type_float_distance (zero);
-- 			elsif point.x = zero then
-- 				distance_to_origin := type_float_distance (abs (point.y));
-- 			elsif point.y = zero then
-- 				distance_to_origin := type_float_distance (abs (point.x));
-- 			else
-- 				distance_to_origin := sqrt (
-- 					type_float_distance (abs (point.x)) ** type_float_distance (2) 
-- 					+
-- 					type_float_distance (abs (point.y)) ** type_float_distance (2)
-- 					);
-- 			end if;
-- 			
-- 			-- compute the current angle of the given point (in degrees):
-- 
-- 			if point.x = zero then
-- 				if point.y > zero then
-- 					angle_out := 90.0;
-- 				elsif point.y < zero then
-- 					angle_out := -90.0;
-- 				else
-- 					angle_out := 0.0;
-- 				end if;
-- 
-- 			elsif point.y = zero then
-- 				if point.x > zero then
-- 					angle_out := 0.0;
-- 				elsif point.x < zero then
-- 					angle_out := 180.0;
-- 				else
-- 					angle_out := 0.0;
-- 				end if;
-- 
-- 			else
-- 				angle_out := type_float_angle (arctan (
-- 					x => type_float_distance (point.x),
-- 					y => type_float_distance (point.y),
-- 					cycle => type_float_distance (units_per_cycle))
-- 					);
-- 			end if;
-- 
-- 			-- Compute new angle by adding current angle and given angle.
-- 			-- This computation depends on the Y axis style. The in the conventional style (Y going upwards positive)
-- 			-- we add the given angle to the current angle. In the old fashioned stlyle (Y going downwards positive)
-- 			-- we subtract the given angle from the current angle.
-- -- 			log_indentation_up;
-- -- 			log ("angle in  " & to_string (type_rotation (angle_out)), log_threshold);
-- -- 			if Y_axis_positive = upwards then
-- 				angle_out := angle_out + type_float_angle (angle);
-- -- 			else
-- -- 				angle_out := angle_out - type_float_angle (angle);
-- -- 			end if;
-- -- 			log ("angle out " & type_float_angle'image (angle_out), log_threshold);
-- 			
-- 			-- compute new x   -- (cos angle_out) * distance_to_origin
-- 			scratch := cos (type_float_distance (angle_out), type_float_distance (units_per_cycle));
-- 			--point.x := type_distance (scratch * distance_to_origin);
-- 			set (X, type_distance (scratch * distance_to_origin), point);
-- 			--log ("x in sch. " & to_string (point.x), log_threshold);
-- 
-- 			-- compute new y   -- (sin angle_out) * distance_to_origin
-- 			scratch := sin (type_float_distance (angle_out), type_float_distance (units_per_cycle));
-- 			--point.y := type_distance (scratch * distance_to_origin);
-- 			set (Y, type_distance (scratch * distance_to_origin), point);
-- -- 			log ("point out " & to_string (point), log_threshold);
-- -- 			log_indentation_down;
-- 	
-- 		end if; -- if angle not zero
-- 	end rotate;
	
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

	function "<" (left, right : in type_coordinates) return boolean is
	-- Returns true if left comes before right.
	-- Returns false if left equals right.
		result : boolean := false;
	begin
		if left.sheet < right.sheet then
			result := true;
		elsif left.sheet > right.sheet then
			result := false;
		else
			-- sheet numbers are equal -> compare x
			
			if left.x < right.x then
				result := true;
			elsif left.x > right.x then
				result := false;
			else 
				-- x positions equal -> compare y
				
				if left.y < right.y then
					result := true;
				elsif left.y > right.y then
					result := false;
				else
					-- y positions equal
					result := false;
				end if;

			end if;
		end if;
			
		return result;
	end;
	
	procedure move (
		position	: in out type_coordinates'class;
		offset		: in type_coordinates_relative) is
		use et_string_processing;
		use geometry;
	begin
-- 		position.x := position.x + offset.x;
		set (X, x (position) + x (offset), position);
		
-- 		position.y := position.y + offset.y;
		set (Y, y (position) + y (offset), position);

		-- Constraint error will arise here if resulting sheet number is less than 1.
		position.sheet := type_sheet (type_sheet_relative (position.sheet) + offset.sheet);
	end;
	
	function to_coordinates (
		point 	: in type_point'class;
		sheet	: in type_sheet)
		return type_coordinates is
	begin
-- 		return (
-- 			x		=> point.x,
-- 			y		=> point.y,
-- 			sheet	=> sheet
-- 			);
		return (type_point (point) with sheet);
	end;

	function to_coordinates_relative (
		point 	: in type_point'class;
		sheet	: in type_sheet_relative)
		return type_coordinates_relative is
	begin
-- 		return (
-- 			x		=> point.x,
-- 			y		=> point.y,
-- 			sheet	=> sheet
-- 			);
		return (type_point (point) with sheet);
	end;
	
	function to_string (position : in type_coordinates) return string is
		use et_string_processing;
	begin
		return coordinates_preamble_sheet
			& to_sheet (position.sheet) 
			& latin_1.space & axis_separator & latin_1.space
			& to_string (x (position))
			& latin_1.space & axis_separator & latin_1.space
			& to_string (y (position));
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
		axis		: in type_axis_2d)
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

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
