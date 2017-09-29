------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET COORDINATES                          --
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

with ada.numerics.generic_elementary_functions;
with et_string_processing;

package body et_coordinates is

	function mil_to_distance (mil : in string) return type_distance is
	-- Returns the given mils to type_distance.		

		type type_distance_intermediate is digits 13 range -10000000.0 .. 1000000.0;
		-- unit is mil
		-- CS: refine range and delta if required

		d_in : type_distance_intermediate;
	begin
		d_in := type_distance_intermediate'value (mil);

		return type_distance ( d_in * (25.4 * 0.001) );
		
		-- CS: exception handler
	end mil_to_distance;

	
	function to_string (distance : in type_distance) return string is
	-- Returns the given distance to a string.
	begin
		return trim (type_distance'image (distance), left);
	end to_string;



	function to_string (angle : in type_angle) return string is
	-- Returns the the given angle as string.
		preamble	: constant string (1..5) := "angle";
		suffix		: constant string (1..4) := " deg";
	begin
		return (preamble & type_angle'image (angle) & suffix);
	end to_string;

	procedure warning_angle_greater_90_degrees is
		use et_string_processing;
	begin
		log (et_string_processing.message_warning 
			& "text placed in an angle greater than" 
			& type_angle_90'image (type_angle_90'last));
	end warning_angle_greater_90_degrees;
	
	
	
	function to_string (point : in type_2d_point) return string is
	-- Returns the given point coordinates to a string.
	begin
		return position_preamble
			& trim (type_distance'image (point.x), left)
			& latin_1.space & axis_separator & latin_1.space
			& trim (type_distance'image (point.y), left);
	end to_string;

	function distance_x (point : in type_2d_point) return type_distance is
	begin
		return point.x;
	end distance_x;

	function distance_y (point : in type_2d_point) return type_distance is
	begin
		return point.y;
	end distance_y;

	procedure set_x (point : in out type_2d_point; x : in type_distance) is
	begin
		point.x := x;
	end set_x;
	
	procedure set_y (point : in out type_2d_point; y : in type_distance) is
	begin
		point.y := y;
	end set_y;

	procedure set (point : in out type_2d_point; position : in type_2d_point) is
	begin
		point := position;
	end set;
	
	procedure mirror (
		point	: in out type_2d_point;
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
		point	: in out type_2d_point;
		offset	: in type_2d_point)
		is
	begin
		point.x := point.x + offset.x;
		point.y := point.y + offset.y;
	end move;

	procedure rotate (
	-- Rotates the given point by the given angle with the origin as center.
		point	: in out type_2d_point;
		angle	: in type_angle)
		is

		type type_float_distance is digits 7 range -1000.0 .. 1000.0; -- CS: refine
		package functions_distance is new ada.numerics.generic_elementary_functions (type_float_distance);
		use functions_distance;
		
		type type_float_angle is digits 4 range -719.9 .. 719.9; -- CS: refine			
		package functions_angle is new ada.numerics.generic_elementary_functions (type_float_angle);
		use functions_angle;

		angle_out			: type_float_angle;		-- unit is degrees
		distance_to_origin	: type_float_distance;	-- unit is mm
		scratch				: type_float_distance;
	begin
		-- Do nothing if the given rotation is zero.
		if angle /= 0.0 then

			-- CS: in order to improve performance some ops can be skipped or simplified
			-- if point.x or point.y are zero
			
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
			if point.x /= zero_distance then
				angle_out := type_float_angle (arctan (
					x => type_float_distance (point.x),
					y => type_float_distance (point.y),
					cycle => type_float_distance (units_per_cycle))
					);
			else -- x = 0
				if point.y > zero_distance then
					angle_out := 90.0;
				elsif point.y < zero_distance then
					angle_out := -90.0;
				else
					angle_out := 0.0;
				end if;
			end if;
			
			-- compute new angle by adding current angle and given angle
			angle_out := angle_out + type_float_angle (angle);

	-- 		-- Remove multiturns in angle_out. 
	-- 		CS: no need because angle_out is invisible to the outside world.
	-- 		-- example 1: angle_out =  370 degrees.  370 - 360 =  10. so angle_out equals  10 degrees.
	-- 		-- example 2: angle_out = -370 degrees. -370 + 360 = -10. so angle_out equals -10 degrees.
	-- 		if angle_out > type_float_angle (type_angle'last) then
	-- 			angle_out := angle_out - type_float_angle (units_per_cycle);
	-- 		elsif angle_out < type_float_angle (type_angle'first) then
	-- 			angle_out := angle_out + type_float_angle (units_per_cycle);
	-- 		else
	-- 			null;
	-- 		end if;

			-- compute new x   -- (cos angle_out) * distance_to_origin
			scratch := cos (type_float_distance (angle_out), type_float_distance (units_per_cycle));
			point.x := type_distance (scratch * distance_to_origin);

			-- compute new y   -- (sin angle_out) * distance_to_origin
			scratch := sin (type_float_distance (angle_out), type_float_distance (units_per_cycle));
			point.y := type_distance (scratch * distance_to_origin);
	
		end if; -- if angle not zero
	end rotate;

	function to_string (position : in type_coordinates) return string is
	-- Returns the given position as string.
	begin
		return coordinates_preamble
			& trim (positive'image (position.sheet_number),left) 
			& latin_1.space & axis_separator & latin_1.space
			--& to_string ( type_2d_point (position));
			& to_string (distance_x (position))
			& latin_1.space & axis_separator & latin_1.space
			& to_string (distance_y (position));

		-- CS: output in both mil and mm
		
		-- CS: exception handler
	end to_string;

	function path (position : in type_coordinates) return type_path_to_submodule.list is
	begin
		return position.path;
	end path;

	function module (position : in type_coordinates) return type_submodule_name.bounded_string is
	begin
		return position.module_name;
	end module;

	function sheet (position : in type_coordinates) return positive is
	begin
		return position.sheet_number;
	end sheet;

	procedure set_module (position : in out type_coordinates; name : in type_submodule_name.bounded_string) is
	-- Sets the module name in given position.
	begin
		position.module_name := name;
	end set_module;

	procedure set_path (position : in out type_coordinates; path : in type_path_to_submodule.list) is
	-- Sets the path in given position.
	begin
		position.path := path;
	end set_path;

	procedure set_sheet (position : in out type_coordinates; sheet : in positive) is
	-- Sets the sheet number in given position.
	begin
		position.sheet_number := sheet;
	end set_sheet;
	
end et_coordinates;

-- Soli Deo Gloria
