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
	
	function to_angle (angle : in string) return type_rotation is 
		use et_string_processing;
		r : type_rotation;
	begin
		r := type_rotation'value (angle);
		return r;

		exception 
			when constraint_error => 
				log (ERROR, "Rotation " & angle & " outside range" & 
					 geometry.to_string (type_rotation'first) &
					 " .." & 
					 geometry.to_string (type_rotation'last) &
					 " (must be an integer) !",
					 console => true
					);
				raise;

			-- CS check for multiple of 90 degree
			when system.assertions.assert_failure =>
				log (ERROR, "Rotation " & angle & " is not a multiple of" &
					 geometry.to_string (rotation => type_rotation'small) & " !",
					 console => true
					);
				raise;
				
			when others =>
				raise;
	end to_angle;
	
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

	function "<" (left, right : in type_position) return boolean is
		result : boolean := false;
		use et_coordinates.geometry;
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
					-- y positions equal -> compare rotation

					if rot (left) < rot (right) then
						result := true;
					elsif rot (left) > rot (right) then
						result := false;
					else
						-- rotations equal
						result := false;
					end if;
				end if;

			end if;
		end if;
			
		return result;
	end;
	
	procedure move (
		position	: in out type_position'class;
		offset		: in type_position_relative) is
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
		point 		: in geometry.type_point'class;
		sheet		: in type_sheet;
		rotation	: in type_rotation := geometry.zero_rotation)
		return type_position is

		use geometry;
		p : type_position;
	begin
		set (p, point);
		set_sheet (p, sheet);
		set (p, rotation);
		return p;
	end;

	function to_coordinates_relative (
		point 		: in geometry.type_point'class;
		sheet		: in type_sheet_relative;
		rotation	: in type_rotation := geometry.zero_rotation)
		return type_position_relative is
		p : type_position_relative;
		use geometry;
	begin
		set (p, point);
		p.sheet := sheet;
		set (p, rotation);
		return p;
	end;
	
	function to_string (position : in type_position) return string is
		use et_string_processing;
		use geometry;

		coordinates_preamble_sheet : constant string := " pos "
			& "(sheet"
			& axis_separator
			& "x"
			& axis_separator
			& "y) ";

	begin
		return coordinates_preamble_sheet
			& to_sheet (position.sheet) 
			& latin_1.space & axis_separator & latin_1.space
			& to_string (x (position))
			& latin_1.space & axis_separator & latin_1.space
			& to_string (y (position));
	end to_string;

	function sheet (position : in type_position) return type_sheet is
	begin
		return position.sheet;
	end sheet;

	procedure set_sheet (position : in out type_position; sheet : in type_sheet) is
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
