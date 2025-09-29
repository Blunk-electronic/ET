------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        SCHEMATIC COORDINATES                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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

with ada.text_io;						use ada.text_io;
with ada.exceptions;
-- with ada.characters.handling;		use ada.characters.handling;
with ada.strings;						use ada.strings;

with et_axes;							use et_axes;
with et_keywords;						use et_keywords;
with et_general_rw;


package body et_schematic_coordinates is


	
	
	function "<" (left, right : in type_object_position) 
		return boolean 
	is
		result : boolean := false;
	begin
		if left.sheet < right.sheet then
			result := true;
		elsif left.sheet > right.sheet then
			result := false;
		else
			-- sheet numbers are equal -> compare x
			
			if get_x (left) < get_x (right) then
				result := true;
			elsif get_x (left) > get_x (right) then
				result := false;
			else 
				-- x positions equal -> compare y
				
				if get_y (left) < get_y (right) then
					result := true;
				elsif get_y (left) > get_y (right) then
					result := false;
				else
					-- y positions equal -> compare rotation

					if get_rotation (left) < get_rotation (right) then
						result := true;
					elsif get_rotation (left) > get_rotation (right) then
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
		position	: in out type_object_position'class;
		offset		: in type_object_position_relative) 
	is begin
		position.set (AXIS_X, get_x (position) + get_x (offset));
		position.set (AXIS_Y, get_y (position) + get_y (offset));

		-- Constraint error will arise here if resulting sheet number is less than 1.
		position.sheet := type_sheet (type_sheet_relative (position.sheet) + offset.sheet);
	end;



	
	function to_position (
		position	: in type_position;
		sheet		: in type_sheet)
		return type_object_position
	is 
		result : type_object_position;
	begin
		result.place := position.place;
		result.rotation := get_rotation (position);
		result.sheet := sheet;
		
		return result;
	end;


	
	
	
	function to_position (
		point 		: in type_vector_model;
		sheet		: in type_sheet;
		rotation	: in type_rotation_model := zero_rotation)
		return type_object_position 
	is
		p : type_object_position;
	begin
		set (p, point);
		set_sheet (p, sheet);
		set (p, rotation);
		return p;
	end;

	

	
	function to_position_relative (
		point 		: in type_vector_model;
		sheet		: in type_sheet_relative;
		rotation	: in type_rotation_model := zero_rotation)
		return type_object_position_relative 
	is
		p : type_object_position_relative;
	begin
		set (p, point);
		p.sheet := sheet;
		set (p, rotation);
		return p;
	end;


	


	function to_string (
		position	: in type_object_position;
		format		: in type_output_format := FORMAT_1)
		return string
	is
		s : constant string := to_string (get_sheet (position));
		x : constant string := to_string (get_x (position));
		y : constant string := to_string (get_y (position));
		r : constant string := to_string (get_rotation (position));
		
		separator : constant string := " / ";
	begin
		case format is
			when FORMAT_1 =>
				return "sheet/x/y/rotation " & s & separator & x & separator & y & separator & r;

			when FORMAT_2 =>
				return "sheet " & s & " x " & x & " y " & y & " rotation " & r;

			when FORMAT_3 =>
				return s & space & x & space & y & space & r;

			when others => -- CS: do the same as with FORMAT_1
				return "sheet/x/y/rotation " & s & separator & x & separator & y & separator & r;
				
		end case;
	end to_string;





	

	function to_position (
		line : in type_fields_of_line;
		from : in type_field_count_positive)
		return type_object_position
	is		
		use pac_geometry_2;
		use et_sheets;
		use et_general_rw;
		
		position : type_object_position; -- to be returned
		place : type_field_count_positive := from; -- the field being read from given line

		-- CS: more detailled syntax check required
		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= get_field_count (line) loop

			-- We expect after "sheet" the sheet number
			if get_field (line, place) = keyword_sheet then
				position.sheet := to_sheet (get_field (line, place + 1));
				
			-- We expect after the x the corresponding value for x
			elsif get_field (line, place) = keyword_x then
				set (position.place, AXIS_X, to_distance (get_field (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif get_field (line, place) = keyword_y then
				set (position.place, AXIS_Y, to_distance (f (line, place + 1)));


			elsif get_field (line, place) = keyword_rotation then
				position.set (to_rotation (get_field (line, place + 1)));

			else
				invalid_keyword (get_field (line, place));
				raise constraint_error; -- CS
			end if;
				
			place := place + 2;
		end loop;
		
		return position;
	end to_position;




	
	function to_position (
		line : in type_fields_of_line;
		from : in type_field_count_positive)
		return type_object_position_relative
	is		
		use pac_geometry_2;
		use et_sheets;
		use et_general_rw;
		
		position : type_object_position_relative; -- to be returned
		place : type_field_count_positive := from; -- the field being read from given line

		-- CS: more detailled syntax check required
		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= get_field_count (line) loop

			-- We expect after "sheet" the sheet number
			if get_field (line, place) = keyword_sheet then
				position.sheet := to_sheet (get_field (line, place + 1));
				
			-- We expect after the x the corresponding value for x
			elsif get_field (line, place) = keyword_x then
				set (position.place, AXIS_X, to_distance (get_field (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif get_field (line, place) = keyword_y then
				set (position.place, AXIS_Y, to_distance (f (line, place + 1)));


			elsif get_field (line, place) = keyword_rotation then
				position.set (to_rotation (get_field (line, place + 1)));

			else
				invalid_keyword (get_field (line, place));
				raise constraint_error; -- CS
			end if;
				
			place := place + 2;
		end loop;
		
		return position;
	end to_position;

	

	
	
	function get_place (
		position : in type_object_position)
		return type_vector_model
	is 
		result : type_vector_model;
	begin
		result := position.place;
		return result;
	end get_place;




	procedure set_place (
		position 	: in out type_object_position;
		place		: in type_vector_model)
	is begin
		position.place := place;
	end set_place;



	
	function get_rotation (
		position : in type_object_position) 
		return type_rotation_model
	is begin
		return position.rotation;
	end get_rotation;





	procedure set_rotation (
		position 	: in out type_object_position;
		rotation	: in type_rotation_model)
	is begin
		position.rotation := rotation;
	end set_rotation;
	
	
	
	
	function get_sheet (
		position : in type_object_position) 
		return type_sheet 
	is begin
		return position.sheet;
	end get_sheet;


	
	
	procedure set_sheet (
		position	: in out type_object_position;
		sheet		: in type_sheet) 
	is begin
		position.sheet := sheet;
	end set_sheet;


	
end et_schematic_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
