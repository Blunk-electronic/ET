------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD COORDINATES                               --
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
with ada.characters;					use ada.characters;
with ada.characters.latin_1;			use ada.characters.latin_1;
with ada.characters.handling;			use ada.characters.handling;
with ada.exceptions;

with et_axes;							use et_axes;
with et_keywords;						use et_keywords;
with et_general_rw;


package body et_board_coordinates is

	
	function to_string (
		position	: in type_package_position;
		format		: in type_output_format := FORMAT_1)
		return string 
	is 
		x : constant string := to_string (get_x (position));
		y : constant string := to_string (get_y (position));
		r : constant string := to_string (get_rotation (position));
		f : constant string := to_string (get_face (position));

		separator : constant string := " / ";
	begin
		case format is
			when FORMAT_1 =>
				return "x/y/rotation/face " & x & separator & y & separator & r & separator & f;

			when FORMAT_2 =>
				return "x " & x & " y " & y & " rotation " & r & " f " & f;

			when FORMAT_3 =>
				return x & space & y & space & r & space & f;

			when others => -- CS do the same as with FORMAT_1
				return "x/y/rotation/face " & x & separator & y & separator & r & separator & f;
		end case;
	end to_string;





	

	function to_position (
		line : in type_fields_of_line;
		from : in type_field_count_positive)
		return type_package_position
	is
		use et_general_rw;
		
		position : type_package_position; -- to be returned
		place : type_field_count_positive := from; -- the field being read from given line

		-- CS: more detailled syntax check required
		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= get_field_count (line) loop

			-- We expect after the x the corresponding value for x
			if get_field (line, place) = keyword_x then
				set (position.place, AXIS_X, to_distance (get_field (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif get_field (line, place) = keyword_y then
				set (position.place, AXIS_Y, to_distance (get_field (line, place + 1)));

			-- We expect after "rotation" the corresponding value for the rotation
			elsif get_field (line, place) = keyword_rotation then
				set (position, to_rotation (get_field (line, place + 1)));

			-- We expect after "face" the actual face (top/bottom)
			elsif get_field (line, place) = keyword_face then
				set_face (position, to_face (get_field (line, place + 1)));
			else
				invalid_keyword (get_field (line, place));
				raise constraint_error; -- CS
			end if;
				
			place := place + 2;
		end loop;
		
		return position;
	end to_position;



	

	
	
	function to_package_position (
		point 		: in type_vector_model;
		rotation	: in type_rotation_model := zero_rotation;
		face		: in type_face := TOP)
		return type_package_position 
	is begin
		return pos : type_package_position do
			set (pos, point);
			set (pos, rotation);
			pos.face := face;
		end return;
	end to_package_position;


	
	
	procedure set_face (
		position: in out type_package_position;
		face	: in type_face)
	is begin
		position.face := face;
	end set_face;


	
	
	function get_face (
		packge : in type_package_position)
		return type_face 
	is begin
		return packge.face;
	end get_face;



	

	procedure toggle_face (
		position : in out type_package_position)
	is begin
		case get_face (position) is
			when TOP	=> set_face (position, BOTTOM);
			when BOTTOM	=> set_face (position, TOP);
		end case;
	end;

	
	
	function get_place (
		position : in type_package_position)
		return type_vector_model
	is begin
		return position.place;
	end get_place;



	procedure set_place (
		position 	: in out type_package_position;
		place		: in type_vector_model)
	is begin
		position.place := place;
	end;
	

	
	function get_position (
		position : in type_package_position)
		return type_position
	is begin
		return type_position (position);
	end get_position;

	
	
	function to_terminal_position (
		point		: in type_vector_model;
		rotation	: in type_rotation_model)
		return type_position'class 
	is
		pos : type_position;
	begin
		--pos := (point with rotation);
		set (pos, point);
		set (pos, rotation);
		return pos;
	end to_terminal_position;


	
end et_board_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
