------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD COORDINATES                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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
-- with ada.text_io;			use ada.text_io;
with ada.characters;					use ada.characters;
with ada.characters.latin_1;			use ada.characters.latin_1;

with et_axes;							use et_axes;
with et_keywords;						use et_keywords;


package body et_board_coordinates is


-- FACE (top/bottom):

	function get_face (
		object	: in type_board_object)
		return type_face
	is (object.face);


	function get_face (
		object	: in type_board_object)
		return string
	is (to_string (get_face (object)));



	procedure set_face (
		object	: in out type_board_object;
		face	: in type_face)
	is begin
		object.face := face;
	end set_face;



	procedure toggle_face (
		object	: in out type_board_object)
	is begin
		case get_face (object) is
			when TOP	=> set_face (object, BOTTOM);
			when BOTTOM	=> set_face (object, TOP);
		end case;
	end toggle_face;



	function is_flipped (
		object	: in type_board_object)
		return boolean
	is (get_face (object) = BOTTOM);




-- AREA:

	function on_face_and_in_area (
		object	: in type_board_object;
		face	: in type_face;
		area	: in type_area)
		return boolean
	is
		result : boolean := false;
	begin
		if get_face (object) = face
		and in_area (object, area) then
			result := true;
		end if;

		return result;
	end on_face_and_in_area;






-- POSITION:

	-- function get_position (
	-- 	object	: in type_board_object)
	-- 	return type_board_object'class
	-- is (type_board_object (object));


	procedure set_position (
		object		: in out type_board_object;
		position	: in type_board_object'class)
	is begin
		object.set_place (get_place (position));
		object.set_rotation (get_rotation (position));
		object.set_face (get_face (position));
	end set_position;




	function get_position (
		object	: in type_board_object;
		format	: in type_output_format := FORMAT_1)
		return string
	is
		x : constant string := to_string (get_x (object));
		y : constant string := to_string (get_y (object));
		r : constant string := to_string (get_rotation (object));
		f : constant string := to_string (get_face (object));

		separator : constant string := " / ";
	begin
		case format is
			when FORMAT_1 =>
				return "x/y/rotation/face " & x & separator & y & separator & r & separator & f;

			when FORMAT_2 =>
				return "x " & x & " y " & y & " rotation " & r & " face " & f;

			when FORMAT_3 =>
				return x & space & y & space & r & space & f;

			when others => -- CS do the same as with FORMAT_1
				return "x/y/rotation/face " & x & separator & y & separator & r & separator & f;
		end case;
	end get_position;







	function to_board_object_position (
		line : in type_fields_of_line;
		from : in type_field_count_positive)
		return type_board_object'class
	is
		position : type_board_object; -- to be returned
		place : type_field_count_positive := from; -- the field being read from given line

		-- CS: more detailled syntax check required
		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= get_field_count (line) loop

			-- We expect after the x the corresponding value for x
			if get_field (line, place) = keyword_x then
				set_x (position, to_distance (get_field (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif get_field (line, place) = keyword_y then
				set_y (position, to_distance (get_field (line, place + 1)));

			-- We expect after "rotation" the corresponding value for the rotation
			elsif get_field (line, place) = keyword_rotation then
				set_rotation (position, to_rotation (get_field (line, place + 1)));

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
	end to_board_object_position;







	function to_board_object_position (
		point		: in type_vector_model;
		rotation	: in type_rotation_model := zero_rotation;
		face		: in type_face := TOP)
		return type_board_object'class
	is begin
		return pos : type_board_object do
			set_place (pos, point);
			set_rotation (pos, rotation);
			set_face (pos, face);
		end return;
	end to_board_object_position;



end et_board_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
