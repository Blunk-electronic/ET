------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            BASIC OBJECTS                                 --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--
--  To Do:
--
--

with ada.strings;					use ada.strings;
with ada.characters.latin_1;
with ada.characters.handling;		use ada.characters.handling;



package body et_basic_objects is


	function get_place (
		object	: in type_object)
		return type_vector_model
	is (object.place);



	function get_x (
		object	: in type_object)
		return type_distance
	is (get_x ((get_place (object))));


	procedure set_x (
		object	: in out type_object;
		value	: in type_distance)
	is begin
		object.place.x := value;
	end set_x;


	function get_y (
		object	: in type_object)
		return type_distance
	is (get_y ((get_place (object))));


	procedure set_y (
		object	: in out type_object;
		value	: in type_distance)
	is begin
		object.place.y := value;
	end set_y;




	procedure move_to (
		object	: in out type_object;
		place	: in type_vector_model)
	is begin
		object.place := place;
	end move_to;


	procedure move_by (
		object	: in out type_object;
		offset	: in type_vector_model)
	is begin
		object.place := object.place + offset;
	end move_by;


	procedure reset_position (
		object	: in out type_object)
	is begin
		object.place := origin;
	end reset_position;


	function get_position (
		object	: in type_object;
		format	: in type_output_format := FORMAT_1)
		return string
	is
		x : constant string := to_string (get_x (object.place));
		y : constant string := to_string (get_y (object.place));

		separator : constant string := " / ";
	begin
		case format is
			when FORMAT_1 =>
				return "x/y " & x & separator & y;

			when FORMAT_2 =>
				return "x " & x & " y " & y;

			when FORMAT_3 =>
				return x & space & y;

			when others => -- CS do the same as with FORMAT_1
				return "x/y " & x & separator & y;
		end case;
	end get_position;






	procedure mirror_along_axis (
		object	: in out type_object;
		axis	: in type_mirror)
	is begin
		mirror_point (object.place, axis);
	end mirror_along_axis;


	procedure rotate_about_origin (
		object		: in out type_object;
		rotation	: in type_rotation)
	is begin
		rotate_by (object.place, rotation);
	end rotate_about_origin;




	function is_selected (
		object : in type_object)
		return boolean
	is (is_selected (object.status));


	procedure set_selected (
		object : in out type_object)
	is begin
		set_selected (object.status);
	end set_selected;


	procedure clear_selected (
		object : in out type_object)
	is begin
		clear_selected (object.status);
	end clear_selected;






	function is_proposed (
		object : in type_object)
		return boolean
	is (is_proposed (object.status));


	procedure set_proposed (
		object : in out type_object)
	is begin
		set_proposed (object.status);
	end set_proposed;


	procedure clear_proposed (
		object : in out type_object)
	is begin
		clear_proposed (object.status);
	end clear_proposed;





	function is_moving (
		object : in type_object)
		return boolean
	is (is_moving (object.status));


	procedure set_moving (
		object : in out type_object)
	is begin
		set_moving (object.status);
	end set_moving;


	procedure clear_moving (
		object : in out type_object)
	is begin
		clear_moving (object.status);
	end clear_moving;




	function get_status (
		object	: in type_object)
		return type_object_status
	is (object.status);


	procedure set_status (
		object		: in out type_object;
		status		: in type_object_status)
	is begin
		object.status := status;
	end set_status;


	procedure modify_status (
		object		: in out type_object;
		operation	: in type_status_operation)
	is begin
		modify_status (object.status, operation);
	end modify_status;



	procedure reset_status (
		object		: in out type_object)
	is begin
		reset_status (object.status);
	end reset_status;









	function get_rotation (
		object	: in type_object_with_rotation)
		return type_rotation
	is (object.rotation);


	procedure set_rotation (
		object		: in out type_object_with_rotation;
		rotation	: in type_rotation)
	is begin
		object.rotation := rotation;
	end set_rotation;


	procedure set_rotation_relative (
		object		: in out type_object_with_rotation;
		rotation	: in type_rotation)
	is begin
		object.rotation := add (object.rotation, rotation);
	end set_rotation_relative;


	procedure set_position (
		object	: in out type_object_with_rotation;
		line	: in type_fields_of_line;
		from	: in type_field_count_positive)
	is begin
		null; -- CS
	end set_position;


	procedure reset_position (
		object	: in out type_object_with_rotation)
	is begin
		object.place := origin;
		object.rotation := 0.0;
	end reset_position;



	function get_position (
		object	: in type_object_with_rotation;
		format	: in type_output_format := FORMAT_1)
		return string
	is
		x : constant string := to_string (get_x (object.place));
		y : constant string := to_string (get_y (object.place));
		r : constant string := to_string (object.rotation);

		separator : constant string := " / ";
	begin
		case format is
			when FORMAT_1 =>
				return "x/y/rotation " & x & separator & y & separator & r;

			when FORMAT_2 =>
				return "x " & x & " y " & y & " rotation " & r;

			when FORMAT_3 =>
				return x & space & y & space & r;

			when others => -- CS do the same as with FORMAT_1
				return "x/y/rotation " & x & separator & y & separator & r;
		end case;
	end get_position;




	procedure add (
		object		: in out type_object_with_rotation;
		offset		: in type_vector_model;
		rotation	: in type_rotation;
		mirror		: in type_mirror := MIRROR_NO)
	is begin
		case mirror is
			when MIRROR_NO =>
				rotate_by (object.place, rotation);
				add (object.place, offset);
				add (object.rotation, rotation);

			when MIRROR_ALONG_Y_AXIS =>
				mirror_point (object.place, MIRROR_ALONG_Y_AXIS);
				rotate_by (object.place, -rotation);
				add (object.place, offset);
				add (object.rotation, rotation);

			when others =>
				null; -- CS
		end case;
	end add;



end et_basic_objects;

