------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            BASIC OBJECTS                                 --
--                                                                          --
--                               S p e c                                    --
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

with et_axes;						use et_axes;
with et_geometry_2a;

with et_object_status;				use et_object_status;
with et_mirroring;					use et_mirroring;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_string_processing;			use et_string_processing;



generic
	with package pac_geometry is new et_geometry_2a (<>);


package et_basic_objects is
	use pac_geometry;
	use pac_geometry.pac_geometry_1;


	type type_object is abstract tagged record
		place 	: type_vector_model;
		status	: type_object_status;
	end record;



-- PLACE:

	function get_place (
		object	: in type_object)
		return type_vector_model;



	function get_x (
		object	: in type_object)
		return type_distance;

	procedure set_x (
		object	: in out type_object;
		value	: in type_distance);


	function get_y (
		object	: in type_object)
		return type_distance;

	procedure set_y (
		object	: in out type_object;
		value	: in type_distance);



	procedure move_to (
		object	: in out type_object;
		place	: in type_vector_model);

	procedure set_place (
		object	: in out type_object;
		place	: in type_vector_model)
		renames move_to;


	procedure move_by (
		object	: in out type_object;
		offset	: in type_vector_model);


	procedure reset_position (
		object	: in out type_object);


	-- This function returns the object position
	-- as string formatted as follows:
	-- FORMAT_1 : x/y 4.5 / 5.6
	-- FORMAT_2 : x 4.5 y 5.6
	-- FORMAT_3 : 4.5 5.6
	function get_position (
		object	: in type_object;
		format	: in type_output_format := FORMAT_1)
		return string;


	-- Reads a line like "position x 44.5 y 53.5"
	-- and assigns the position to the object:
	procedure set_position (
		object	: in out type_object;
		line	: in type_fields_of_line;
		from	: in type_field_count_positive);


-- MIRROR:

	-- Mirrors the place along the given axis.
	procedure mirror_along_axis (
		object	: in out type_object;
		axis	: in type_mirror);



-- ROTATION:

	-- Rotates the place about the origin
	-- by the given angle:
	procedure rotate_about_origin (
		object		: in out type_object;
		rotation	: in type_rotation);




-- STATUS FLAGS:

	function is_selected (
		object : in type_object)
		return boolean;

	procedure set_selected (
		object : in out type_object);

	procedure clear_selected (
		object : in out type_object);




	function is_proposed (
		object : in type_object)
		return boolean;

	procedure set_proposed (
		object : in out type_object);

	procedure clear_proposed (
		object : in out type_object);




	function is_moving (
		object : in type_object)
		return boolean;

	procedure set_moving (
		object : in out type_object);

	procedure clear_moving (
		object : in out type_object);





	function get_status (
		object	: in type_object)
		return type_object_status;

	procedure set_status (
		object	: in out type_object;
		status	: in type_object_status);

	procedure modify_status (
		object		: in out type_object;
		operation	: in type_status_operation);

	procedure reset_status (
		object		: in out type_object);





	type type_object_with_rotation is new type_object with record
		rotation	: type_rotation;
	end record;


	function get_rotation (
		object	: in type_object_with_rotation)
		return type_rotation;


	procedure set_rotation (
		object		: in out type_object_with_rotation;
		rotation	: in type_rotation);


	-- Changes the rotation of the object by the given
	-- relative rotation.
	-- Preserves x/y. Changes rotation only.
	procedure set_rotation_relative (
		object		: in out type_object_with_rotation;
		rotation	: in type_rotation);

	procedure rotate_by (
		object		: in out type_object_with_rotation;
		rotation	: in type_rotation)
		renames set_rotation_relative;



	overriding procedure reset_position (
		object	: in out type_object_with_rotation);


	-- This function returns the given object position
	-- as string formatted as follows:
	-- FORMAT_1 : x/y/rotation 4.5 / 5.6 / 90.0
	-- FORMAT_2 : x 4.5 y 5.6 rotation 90.0
	-- FORMAT_3 : 4.5 5.6 90.0
	overriding function get_position (
		object	: in type_object_with_rotation;
		format	: in type_output_format := FORMAT_1)
		return string;


	-- Reads a line like "position x 44.5 y 53.5 rotation 90.0"
	-- and assigns the position to the object:
	overriding procedure set_position (
		object	: in out type_object_with_rotation;
		line	: in type_fields_of_line;
		from	: in type_field_count_positive);


	-- This is more than just adding
	-- location vectors (place) and rotation.
	-- This function does the follwing:
	-- If mirror is MIRROR_NO, then
	--  1. Rotates the place of the object about its
	--     own origin by given rotation.
	--  2. Adds to object.place the given offset.
	--  3. Adds to object.rotation the given rotation.
	--
	-- If mirror is MIRROR_ALONG_Y_AXIS, then
	--  1. Mirrors object.place along Y-axis.
	--  2. Rotates the object.place about its
	--     own origin by the negative rotation.
	--  3. Adds to object.place the given offset.
	--  4. Adds to object.rotation the given rotation.
	--
	-- If mirror is something else, then nothing happens:
	procedure add (
		object		: in out type_object_with_rotation;
		offset		: in type_vector_model;
		rotation	: in type_rotation;
		mirror		: in type_mirror := MIRROR_NO);


end et_basic_objects;

