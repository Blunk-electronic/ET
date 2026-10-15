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
-- with et_coordinates_formatting;		use et_coordinates_formatting;


generic
	with package pac_geometry is new et_geometry_2a (<>);


package et_basic_objects is
	use pac_geometry;


	type type_object is abstract tagged record
		place 	: type_vector_model;
		status	: type_object_status;
	end record;


-- PLACE:

	function get_place (
		object	: in type_object)
		return type_vector_model;


	procedure move_to (
		object	: in out type_object;
		place	: in type_vector_model);

	procedure move_by (
		object	: in out type_object;
		offset	: in type_vector_model);





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






end et_basic_objects;

