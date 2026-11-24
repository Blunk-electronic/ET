------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DEVICE ELECTRICAL / PACKAGE                         --
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
-- To Do:
-- 1. device accessories
-- 2. see comments on package pac_devices_electrical
--
--
-- DESCRIPTION:
--
-- The subprograms in this package address the package
-- of a device. So the targeted device must be real.
-- Otherwise exceptions wlll be raised.
-- 1. Here it is about coordinates, location vectors
--    places, angles and rotation as they are used in
--    the board domain exclusively.
-- 2. The package of an electrical device is not derived
--    from the generic type_board_object. However, the subprograms
--    here are named by the same scheme as for the type_board_object
--    (see package et_basic_objects).
-- 3. Some subprograms are prepared and put in comments. For details
--    see package et_basic_objects.


with et_coordinates_formatting;			use et_coordinates_formatting;
with et_coordinates_abs_rel;			use et_coordinates_abs_rel;

with et_board_geometry;					use et_board_geometry;
with et_board_coordinates;				use et_board_coordinates;
with et_pcb_sides;						use et_pcb_sides;
-- with et_device_appearance;				use et_device_appearance;

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;



package et_device_electrical.packge is
	-- NOTE: This child package has been named intentionally
	-- "packge". This is not a typing error. The name "packge" has
	-- been choosen in order to avoid clashes with the Ada compiler
	-- as "package" is a reserved keyword.

	use pac_geometry_2;






-- POSITION:

	procedure set_place (
		device	: in out type_device_electrical;
		place	: in type_vector_model);


	-- CS:
	-- set_x/y ?
	-- get_x/y ?


	-- Moves the device by the given offset:
	procedure move_by (
		device	: in out type_device_electrical;
		offset	: in type_vector_model);


	function get_place (
		device	: in type_device_electrical)
		return type_vector_model;


	function get_place (
		device	: in type_device_electrical;
		format	: in type_output_format := FORMAT_1)
		return string;





	-- Sets the position of the package in the board drawing.
	procedure set_position (
		device		: in out type_device_electrical;
		position	: in type_board_object);


	-- CS:
	-- Reads a line like "position x 44.5 y 53.5"
	-- and assigns the position to the object:
	-- procedure set_position (
	-- 	device	: in out type_device_electrical;
	-- 	line	: in type_fields_of_line;
	-- 	from	: in type_field_count_positive);


	-- CS: reset_position ?


	-- Returns the position of the package in the board drawing.
	function get_position (
		device : in type_device_electrical)
		return type_board_object;


	-- Returns the position of the package in the board drawing.
	function get_position (
		device	: in type_device_electrical;
		format	: in type_output_format := FORMAT_1)
		return string;


	function get_rotation (
		device	: in type_device_electrical)
		return type_rotation_model;



	procedure set_rotation (
		device		: in out type_device_electrical;
		rotation	: in type_rotation_model);


	procedure rotate_to (
		device		: in out type_device_electrical;
		rotation	: in type_rotation_model)
		renames set_rotation;



	procedure set_rotation_relative (
		device		: in out type_device_electrical;
		rotation	: in type_rotation_model);


	procedure rotate_by (
		device		: in out type_device_electrical;
		rotation	: in type_rotation_model)
		renames set_rotation_relative;


	-- CS:
	-- Rotates the device about the origin
	-- by the given angle:
	-- procedure rotate_about_origin (
	-- 	device		: in out type_device_electrical;
	-- 	rotation	: in type_rotation_model);




-- MIRROR:

	-- CS: mirror_along_axis ?


	-- CS
	-- procedure add (
	-- 	device		: in out type_device_electrical;
	-- 	offset		: in type_vector_model;
	-- 	rotation	: in type_rotation;
	-- 	mirror		: in type_mirror := MIRROR_NO);




-- FACE:

	procedure set_face (
		device	: in out type_device_electrical;
		face	: in type_face);


	procedure toggle_face (
		device	: in out type_device_electrical);


	function get_face (
		device	: in type_device_electrical)
		return type_face;


	function get_face (
		device	: in type_device_electrical)
		return string;




-- AREA:

	-- Returns true if the given device is in the
	-- given area.
	-- The device must be real. Otherwise an exception
	-- will be raised:
	function in_area (
		device	: in type_device_electrical;
		area	: in type_area)
		return boolean;


	-- Returns true if the given device meets the
	-- following criteria:
	-- 1. is real (means it has a representation in the board)
	-- 2. is mounted on the given face
	-- 3. is in the given area
	function on_face_and_in_area (
		device	: in type_device_electrical;
		face	: in type_face;
		area	: in type_area)
		return boolean;






-- PLACEHOLDERS:


	procedure set_placeholders (
		device			: in out type_device_electrical;
		placeholders	: in type_text_placeholders);


	function get_placeholders_silkscreen (
		device	: in type_device_electrical;
		face	: in type_face)
		return pac_text_placeholders.vector;


	function get_placeholders_assy_doc (
		device	: in type_device_electrical;
		face	: in type_face)
		return pac_text_placeholders.vector;


	function get_placeholders (
		device			: in type_device_electrical)
		return type_text_placeholders;


	-- Resets the positions of placeholders back to the
	-- defaults as specified in the package model:
	procedure reset_placeholder_positions (
		device : in out type_device_electrical);


	procedure iterate_placeholders_silkscreen (
		device	: in type_device_electrical;
		face	: in type_face;
		process	: not null access procedure (position : in pac_text_placeholders.cursor));


	procedure iterate_placeholders_assy_doc (
		device	: in type_device_electrical;
		face	: in type_face;
		process	: not null access procedure (position : in pac_text_placeholders.cursor));



	-- Moves the placeholder given by meaning, layer, face and index.
	-- NOTE: Index identifies the targeted placeholder in connection
	--       with its meaning. For example, if meaning is "value" and index is 3
	--       then the 3rd value placeholder is adressed.
	--
	-- If no matching placeholder has been found, then nothing happens.
	-- CS; An error flag output by this procedure could be useful.
	--
	-- If coordinates is absolute, then the affected placeholder
	-- is moved to the given point and its anchor mode set accordingly.
	-- If coordinates is relative, then the affected placeholder
	-- is moved by the x/y-distance given by point and its anchor mode
	-- set accordingly:
	procedure move_placeholder (
		device		: in out type_device_electrical;
		meaning		: in type_placeholder_meaning;
		layer		: in type_placeholder_layer; -- silkscreen, assy doc
		face		: in type_face;
		index		: in type_placeholder_index; -- 1, 2, 3, ...
		coordinates	: in type_coordinates; -- relative/absolute
		point		: in type_vector_model); -- x/y
		-- CS rename to destination_offset rework documentation above



	-- Rotates the placeholder given by meaning, layer, face and index.
	-- NOTE: Index identifies the targeted placeholder in connection
	--       with its meaning. For example, if meaning is "value" and index is 3
	--       then the 3rd value placeholder is adressed.
	--
	-- If no matching placeholder has been found, then nothing happens.
	-- CS; An error flag output by this procedure could be useful.
	--
	-- If coordinates is absolute, then the placeholder
	-- is rotated to the given rotation.
	-- If coordinates is relative, then the placeholder
	-- is rotated by the given rotation:
	procedure rotate_placeholder (
		device		: in out type_device_electrical;
		meaning		: in type_placeholder_meaning;
		layer		: in type_placeholder_layer; -- silkscreen, assy doc
		face		: in type_face;
		index		: in type_placeholder_index; -- 1, 2, 3, ...
		coordinates	: in type_coordinates;
		rotation	: in type_rotation_model);



	-- Sets the proposed-flag of all placeholders which are in the
	-- given zone around the given place.
	-- Since the placeholders have a position that is relative to
	-- the parent package, the absolute package position must also
	-- be provided.
	-- Adds to count the number of placeholders that have been found:
	procedure propose_placeholders (
		device				: in out type_device_electrical;
		package_position	: in type_board_object; -- CS: no need. already provided in device
		catch_zone			: in type_catch_zone;
		count				: in out natural;
		log_threshold		: in type_log_level);



	-- Modifies the status of a placeholder among a given
	-- list of placeholders. The placeholder is identified by
	-- the layer, face and the cursor to the placeholder:
	procedure modify_status (
		device				: in out type_device_electrical;
		layer				: in type_placeholder_layer;
		face				: in type_face;
		placeholder_cursor	: in pac_text_placeholders.cursor;
		operation			: in type_status_operation);



	-- Resets the status of all placeholders:
	procedure reset_placeholder_status (
		device : in out type_device_electrical);


end et_device_electrical.packge;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
