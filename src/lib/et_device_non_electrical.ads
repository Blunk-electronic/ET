------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         DEVICE NON-ELECTRICAL                            --
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
--   to do:
--
--
-- DESCRIPTION:
--
-- This package is about the type, basic properties and subprograms related
-- to a so called "non-electrical device" as it is modeled in the board.
-- These devices have a representation in the board domain only.


with et_pcb_sides;						use et_pcb_sides;
with et_board_geometry;					use et_board_geometry;
with et_board_coordinates;				use et_board_coordinates;
with et_coordinates_formatting;			use et_coordinates_formatting;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;
with et_conductors_floating_package;	use et_conductors_floating_package;
with et_package_library;				use et_package_library;
with et_package_model_name;				use et_package_model_name;
with et_device_name;					use et_device_name;
with et_device_prefix;					use et_device_prefix;
with et_device_value;					use et_device_value;
with et_device_purpose;					use et_device_purpose;
with et_device_partcode;				use et_device_partcode;
with et_device_property_level;			use et_device_property_level;
with et_pcb_signal_layers;				use et_pcb_signal_layers;
with et_mirroring;						use et_mirroring;
with et_coordinates_abs_rel;			use et_coordinates_abs_rel;


package et_device_non_electrical is

	use pac_geometry_2;


	procedure device_name_in_use (
		name : in type_device_name); -- FD1, MH1, ...



	-- In this world, if a package is flipped, then it is
	-- mirrored along the Y-axis.
	-- This function maps from flip status to mirror along y-axis.
	-- If flipped is false, then the return is MIRRROR_NO.
	-- If flipped is true, then the return is MIRROR_ALONG_Y_AXIS:
	function to_mirror_along_y_axis ( -- CS remove ?
		flipped : in type_flipped)
		return type_mirror;




	-- Devices which do not have a counterpart in the schematic
	-- (like fiducials, mounting holes, ...). They can have
	-- terminals. But the terminals are not connected with any net.
	-- They have names like H1 (hole) or FD (fiducial).
	-- Despite not having a counterpart in the schematic, these devices
	-- are modeled via package models:

	type type_device_non_electrical is new  -- CS should be private
		type_board_object
	with record
		-- Text placeholders for value, name, purpose, ...
		placeholders : type_text_placeholders;

		-- The link to the package model like "../lbr/packages/fiducial.pac":
		model_cursor : pac_package_models.cursor;

		-- IMPORTANT: When reading the module file, the
		-- package libraries must have been read beforehand.
		-- Otherwise a valid cursor can not be assigned
		-- to the device !

		-- A value will rarely be assigned. But in case it is required:
		value : type_device_value;

		-- The partcode:
		partcode : type_device_partcode;

		-- A purpose will rarely be assigned. But in case it is required:
		purpose : type_device_purpose;
	end record;



	function get_position (
		device : in type_device_non_electrical)
		return type_board_object;



	-- Returns the name of the package model file
	-- like "../lbr/packages/fiducial.pac"
	function get_package_model_name (
		device	: in type_device_non_electrical)
		return type_package_model_name;


	function get_package_model_name (
		device : in type_device_non_electrical)
		return string;




	-- procedure set_rotation_relative (
	-- 	device		: in out type_device_non_electrical;
	-- 	rotation	: in type_rotation_model);




-- VALUE:

	procedure set_value (
		device	: in out type_device_non_electrical;
		value	: in type_device_value);


	function get_value (
		device	: in type_device_non_electrical)
		return type_device_value;


	function get_value (
		device	: in type_device_non_electrical)
		return string;


	function has_value (
		device	: in type_device_non_electrical)
		return boolean;



-- PARTCODE:

	procedure set_partcode (
		device		: in out type_device_non_electrical;
		partcode	: in type_device_partcode);


	function get_partcode (
		device	: in type_device_non_electrical)
		return type_device_partcode;


	function get_partcode (
		device	: in type_device_non_electrical)
		return string;


	function has_partcode (
		device	: in type_device_non_electrical)
		return boolean;



-- PURPOSE:

	procedure set_purpose (
		device	: in out type_device_non_electrical;
		purpose	: in type_device_purpose);


	function get_purpose (
		device	: in type_device_non_electrical)
		return type_device_purpose;


	function get_purpose (
		device	: in type_device_non_electrical)
		return string;


	function has_purpose (
		device	: in type_device_non_electrical)
		return boolean;




-- BOM:

	-- Returns true if the given device has a
	-- a real package with a height, means if it is relevant
	-- for creating bill of materials (BOM):
	function is_bom_relevant (
		device : in type_device_non_electrical)
		return boolean;









-- PLACEHOLDERS:

	-- Resets the positions of placeholders back to the
	-- defaults as specified in the package model:
	procedure reset_placeholder_positions (
		device		: in out type_device_non_electrical);



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
		device		: in out type_device_non_electrical;
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
		device		: in out type_device_non_electrical;
		meaning		: in type_placeholder_meaning;
		layer		: in type_placeholder_layer; -- silkscreen, assy doc
		face		: in type_face;
		index		: in type_placeholder_index; -- 1, 2, 3, ...
		coordinates	: in type_coordinates;
		rotation	: in type_rotation_model);





-- PROPERTIES QUERIES:


	-- Returns properties of the given device.
	-- 1. Level determines the degree and amount of information to be returned.
	-- 2. If linebreaks is true, then linebreaks are inserted.
	--    This is useful when the output is to be displayed
	--    in a window or if it is to be written in a file:
	function get_device_properties (
		device		: in type_device_non_electrical;
		level		: in type_properties_level;
		linebreaks	: in boolean := false)
		return string;



end et_device_non_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
