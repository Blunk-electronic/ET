------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICES ELECTRICAL LISTS                         --
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
-- This package is about the type, basic properties and subprograms related
-- to a called "electrical device" as it is modeled in the schematic
-- and in the board.
-- These devices have a representation in schematic and mostly in the board.

with ada.containers;			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_ordered_maps;

with et_device_placeholders.packages;

with et_device_electrical;				use et_device_electrical;
with et_text_content;					use et_text_content;
with et_device_prefix;					use et_device_prefix;

with et_device_value;					use et_device_value;
with et_device_purpose;					use et_device_purpose;
with et_device_partcode;				use et_device_partcode;

with et_device_model_names;				use et_device_model_names;
with et_device_name;					use et_device_name;
with et_device_library;					use et_device_library;

with et_package_model_name;				use et_package_model_name;
with et_package_library;				use et_package_library;
with et_package_variant_name;			use et_package_variant_name;

with et_commit;
with et_object_status;					use et_object_status;


package et_devices_electrical is



	-- The devices of a module are collected in a map.
	-- CS: This must be a hashed map:
	package pac_devices_electrical is new indefinite_ordered_maps (
		key_type		=> type_device_name, -- something like "IC43"
		element_type	=> type_device_electrical);

	use pac_devices_electrical;



	-- Extracts from the given list of devices the
	-- names (like IC1, R23, D23):
	function get_device_names (
		devices : in pac_devices_electrical.map)
		return pac_device_names.set;



	-- Returns the number of devices that the
	-- given list contains:
	function get_count (
		devices	: in pac_devices_electrical.map)
		return natural;


	function get_count (
		devices	: in pac_devices_electrical.map)
		return string;




	function get_device_model_file (
		device : pac_devices_electrical.cursor)
		return type_device_model_name; -- *.dev



	-- Maps from schematic device cursor to
	-- cursor to device model (in library):
	function get_device_model (
		device : in pac_devices_electrical.cursor)
		return pac_device_models.cursor;






	function get_device_name (
		device : in pac_devices_electrical.cursor)
		return type_device_name;


	function get_prefix (
		device : in pac_devices_electrical.cursor)
		return type_device_prefix;



	-- Returns the name of a device as string:
	function get_device_name (
		device : in pac_devices_electrical.cursor)
		return string;








	-- Returns true if given device is real (means if it has a physical
	-- counterpart in the PCB layout). For a resistor it returns true.
	-- For a GND symbol it returns false:
	function is_real (
		device : in pac_devices_electrical.cursor)
		return boolean;





-- VALUE:

	-- Returns the value of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_value (
		device : in pac_devices_electrical.cursor)
		return type_device_value;



-- PARTCODE:

	-- Returns the partcode of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_partcode (
		device : in pac_devices_electrical.cursor)
		return type_device_partcode;



-- PURPOSE:

	-- Returns the purpose of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_purpose (
		device : in pac_devices_electrical.cursor)
		return type_device_purpose;






-- STATUS:

	function is_proposed (
		device : in pac_devices_electrical.cursor)
		return boolean;


	function is_selected (
		device : in pac_devices_electrical.cursor)
		return boolean;


	function is_moving (
		device : in pac_devices_electrical.cursor)
		return boolean;



	-- Iterates the devices. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		devices	: in pac_devices_electrical.map;
		process	: not null access procedure (position : in pac_devices_electrical.cursor);
		proceed	: not null access boolean);




-- PACKAGE VARIANT:

	function get_variant (
		device : in pac_devices_electrical.cursor)
		return type_package_variant_name;



-- PACKAGE MODEL:


	-- Returns the name of the package model of the given device
	-- according to the current package variant of the device.
	-- The package model name is something like "libraries/packages/smd/SOT23.pac"
	-- The given device must be real. Otherwise constraint error arises here.
	function get_package_model_name (
		device : in pac_devices_electrical.cursor)
		return type_package_model_name;


	-- Returns the cursor to the package model of the given device
	-- according to the current package variant of the device.
	-- The given device must be real. Otherwise constraint error arises here.
	function get_package_model (
		device : in pac_devices_electrical.cursor)
		return pac_package_models.cursor;



-- BOM:

	-- Returns true if the given device has a
	-- a real package with a height, means if it is relevant
	-- for creating bill of materials (BOM).
	-- The given device itself must be real (means it has a counterpart
	-- in the board drawing). Otherwise a constraint error is raised.
	function is_bom_relevant (
		device : in pac_devices_electrical.cursor)
		return boolean;





-- PLACEHOLDERS:

	-- Maps from meaning of given placeholder to a text content:
	function to_placeholder_content (
		device_cursor	: in pac_devices_electrical.cursor; -- electrical device
		placeholder		: in et_device_placeholders.packages.type_text_placeholder)
		return type_text_content;






-- COMMITS OF ELECTRICAL DEVICES (required for undo/redo operations via the GUI):

	use et_commit;

	package pac_device_commit is new pac_commit (pac_devices_electrical.map);
	use pac_device_commit;

	package pac_device_commits is new doubly_linked_lists (
		element_type	=> pac_device_commit.type_commit);

	type type_devices_undo_redo_stack is record
		dos		: pac_device_commits.list;
		redos	: pac_device_commits.list;
	end record;






end et_devices_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
