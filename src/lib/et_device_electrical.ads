------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICE ELECTRICAL                                --
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


with et_board_coordinates;
with et_device_unit;					use et_device_unit;
with et_device_prefix;					use et_device_prefix;
with et_device_appearance;				use et_device_appearance;
with et_device_purpose;					use et_device_purpose;
with et_device_model_names;				use et_device_model_names;
with et_device_value;					use et_device_value;
with et_device_name;					use et_device_name;
with et_device_partcode;				use et_device_partcode;
with et_device_properties;				use et_device_properties;
with et_device_library;					use et_device_library;
with et_device_library.units;			use et_device_library.units;
with et_device_placeholders.packages;

with et_package_model_name;				use et_package_model_name;
with et_package_variant_name;			use et_package_variant_name;
with et_package_variant;				use et_package_variant;

with et_object_status;					use et_object_status;
with et_unit_name;						use et_unit_name;
with et_device_units;					use et_device_units;
with et_logging;						use et_logging;


package et_device_electrical is

	type type_device_electrical (
		appearance : type_appearance_schematic)
	is private;





	-- Returns true if the given device has a physical
	-- representation in the board drawing:
	function is_real (
		device : in type_device_electrical)
		return boolean;




-- APPEARANCE:

	function get_appearance (
		device	: in type_device_electrical)
		return type_appearance_schematic;


	function get_appearance (
		device	: in type_device_electrical)
		return string;






-- PACKAGE VARIANT:

	procedure set_variant (
		device	: in out type_device_electrical;
		variant	: in type_package_variant_name);


	function get_variant (
		device	: in type_device_electrical)
		return type_package_variant_name;


	function get_variant (
		device	: in type_device_electrical)
		return string;


	-- Returns the name of the package variant of the device.
	-- The device must be a real device.
	-- Otherwise an exception will be raised:
	function get_package_variant (
		device : in type_device_electrical)
		return type_package_variant_name;


	-- Returns a list of available package variants:
	function get_available_package_variants (
		device : in type_device_electrical)
		return pac_package_variants.map;







-- VALUE:

	procedure set_value (
		device	: in out type_device_electrical;
		value	: in type_device_value);


	function get_value (
		device	: in type_device_electrical)
		return type_device_value;


	function get_value (
		device	: in type_device_electrical)
		return string;


	function has_value (
		device	: in type_device_electrical)
		return boolean;



-- PARTCODE:

	procedure set_partcode (
		device		: in out type_device_electrical;
		partcode	: in type_device_partcode);


	function get_partcode (
		device	: in type_device_electrical)
		return type_device_partcode;


	function get_partcode (
		device	: in type_device_electrical)
		return string;


	function has_partcode (
		device	: in type_device_electrical)
		return boolean;





-- PURPOSE:

	procedure set_purpose (
		device	: in out type_device_electrical;
		purpose	: in type_device_purpose);


	function get_purpose (
		device	: in type_device_electrical)
		return type_device_purpose;


	function get_purpose (
		device	: in type_device_electrical)
		return string;


	function has_purpose (
		device	: in type_device_electrical)
		return boolean;




	-- This function copies a given device but
	-- removes all units so that a bare device
	-- with all properties of the original device is
	-- created on return.
	-- If the given device is real, then the status of
	-- the copy will be reset:
	function copy_bare_device (
		device_in : in type_device_electrical)
		return type_device_electrical;







-- DEVICE MODEL:


	function get_device_model_file (
		device : in type_device_electrical)
		return type_device_model_name; -- *.dev
	-- CS rename to get_device_model_name


	function get_device_model_name (
		device : in type_device_electrical)
		return string;



	-- Maps from schematic device to
	-- cursor to device model (in library):
	function get_device_model (
		device	: in type_device_electrical)
		return pac_device_models.cursor;


	procedure set_device_model (
		device	: in out type_device_electrical;
		model	: in pac_device_models.cursor);


	-- Returns the name of the package model of the given device
	-- according to the current package variant of the device.
	-- The package model name is something like "libraries/packages/smd/SOT23.pac"
	-- The given device must be real. Otherwise constraint error arises here.
	function get_package_model_name (
		device : in type_device_electrical)
		return type_package_model_name;







	procedure device_name_in_use (
		name : in type_device_name); -- IC1, R1, ...









-- STATUS:

	-- NOTE: Operations regarding the status
	-- apply to the package of the device (in the board domain)
	-- which implies that the targeted device is real.
	-- Status opertions for individual units (in the schematic)
	-- are specified in the package et_device_units.
	-- Regarding set and clear operations: If the device
	-- is not real then the operation has no effect.
	-- Regarding query operations like is_selected or is_moving: If
	-- the device is not real then the return is always false.

	procedure set_selected (
		device : in out type_device_electrical);


	procedure clear_selected (
		device : in out type_device_electrical);


	function is_selected (
		device : in type_device_electrical)
		return boolean;



	procedure set_proposed (
		device : in out type_device_electrical);


	procedure clear_proposed (
		device : in out type_device_electrical);


	function is_proposed (
		device : in type_device_electrical)
		return boolean;




	procedure set_moving (
		device : in out type_device_electrical);


	procedure clear_moving (
		device : in out type_device_electrical);


	function is_moving (
		device : in type_device_electrical)
		return boolean;




	procedure modify_status (
		device		: in out type_device_electrical;
		operation	: in type_status_operation);



	procedure reset_status (
		device : in out type_device_electrical);



private

	-- This is a device as it appears in the schematic.
	type type_device_electrical (
		appearance : type_appearance_schematic)
	is record
		-- The link to the device model like
		-- "../libraries/devices/transistor/pnp.dev"
		-- is a cursor to the device library:
		model_cursor : pac_device_models.cursor;

		-- IMPORTANT: When reading the module file, the
		-- device model must have been read beforehand.
		-- Otherwise a valid cursor can not be assigned
		-- to the device !

		-- The deployed units like PWR, A, B, ...
		-- Virtual devices have only one unit (like the GND symbol).
		-- Real devices like a single resistor have one unit.
		-- Real devices like FPGAs have many units (like PWR1, PWR2, GPIO1, GPIO2, ...):
		units	: pac_units.map;

		case appearance is
			-- If a device appears in both schematic and layout it has got:
			when APPEARANCE_PCB =>
				-- The package variant:
				variant		: type_package_variant_name; -- D, N

				-- This is layout related. In the layout, the package has a position
				-- and placeholders for name, value and purpose.
				-- The assembly side of a package is by default TOP.
				-- As a result of a flip operation, position.face changes from top to bottom
				-- or vice versa.
				-- Flipping a device to top or bottom means it will be drawn
				-- mirrored along its Y-axis.
				position	: et_board_coordinates.type_board_object;
				-- incl. place, rotation, face and status

				-- Text placeholders in the board drawing
				-- for value, name, purpose, ...
				placeholders	: et_device_placeholders.packages.type_text_placeholders;

				properties	: type_device_properties;


			when APPEARANCE_VIRTUAL => null;
		end case;
	end record;



end et_device_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
