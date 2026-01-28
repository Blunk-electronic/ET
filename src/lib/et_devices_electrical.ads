------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICES ELECTRICAL                               --
--                                                                          --
--                               S p e c                                    --
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
--   ToDo: 
--		1. device accessories
-- 
-- DESCRIPTION:
-- 
-- This package is about the type, basic properties and subprograms related
-- to so called "electrical" devices as they are modelled in the schematic
-- and in the board.
-- These devices have a representation in schematic and mostly in the board.

with ada.containers;           			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_ordered_maps;

with et_assembly_variants;				use et_assembly_variants;
with et_assembly_variant_name;			use et_assembly_variant_name;

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;

with et_board_coordinates;
with et_text_content;					use et_text_content;
with et_device_prefix;					use et_device_prefix;
with et_device_model;					use et_device_model;
with et_device_appearance;				use et_device_appearance;
with et_device_purpose;					use et_device_purpose;
with et_device_model_names;				use et_device_model_names;
with et_device_value;					use et_device_value;
with et_device_name;					use et_device_name;
with et_device_partcode;				use et_device_partcode;
with et_device_library;					use et_device_library;
with et_package_name;					use et_package_name;
with et_package_model_name;				use et_package_model_name;
with et_package_variant;				use et_package_variant;
with et_package_model;					use et_package_model;
with et_package_library;				use et_package_library;
with et_commit;
with et_object_status;					use et_object_status;
with et_units;							use et_units;
with et_logging;						use et_logging;


package et_devices_electrical is


	

	-- This is a device as it appears in the schematic.
	type type_device_electrical (  -- CS should be private
		appearance : type_appearance_schematic) 
	is record

		-- The link to the device model like 
		-- "../libraries/devices/transistor/pnp.dev":
		model_name : pac_device_model_file.bounded_string;
		-- CS rename to model_name
		
		-- model_cursor : pac_device_models.cursor;
		-- CS use a cursor to the model instead ?

		-- The deployed units like PWR, A, B, ...
		-- Virtual devices have only one unit (like the GND symbol).
		-- Real devices like a single resistor have one unit.
		-- Real devices like FPGAs have many units (like PWR1, PWR2, GPIO1, GPIO2, ...):
		units	: pac_units.map;
		
		case appearance is
			-- If a device appears in both schematic and layout it has got:
			when APPEARANCE_PCB =>
				value		: pac_device_value.bounded_string; -- 470R
				
				partcode	: pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
				-- For virtual packages (test points, edge connectors, ...)
				-- usually no partcode is required.

				-- The purpose indicates what the device is doing.
				-- It is usually required for devices that require interaction
				-- with the user of a PCBA:
				purpose		: pac_device_purpose.bounded_string; -- brightness_control

				-- The package variant:
				variant		: pac_package_variant_name.bounded_string; -- D, N

				-- This is layout related. In the layout, the package has a position
				-- and placeholders for name, value and purpose.
				-- The assembly side of a package is by default TOP.
				-- As a result of a flip operation, position.face changes from top to bottom
				-- or vice versa.
				-- Flipping a device to top or bottom means it will be drawn
				-- mirrored along its Y-axis.
				position	: et_board_coordinates.type_package_position; -- incl. rotation and face

				-- Text placeholders for value, name, purpose, ...
				placeholders	: et_device_placeholders.packages.type_text_placeholders;

				-- The status of the package:
				status : type_object_status;
				
			when APPEARANCE_VIRTUAL => null;

		end case;
	end record;


	-- Returns true if the given device has a physical
	-- representation in the board drawing:
	function is_real (
		device : in type_device_electrical)
		return boolean;
	

	
	function get_position (
		device : in type_device_electrical)
		return et_board_coordinates.type_package_position;

	

	function get_device_model_file (
		device : type_device_electrical)
		return pac_device_model_file.bounded_string; -- *.dev
	-- CS rename to get_device_model_name

	

	-- Maps from schematic device to 
	-- cursor to device model (in library):
	function get_device_model (
		device : in type_device_electrical)
		return pac_device_models.cursor;



												


	-- Returns the name of the package model of the given device
	-- according to the current package variant of the device.
	-- The package model name is something like "libraries/packages/smd/SOT23.pac"
	-- The given device must be real. Otherwise constraint error arises here.	
	function get_package_model_name (
		device : in type_device_electrical)
		return pac_package_model_file.bounded_string;

	


	
	
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
		return pac_device_model_file.bounded_string; -- *.dev



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
		return pac_device_prefix.bounded_string;


	
	-- Returns the name of a device as string:
	function get_device_name (
		device : in pac_devices_electrical.cursor)
		return string;
	

	
	
	procedure device_name_in_use (
		name : in type_device_name); -- IC1, R1, ...






	
	
-- DEVICE STATUS OPERATIONS:
	
	-- NOTE: Operations regarding the status
	-- apply to the package of the device (in the board domain)
	-- which implies that the targeted device is real.
	-- Status opertions for individual units (in the schematic)
	-- are specified in the package et_units.
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




	-- Returns true if given device is real (means if it has a physical 
	-- counterpart in the PCB layout). For a resistor it returns true.
	-- For a GND symbol it returns false:
	function is_real (
		device : in pac_devices_electrical.cursor) 
		return boolean;
	


	
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

	

	
-- DEVICE QUERY OPERATIONS:




	-- Returns the name of the package model of the given device
	-- according to the current package variant of the device.
	-- The package model name is something like "libraries/packages/smd/SOT23.pac"
	-- The given device must be real. Otherwise constraint error arises here.	
	function get_package_model_name (
		device : in pac_devices_electrical.cursor)
		return pac_package_model_file.bounded_string;
	

	-- Returns the cursor to the package model of the given device
	-- according to the current package variant of the device.
	-- The given device must be real. Otherwise constraint error arises here.	
	function get_package_model (
		device : in pac_devices_electrical.cursor)
		return pac_package_models.cursor;


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
		return pac_text_content.bounded_string;



	
	
	
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
