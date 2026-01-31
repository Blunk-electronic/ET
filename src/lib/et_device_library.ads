------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           DEVICE LIBRARY                                 --
--                                                                          --
--                              S p e c                                     --
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
-- DESCRIPTION:
--
-- This package describes the structure of
-- the rig-wide library of devices models.
-- Child packages describe units and packages.
--
-- ToDo:
--
--

with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_ordered_maps;

with et_logging;				use et_logging;
with et_terminals;				use et_terminals;
with et_device_appearance;		use et_device_appearance;
with et_device_model_names;		use et_device_model_names;
with et_device_value;			use et_device_value;
with et_device_prefix;			use et_device_prefix;
with et_device_name;			use et_device_name;
with et_device_model;			use et_device_model;



package et_device_library is

	
	-- CS: experimental stuff in order to improve
	-- preformance of the device library using a hashed map:
	
	--function hash_device_model (
		--model	: in pac_device_model_file.bounded_string)
		--return hash_type;

	--function equivalent_models (
		--d1, d2 : in type_device_model)
		--return boolean;

	--subtype type_t is type_device_model (PCB);
	
	--package pac_devices_lib2 is new hashed_maps (
		--key_type 		=> pac_device_model_file.bounded_string, -- ../libraries/devices/logic_ttl/7400.dev
		--element_type	=> type_t,
		--hash			=> hash_device_model,
		--equivalent_keys	=> pac_device_model_file."=",
		--"="				=> equivalent_models);

	-- https://github.com/PiEqThree/Ada_Hash_Map/blob/main/main.adb




	
	

	-- Device models are are stored in files ending with *.dev.
	-- At the same time a
	-- device name (like "libraries/devices/7400.dev")
	-- is also the key to the device library:
	
	package pac_device_models is new indefinite_ordered_maps (
		key_type 		=> pac_device_model_file.bounded_string,
		"<"				=> pac_device_model_file."<",
		element_type	=> type_device_model);

	use pac_device_models;


	

	-- Returns the name prefix for a given device cursor:
	function get_prefix (
		cursor	: in pac_device_models.cursor)
		return pac_device_prefix.bounded_string;

		
	-- Returns the name of the device model for 
	-- a given device cursor:
	function get_device_model_file (
		cursor	: in pac_device_models.cursor)
		return pac_device_model_file.bounded_string;
	-- CS remove. see function get_device_model_name below
	-- which does the same

	

	-- THIS IS THE RIG WIDE LIBRARY OF ELECTRICAL DEVICES:
	
	device_library : pac_device_models.map;

	



	
	-- Creates a device and stores it in device library:
	procedure create_device (
		device_name		: in pac_device_model_file.bounded_string;
		appearance		: in type_appearance;
		log_threshold	: in type_log_level);


	

	-- Returns for a given device model file name
	-- (like ../libraries/devices/transistor/pnp.dev)
	-- the device model in the device library:
	function get_device_model (
		model : in pac_device_model_file.bounded_string)
		return pac_device_models.cursor;



	function get_device_model_name (
		device_cursor : in pac_device_models.cursor)
		return pac_device_model_file.bounded_string;
 
 
	function get_device_model_name (
		device_cursor : in pac_device_models.cursor)
		return string;


	
	
	-- Returns true if the given device has a physical counterpart in 
	-- the layout, means if it is not virtual:
	function is_real (
		device_cursor : in pac_device_models.cursor)
		return boolean;

	



	-- Returns the default value as it is 
	-- specified in the device model.
	-- If the device is virtual (like a GND symbol) or if
	-- no value is predifined in the model, then an empty
	-- string will be returned:
	function get_default_value (
		device_cursor : in pac_device_models.cursor)
		return pac_device_value.bounded_string;


	
end et_device_library;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
