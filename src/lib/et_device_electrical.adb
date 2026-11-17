------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICE ELECTRICAL                                --
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
-- <http://www.gnu.org/licenses/>.
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


-- with ada.text_io;			use ada.text_io;
with et_string_processing;			use et_string_processing;

with et_device_library.packages;



package body et_device_electrical is


	function is_real (
		device : in type_device_electrical)
		return boolean
	is (device.appearance = APPEARANCE_PCB);




-- APPEARANCE:

	function get_appearance (
		device	: in type_device_electrical)
		return type_appearance_schematic
	is (device.appearance);


	function get_appearance (
		device	: in type_device_electrical)
		return string
	is (to_string (get_appearance (device)));





-- UNITS:

	procedure set_units (
		device	: in out type_device_electrical;
		units	: in pac_units.map)
	is begin
		device.units := units;
	end set_units;


	function get_units (
		device	: in type_device_electrical)
		return pac_units.map
	is (device.units);






-- POSITION (in board):

	function get_position (
		device : in type_device_electrical)
		return et_board_coordinates.type_board_object
	is (device.position);





-- VARIANT:

	procedure set_variant (
		device	: in out type_device_electrical;
		variant	: in type_package_variant_name)
	is begin
		device.variant := variant;
	end set_variant;


	function get_variant (
		device	: in type_device_electrical)
		return type_package_variant_name
	is (device.variant);


	function get_variant (
		device	: in type_device_electrical)
		return string
	is (to_string (get_variant (device)));




-- VALUE:

	procedure set_value (
		device	: in out type_device_electrical;
		value	: in type_device_value)
	is begin
		set_value (device.properties, value);
	end set_value;


	function get_value (
		device	: in type_device_electrical)
		return type_device_value
	is (get_value (device.properties));


	function get_value (
		device	: in type_device_electrical)
		return string
	is (to_string (get_value (device)));


	function has_value (
		device	: in type_device_electrical)
		return boolean
	is begin
		if is_empty (get_value (device)) then
			return false;
		else
			return true;
		end if;
	end has_value;





-- PARTCODE:

	procedure set_partcode (
		device		: in out type_device_electrical;
		partcode	: in type_device_partcode)
	is begin
		set_partcode (device.properties, partcode);
	end set_partcode;


	function get_partcode (
		device	: in type_device_electrical)
		return type_device_partcode
	is (get_partcode (device.properties));


	function get_partcode (
		device	: in type_device_electrical)
		return string
	is (to_string (get_partcode (device)));


	function has_partcode (
		device	: in type_device_electrical)
		return boolean
	is begin
		if is_empty (get_partcode (device)) then
			return false;
		else
			return true;
		end if;
	end has_partcode;






-- PURPOSE:

	procedure set_purpose (
		device	: in out type_device_electrical;
		purpose	: in type_device_purpose)
	is begin
		set_purpose (device.properties, purpose);
	end set_purpose;


	function get_purpose (
		device	: in type_device_electrical)
		return type_device_purpose
	is (get_purpose (device.properties));


	function get_purpose (
		device	: in type_device_electrical)
		return string
	is (to_string (get_purpose (device)));



	function has_purpose (
		device	: in type_device_electrical)
		return boolean
	is begin
		if is_empty (get_purpose (device)) then
			return false;
		else
			return true;
		end if;
	end has_purpose;








	function copy_bare_device (
		device_in : in type_device_electrical)
		return type_device_electrical
	is
		device_out : type_device_electrical := device_in;
	begin
		-- Delete already deployed units:
		device_out.units.clear;

		-- If the given device is real, then reset
		-- the status flags of the copy:
		if is_real (device_in) then
			reset_status (device_out.position.status);
		end if;

		return device_out;
	end copy_bare_device;






-- DEVICE MODEL:

	function get_device_model_file (
		device : in type_device_electrical)
		return type_device_model_name
	is
		use pac_device_models;
	begin
		return key (device.model_cursor);
	end get_device_model_file;




	function get_device_model_name (
		device : in type_device_electrical)
		return string
	is
		use pac_device_models;
	begin
		return to_string (key (device.model_cursor));
	end get_device_model_name;



	procedure set_device_model (
		device	: in out type_device_electrical;
		model	: in pac_device_models.cursor)
	is begin
		device.model_cursor := model;
	end set_device_model;




	function get_device_model (
		device : in type_device_electrical)
		return pac_device_models.cursor
	is
		use et_device_model_names;
		model_file : type_device_model_name;
	begin
		-- The name of the device model file is THE link
		-- from device in schematic to device in library:
		model_file := get_device_model_file (device);

		-- Locate the device model in the library:
		return get_device_model (model_file);
	end get_device_model;




	function get_package_model_name (
		device : in type_device_electrical)
		return type_package_model_name
	is
		use et_device_library.packages;
	begin
		return get_package_model (device.model_cursor, device.variant);
	end get_package_model_name;







	procedure device_name_in_use (
		name : in type_device_name)
	is
		use et_logging;
	begin
		log (SEVERITY_ERROR, "Name " & enclose_in_quotes (to_string (name)) &
			 " already used by another electrical device !",
			 console => true);

		raise constraint_error;
	end device_name_in_use;







-- STATUS:


	procedure set_selected (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			set_selected (device.position.status);
		end if;
	end set_selected;




	procedure clear_selected (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			clear_selected (device.position.status);
		end if;
	end clear_selected;



	function is_selected (
		device : in type_device_electrical)
		return boolean
	is begin
		if is_real (device) then
			if is_selected (device.position.status) then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end is_selected;




	procedure set_proposed (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			set_proposed (device.position.status);
		end if;
	end set_proposed;



	procedure clear_proposed (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			clear_proposed (device.position.status);
		end if;
	end clear_proposed;



	function is_proposed (
		device : in type_device_electrical)
		return boolean
	is begin
		if is_real (device) then
			if is_proposed (device.position.status) then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end is_proposed;



	procedure set_moving (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			set_moving (device.position.status);
		end if;
	end set_moving;


	procedure clear_moving (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			clear_moving (device.position.status);
		end if;
	end clear_moving;



	function is_moving (
		device : in type_device_electrical)
		return boolean
	is begin
		if is_real (device) then
			if is_moving (device.position.status) then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end is_moving;







	procedure modify_status (
		device		: in out type_device_electrical;
		operation	: in type_status_operation)
	is begin
		if is_real (device) then
			modify_status (device.position.status, operation);
		end if;
	end modify_status;




	procedure reset_status (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			reset_status (device.position.status);
		end if;
	end reset_status;




end et_device_electrical;


-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
