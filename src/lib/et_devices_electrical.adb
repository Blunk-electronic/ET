------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICES ELECTRICAL                               --
--                                                                          --
--                               B o d y                                    --
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
-- <http://www.gnu.org/licenses/>.   
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

with ada.text_io;					use ada.text_io;
with ada.characters.latin_1;
with ada.characters.handling;
with ada.exceptions;

with et_string_processing;			use et_string_processing;



package body et_devices_electrical is


	function is_real (
		device : in type_device_electrical)
		return boolean
	is begin
		if device.appearance = APPEARANCE_PCB then
			return true;
		else
			return false;
		end if;
	end;


	
	

	function get_device_model_file (
		device : type_device_electrical)
		return pac_device_model_file.bounded_string
	is begin
		return device.model;
	end get_device_model_file;


	

	function get_device_model (
		device : in type_device_electrical)
		return pac_devices_lib.cursor
	is
		use et_device_model_names;
		model_file : pac_device_model_file.bounded_string;
	begin
		-- The name of the device model file is THE link
		-- from device in schematic to device in library:
		model_file := get_device_model_file (device);

		-- Locate the device model in the library:
		return get_device_model_cursor (model_file);
	end get_device_model;

	


	



	function get_package_model_name (
		device : in type_device_electrical)
		return pac_package_model_file_name.bounded_string
	is
		device_cursor_lib : pac_devices_lib.cursor;
	begin
		-- Locate the generic device model in the device library
		device_cursor_lib := get_device_model_cursor (device.model);
		
		return get_package_model (device_cursor_lib, device.variant);
	end get_package_model_name;


	


	
	

	function get_device_model_file (
		device : pac_devices_electrical.cursor)
		return pac_device_model_file.bounded_string
	is begin
		return get_device_model_file (element (device));
	end get_device_model_file;

	


	function get_device_model (
		device : in pac_devices_electrical.cursor)
		return pac_devices_lib.cursor
	is
		use et_device_model_names;
		model_file : pac_device_model_file.bounded_string;
	begin
		-- The name of the device model file is THE link
		-- from device in schematic to device in library:
		model_file := get_device_model_file (device);

		-- Locate the device model in the library:
		return get_device_model_cursor (model_file);
	end get_device_model;


	



	

	

	

	function get_device_name (
		device : in pac_devices_electrical.cursor)
		return type_device_name
	is begin
		return key (device);
	end get_device_name;


	

	function get_device_name (
		device : in pac_devices_electrical.cursor)
		return string
	is begin
		return to_string (key (device));
	end get_device_name;




	function get_prefix (
		device : in pac_devices_electrical.cursor)
		return pac_device_prefix.bounded_string
	is begin
		return get_prefix (key (device));
	end;




	
	procedure device_name_in_use (
		name : in type_device_name)
	is 
		use et_logging;
	begin
		log (ERROR, "Name " & enclose_in_quotes (to_string (name)) &
			 " already used by another electrical device !",
			 console => true);
		
		raise constraint_error;
	end device_name_in_use;


	

	

	

	
	
	procedure set_selected (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			set_selected (device.status);
		end if;
	end;


	
	
	procedure clear_selected (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			clear_selected (device.status);
		end if;
	end;


	
	function is_selected (
		device : in type_device_electrical)
		return boolean
	is begin
		if is_real (device) then
			if is_selected (device.status) then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end;	
	

	
	
	procedure set_proposed (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			set_proposed (device.status);
		end if;
	end;
	

	
	procedure clear_proposed (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			clear_proposed (device.status);
		end if;
	end;

	
	
	function is_proposed (
		device : in type_device_electrical)
		return boolean
	is begin
		if is_real (device) then
			if is_proposed (device.status) then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end;



	procedure set_moving (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			set_moving (device.status);
		end if;
	end;
	

	procedure clear_moving (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			clear_moving (device.status);
		end if;
	end;

	
	
	function is_moving (
		device : in type_device_electrical)
		return boolean
	is begin
		if is_real (device) then
			if is_moving (device.status) then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end;





	

	procedure modify_status (
		device		: in out type_device_electrical;
		operation	: in type_status_operation)
	is begin
		if is_real (device) then
			modify_status (device.status, operation);
		end if;
	end modify_status;

	

	
	procedure reset_status (
		device : in out type_device_electrical)
	is begin
		if is_real (device) then
			reset_status (device.status);
		end if;
	end;

	


	
	function is_real (
		device : in pac_devices_electrical.cursor)
		return boolean 
	is 
		d : type_device_electrical renames element (device);
	begin
		return is_real (d);
	end is_real;
	


	

	function is_proposed (
		device : in pac_devices_electrical.cursor)
		return boolean
	is 
		d : type_device_electrical renames element (device);
	begin
		if is_proposed (d) then
			return true;
		else
			return false;
		end if;
	end is_proposed;
	


	

	function is_selected (
		device : in pac_devices_electrical.cursor)
		return boolean
	is 
		d : type_device_electrical renames element (device);
	begin
		if is_selected (d) then
			return true;
		else
			return false;
		end if;
	end is_selected;


	


	function is_moving (
		device : in pac_devices_electrical.cursor)
		return boolean
	is 
		d : type_device_electrical renames element (device);
	begin
		if is_moving (d) then
			return true;
		else
			return false;
		end if;
	end is_moving;


	


	procedure iterate (
		devices	: in pac_devices_electrical.map;
		process	: not null access procedure (position : in pac_devices_electrical.cursor);
		proceed	: not null access boolean)
	is 
		use pac_devices_electrical;
		c : pac_devices_electrical.cursor := devices.first;
	begin
		while c /= pac_devices_electrical.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;
	

	





	

	function get_package_model_name (
		device : in pac_devices_electrical.cursor)
		return pac_package_model_file_name.bounded_string
	is
		use et_device_model_names;
		device_model		: pac_device_model_file.bounded_string;
		device_cursor_lib	: pac_devices_lib.cursor;
		device_variant		: pac_package_variant_name.bounded_string; -- N, D
	begin
		-- CS: The device is located twice here. Consumes too much time.
		-- The issue may dissolve once devices are stored in a hashed map:
		
		-- load package variant of given device
		device_variant := pac_devices_electrical.element (device).variant;
		
		-- load the name of the generic device model
		device_model := pac_devices_electrical.element (device).model;
		
		-- locate the generic device model in the device library
		device_cursor_lib := get_device_model_cursor (device_model);
		
		return get_package_model (device_cursor_lib, device_variant);
	end get_package_model_name;


	



	
	
	function get_package_model (
		device : in pac_devices_electrical.cursor)
		return pac_package_models.cursor
	is
		package_model : constant pac_package_model_file_name.bounded_string :=
			get_package_model_name (device);  -- libraries/packages/smd/SOT23.pac
	begin
		return get_package_model (package_model);
	end get_package_model;




	function is_bom_relevant (
		device : in pac_devices_electrical.cursor) 
		return boolean 
	is
		package_model : pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac
	begin
		-- Get the package model name of the given device:
		package_model := get_package_model_name (device);

		return is_bom_relevant (package_model);
	end is_bom_relevant;

	
	
	





-- PLACEHOLDERS
	

	function to_placeholder_content (
		device_cursor	: in pac_devices_electrical.cursor;
		placeholder		: in et_device_placeholders.packages.type_placeholder)
		return et_text.pac_text_content.bounded_string 
	is
		device : type_device_electrical renames element (device_cursor);

		use et_text;
		result : pac_text_content.bounded_string;

		use et_device_placeholders;
	begin
		case placeholder.meaning is
			when NAME 		=> result := to_content (to_string (key (device_cursor)));
			when VALUE		=> result := to_content (to_string (device.value));
			when PURPOSE	=> result := to_content (to_string (device.purpose));
		end case;
		
		return result;
	end to_placeholder_content;


	
	
	
end et_devices_electrical;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
