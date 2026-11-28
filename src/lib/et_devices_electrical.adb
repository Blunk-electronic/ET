------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICES ELECTRICAL LISTS                         --
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



package body et_devices_electrical is



	function get_device_names (
		devices : in pac_devices_electrical.map)
		return pac_device_names.set
	is
		result : pac_device_names.set;

		procedure query_device (
			c : in pac_devices_electrical.cursor)
		is begin
			-- Insert the device name in the resulting list:
			result.insert (key (c));
		end query_device;

	begin
		-- Iterate through the devices:
		devices.iterate (query_device'access);

		return result;
	end get_device_names;






	function get_count (
		devices	: in pac_devices_electrical.map)
		return natural
	is (natural (devices.length));



	function get_count (
		devices	: in pac_devices_electrical.map)
		return string
	is (count_type'image (devices.length));






	function get_device_model_file (
		device : pac_devices_electrical.cursor)
		return type_device_model_name
	is (get_device_model_file (element (device)));




	function get_device_model (
		device : in pac_devices_electrical.cursor)
		return pac_device_models.cursor
	is
		d : type_device_electrical renames element (device);
	begin
		return get_device_model (d);
	end get_device_model;












	function get_device_name (
		device : in pac_devices_electrical.cursor)
		return type_device_name
	is (key (device));




	function get_device_name (
		device : in pac_devices_electrical.cursor)
		return string
	is (to_string (key (device)));




	function get_prefix (
		device : in pac_devices_electrical.cursor)
		return type_device_prefix
	is (get_prefix (key (device)));









	function is_real (
		device : in pac_devices_electrical.cursor)
		return boolean
	is
		d : type_device_electrical renames element (device);
	begin
		return is_real (d);
	end is_real;









-- VALUE:

	function get_value (
		device : in pac_devices_electrical.cursor)
		return type_device_value
	is
		d : type_device_electrical renames element (device);
	begin
		return get_value (d);
	end get_value;







-- PARTCODE:

	function get_partcode (
		device : in pac_devices_electrical.cursor)
		return type_device_partcode
	is
		d : type_device_electrical renames element (device);
	begin
		return get_partcode (d);
	end get_partcode;







-- PURPOSE:


	function get_purpose (
		device : in pac_devices_electrical.cursor)
		return type_device_purpose
	is
		d : type_device_electrical renames element (device);
	begin
		return get_purpose (d);
	end get_purpose;








-- STATUS:


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






-- PACKAGE VARIANT:

	function get_variant (
		device : in pac_devices_electrical.cursor)
		return type_package_variant_name
	is
		d : type_device_electrical renames element (device);
	begin
		return get_variant (d);
	end get_variant;







-- PACKAGE MODEL:

	function get_package_model_name (
		device : in pac_devices_electrical.cursor)
		return type_package_model_name
	is
		use et_device_library.packages;
		d : type_device_electrical renames element (device);
	begin
		return get_package_model (
			get_device_model (d),
			get_variant (d));
	end get_package_model_name;





	function get_package_model (
		device : in pac_devices_electrical.cursor)
		return pac_package_models.cursor
	is
		package_model : constant type_package_model_name :=
			get_package_model_name (device);  -- libraries/packages/smd/SOT23.pac
	begin
		return get_package_model (package_model);
	end get_package_model;




	function is_bom_relevant (
		device : in pac_devices_electrical.cursor)
		return boolean
	is
		package_model : type_package_model_name; -- libraries/packages/smd/SOT23.pac
	begin
		-- Get the package model name of the given device:
		package_model := get_package_model_name (device);

		return is_bom_relevant (package_model);
	end is_bom_relevant;












-- PLACEHOLDERS


	function to_placeholder_content (
		device_cursor	: in pac_devices_electrical.cursor;
		placeholder		: in et_device_placeholders.packages.type_text_placeholder)
		return type_text_content
	is
		device : type_device_electrical renames element (device_cursor);

		result : type_text_content;

		use et_device_placeholders;
	begin
		case placeholder.meaning is
			when NAME		=> result := to_content (to_string (key (device_cursor)));
			when VALUE		=> result := to_content (to_string (get_value (device)));
			when PURPOSE	=> result := to_content (to_string (get_purpose (device)));
		end case;

		return result;
	end to_placeholder_content;





end et_devices_electrical;


-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
