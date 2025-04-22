------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         DEVICES NON-ELECTRICAL                           --
--                                                                          --
--                              B o d y                                     --
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

with ada.text_io;					use ada.text_io;



package body et_devices_non_electrical is


	function to_mirror_along_y_axis (
		flipped : in type_flipped)
		return type_mirror
	is begin
		case flipped is
			when YES => return MIRROR_ALONG_Y_AXIS;
			when NO =>  return MIRROR_NO;
		end case;
	end to_mirror_along_y_axis;

	




	procedure set_proposed (
		device : in out type_device_non_electric)
	is begin
		set_proposed (device.status);
	end;

	
	procedure clear_proposed (
		device : in out type_device_non_electric)
	is begin
		clear_proposed (device.status);
	end;

	
	function is_proposed (
		device : in type_device_non_electric)
		return boolean
	is begin
		if is_proposed (device.status) then
			return true;
		else
			return false;
		end if;
	end;
	

	

	procedure set_selected (
		device : in out type_device_non_electric)
	is begin
		set_selected (device.status);
	end;

	
	procedure clear_selected (
		device : in out type_device_non_electric)
	is begin
		clear_selected (device.status);
	end;
	
	
	function is_selected (
		device : in type_device_non_electric)
		return boolean
	is begin
		if is_selected (device.status) then
			return true;
		else
			return false;
		end if;
	end;




	procedure set_moving (
		device : in out type_device_non_electric)
	is begin
		set_moving (device.status);
	end;

	
	procedure clear_moving (
		device : in out type_device_non_electric)
	is begin
		clear_moving (device.status);
	end;

	
	function is_moving (
		device : in type_device_non_electric)
		return boolean
	is begin
		if is_moving (device.status) then
			return true;
		else
			return false;
		end if;
	end;
	



	procedure set_locked (
		device : in out type_device_non_electric)
	is begin
		set_locked (device.status);
	end;

	
	procedure clear_locked (
		device : in out type_device_non_electric)
	is begin
		clear_locked (device.status);
	end;

	
	function is_locked (
		device : in type_device_non_electric)
		return boolean
	is begin
		if is_locked (device.status) then
			return true;
		else
			return false;
		end if;
	end;


	



	procedure modify_status (
		device		: in out type_device_non_electric;
		operation	: in type_status_operation)						
	is begin
		modify_status (device.status, operation);
	end;

	


	procedure reset_status (
		device : in out type_device_non_electric)
	is begin
		reset_status (device.status);
	end;




	
	procedure iterate (
		devices	: in pac_devices_non_electric.map;
		process	: not null access procedure (position : in pac_devices_non_electric.cursor);
		proceed	: not null access boolean)
	is
		use pac_devices_non_electric;
		c : pac_devices_non_electric.cursor := devices.first;
	begin
		while c /= no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	

	
	function to_string (
		device : in pac_devices_non_electric.cursor)
		return string
	is begin
		return to_string (key (device));
	end to_string;

	
	

	function is_proposed (
		device : in pac_devices_non_electric.cursor)
		return boolean
	is begin
		if is_proposed (element (device)) then
			return true;
		else
			return false;
		end if;
	end;
	
	

	function is_selected (
		device : in pac_devices_non_electric.cursor)
		return boolean
	is begin
		if is_selected (element (device)) then
			return true;
		else
			return false;
		end if;
	end;


	
	function is_moving (
		device : in pac_devices_non_electric.cursor)
		return boolean
	is begin
		if is_moving (element (device)) then
			return true;
		else
			return false;
		end if;
	end;
	

	function is_locked (
		device : in pac_devices_non_electric.cursor)
		return boolean		
	is begin
		if is_locked (element (device)) then
			return true;
		else
			return false;
		end if;
	end;
		



	function get_package_model (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_package_models.cursor
	is 
		result : pac_package_models.cursor;
	begin
		result := get_package_model (element (device_cursor).package_model);
		return result;
	end get_package_model;





	function get_position (
		device_cursor	: in pac_devices_non_electric.cursor) -- FD1
		return type_package_position
	is begin
		return element (device_cursor).position;
	end get_position;


	

	function get_position (
		device_cursor	: in pac_devices_non_electric.cursor) -- FD1
		return type_vector_model
	is begin
		return get_position (device_cursor).place;
	end get_position;



	
	function get_face (
		device_cursor	: in pac_devices_non_electric.cursor)
		return type_face
	is 
		position : type_package_position;
	begin
		position := element (device_cursor).position;
		return get_face (position);
	end get_face;
	
	
end et_devices_non_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
