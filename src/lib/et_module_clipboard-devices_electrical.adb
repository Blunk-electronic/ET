------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   MODULE CLIPBOARD DEVICES ELECTRICAL                    --
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
--  ToDo:
--


-- with et_exceptions;				use et_exceptions;


-- with ada.text_io;			use ada.text_io;

with et_device_name;				use et_device_name;
with et_unit_name;					use et_unit_name;



package body et_module_clipboard.devices_electrical is



	procedure copy_unit_to_clipboard (
		device_cursor	: in pac_devices_electrical.cursor;
		unit_cursor		: in pac_units.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_devices_electrical;
		
		-- Get the name of the given device (like IC3):
		device_name : constant type_device_name := 
			get_device_name (device_cursor);
			
		-- Get the given 
		device : constant type_device_electrical :=
			element (device_cursor);
			
		unit_name : constant type_unit_name :=
			get_unit_name (unit_cursor);

		
		
		procedure insert_device_and_unit is
			device_cursor_clipboard : pac_devices_electrical.cursor;
			inserted : boolean;
			

			-- This procedure inserts the given unit
			-- in the candidate device:			
			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_electrical)
			is
				use pac_units;
				unit : constant type_unit := element (unit_cursor);
			begin
				log (text => "insert unit " & to_string (unit_name),
					level => log_threshold + 2);
					
				device.units.insert (
					key			=> unit_name,
					new_item	=> unit);
					
				-- The unit should not exist already.
				-- Otherwise an exception will be raised here.
				-- CS: Exception handler ?
			end query_device;
			
			
		begin
			-- Locate the given device in the clipboard:
			device_cursor_clipboard := clipboard.devices.find (device_name);
			
			-- If the device already exists, then do nothing
			-- special except a log message:
			if has_element (device_cursor_clipboard) then
				
				log (text => "device " & to_string (device_name)
					& " already in clipboard",
					level => log_threshold + 1);

			else
			-- If the device does not exist yet, then
			-- insert the bare device (witout units) in the clipboard.
				clipboard.devices.insert (
					key			=> device_name,
					new_item	=> copy_bare_device (device),
					position	=> device_cursor_clipboard,
					inserted	=> inserted);
			
			end if;
			
			-- Insert the given unit in the device:
			clipboard.devices.update_element (
				device_cursor_clipboard, query_device'access);
			
		end insert_device_and_unit;
		
		
	begin
		log (text => "copy device " & to_string (device_name)
			& " unit " & to_string (unit_name) & " to clipboard.",
			 level => log_threshold);

		log_indentation_up;

		insert_device_and_unit;

		log_indentation_down;
	end copy_unit_to_clipboard;




end et_module_clipboard.devices_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
