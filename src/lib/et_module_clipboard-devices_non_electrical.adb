------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   MODULE CLIPBOARD DEVICES NON-ELECTRICAL                --
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

with et_module_names;
with et_device_name;



package body et_module_clipboard.devices_non_electrical is


-- COPY:


	procedure copy_device_to_clipboard (
		device_cursor	: in pac_devices_non_electrical.cursor;
		log_threshold	: in type_log_level)
	is
		use et_device_name;
		use pac_devices_non_electrical;

		-- Get the name of the given device (like MH2):
		device_name : constant type_device_name :=
			get_device_name (device_cursor);

		-- Get the given device:
		device : constant type_device_non_electrical :=
			element (device_cursor);


		procedure insert_device is
		begin
			clipboard.devices_non_electric.insert (
				key			=> device_name,
				new_item	=> device);

		end insert_device;


	begin
		log (text => "copy device " & to_string (device_name)
			& " to clipboard.",
			 level => log_threshold);

		log_indentation_up;

		insert_device;

		log_indentation_down;
	end copy_device_to_clipboard;










	procedure copy_selected_devices_to_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_generic_modules;
		use et_module_names;


		procedure query_module (
			module_name	: in type_module_name;
			module		: in type_generic_module)
		is
			pragma unreferenced (module_name);
			use et_device_name;
			use pac_devices_non_electrical;

			device_cursor : pac_devices_non_electrical.cursor :=
				module.devices_non_electric.first;


			procedure query_device (
				device_name	: in type_device_name;
				device		: in type_device_non_electrical)
			is begin
				if is_selected (device) then
					-- We have a selected device

					log (text => to_string (device_name),
						 level => log_threshold + 1);

					log_indentation_up;

					copy_device_to_clipboard (
						device_cursor, log_threshold + 2);

					log_indentation_down;
				end if;
			end query_device;


		begin
			-- Iterate through the devices:
			while has_element (device_cursor) loop
				query_element (device_cursor, query_device'access);
				next (device_cursor);
			end loop;
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " copy selected devices to clipboard ",
			 level => log_threshold);

		log_indentation_up;

		query_element (module_cursor, query_module'access);

		log_indentation_down;
	end copy_selected_devices_to_clipboard;










-- PASTE:


	procedure paste_devices_from_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		offset			: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure do_paste is
			use et_module_clipboard;
			use et_device_name;
			use pac_devices_non_electrical;

			device_cursor : pac_devices_non_electrical.cursor :=
				clipboard.devices_non_electric.first;


			procedure query_device (
				device_name	: in type_device_name;
				device		: in type_device_non_electrical)
			is

			begin
				log (text => "device " & to_string (device_name),
					level => log_threshold + 1);

				log_indentation_up;


				log_indentation_down;
			end query_device;


		begin
			-- Iterate through the devices in the clipboard:
			while has_element (device_cursor) loop
				query_element (device_cursor, query_device'access);
				next (device_cursor);
			end loop;
		end do_paste;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " paste devices from clipboard. Group offset: "
			 & to_string (offset),
			 level => log_threshold);

		log_indentation_up;
		do_paste;

		log_indentation_down;
	end paste_devices_from_clipboard;





end et_module_clipboard.devices_non_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
