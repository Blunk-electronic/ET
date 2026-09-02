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

with et_module_names;
with et_device_name;
with et_unit_name;
with et_devices_electrical.units;
with et_schematic_ops_units;



package body et_module_clipboard.devices_electrical is


-- COPY:


	procedure copy_unit_to_clipboard (
		device_cursor	: in pac_devices_electrical.cursor;
		unit_cursor		: in pac_units.cursor;
		log_threshold	: in type_log_level)
	is
		use et_device_name;
		use et_unit_name;
		use pac_devices_electrical;

		-- Get the name of the given device (like IC3):
		device_name : constant type_device_name :=
			get_device_name (device_cursor);

		-- Get the given device:
		device : constant type_device_electrical :=
			element (device_cursor);

		-- Get the name of the given unit:
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

				-- CS: reset status flags of unit ?

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










	procedure copy_selected_units_to_clipboard (
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
			use pac_devices_electrical;
			device_cursor : pac_devices_electrical.cursor := module.devices.first;


			procedure query_device (
				device_name	: in type_device_name;
				device		: in type_device_electrical)
			is
				use et_unit_name;
				use pac_units;
				unit_cursor : pac_units.cursor := device.units.first;


				procedure query_unit (
					unit_name	: in type_unit_name;
					unit		: in type_unit)
				is
					use et_devices_electrical.units;
				begin
					if is_selected (unit) then
						-- We have a selected unit.

						-- Log device and unit name:
						log (text => to_string (device_name, unit_name),
							level => log_threshold + 1);

						log_indentation_up;

						copy_unit_to_clipboard (
							device_cursor, unit_cursor, log_threshold + 2);

						log_indentation_down;
					end if;
				end query_unit;


			begin
				-- Iterate through the units:
				while has_element (unit_cursor) loop
					query_element (unit_cursor, query_unit'access);
					next (unit_cursor);
				end loop;
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
			 & " copy selected units to clipboard ",
			 level => log_threshold);

		log_indentation_up;

		query_element (module_cursor, query_module'access);

		log_indentation_down;
	end copy_selected_units_to_clipboard;








-- PASTE:


	procedure paste_units_from_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		offset			: in type_object_position_relative;
		log_threshold	: in type_log_level)
	is

		procedure do_paste is
			use et_module_clipboard;
			use et_device_name;
			use pac_devices_electrical;
			device_cursor : pac_devices_electrical.cursor :=
				clipboard.devices.first;


			procedure query_device (
				device_name	: in type_device_name;
				device		: in type_device_electrical)
			is
				use et_unit_name;
				use pac_units;
				unit_cursor : pac_units.cursor := device.units.first;

				-- On copying a unit, a new device is created
				-- indirectly. Here we store the name of the
				-- newly created device. It is required in case
				-- another unit is found that belongs to the
				-- same device:
				device_created : type_device_name;

				-- Here we store the name of the last device
				-- for which a unit has been copied:
				device_last : type_device_name; -- assumes default


				procedure query_unit (
					unit_name	: in type_unit_name;
					unit		: in type_unit)
				is
					pragma unreferenced (unit);
					use et_schematic_ops_units;


					procedure copy_in_same_device is begin
						log (text => "copy unit into same device",
							level => log_threshold + 3);

						copy_unit (
							module_cursor	=> module_cursor,
							device_cursor	=> device_cursor,
							unit_cursor		=> unit_cursor,
							sheet			=> get_sheet (offset),
							offset			=> get_place (offset),
							target_device	=> device_created,
							device_created	=> device_created,
							log_threshold	=> log_threshold + 4);

					end copy_in_same_device;


					procedure copy_in_new_device is begin
						log (text => "copy unit in new device",
							level => log_threshold + 3);

						log_indentation_up;

						copy_unit (
							module_cursor	=> module_cursor,
							device_cursor	=> device_cursor,
							unit_cursor		=> unit_cursor,
							sheet			=> get_sheet (offset),
							offset			=> get_place (offset),
							device_created	=> device_created,
							log_threshold	=> log_threshold + 4);

						log_indentation_down;
					end copy_in_new_device;



				begin
					log (text => "unit " & to_string (unit_name),
						level => log_threshold + 2);

					log_indentation_up;

					-- If the last processed device is the same
					-- as the current one, then no new device is
					-- to be created but just the unit copied:
					if device_last = device_name then
						copy_in_same_device;
					else
						-- If a another device is being processed,
						-- then copy the current unit in a new
						-- device:
						copy_in_new_device;
					end if;

					-- Backup the name of the last device:
					device_last := device_name;

					log_indentation_down;
				end query_unit;


			begin
				log (text => "device " & to_string (device_name),
					level => log_threshold + 1);

				log_indentation_up;

				-- Iterate through the units:
				while has_element (unit_cursor) loop
					query_element (unit_cursor, query_unit'access);
					next (unit_cursor);
				end loop;

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
			 & " paste units from clipboard. Group offset: " & to_string (offset),
			 level => log_threshold);

		log_indentation_up;
		do_paste;

		log_indentation_down;
	end paste_units_from_clipboard;





end et_module_clipboard.devices_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
