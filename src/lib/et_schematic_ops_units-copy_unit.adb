------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                  SCHEMATIC OPERATIONS / COPY UNIT                        --
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

--   For correct displaying set tab with in your editor to 4.

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
with et_unit_name;						use et_unit_name;
use et_unit_name.pac_unit_name;


separate (et_schematic_ops_units)

procedure copy_unit (
	module_cursor	: in pac_generic_modules.cursor;
	device_cursor	: in pac_devices_electrical.cursor;
	unit_cursor		: in pac_units.cursor;
	sheet			: in type_sheet_relative;
	destination		: in type_vector_model;
	target_device	: in type_device_name := device_name_default;
	device_created	: out type_device_name;
	log_threshold	: in type_log_level)
is
	-- Since the name of the unit to be copied is used frequently
	-- here, we store it in a constant:
	unit_name : constant type_unit_name :=
		get_unit_name (unit_cursor);


	-- Here we store the position of the new unit.
	-- It is an absolute position:
	position_new : type_object_position;


	-- This procedure computes the position
	-- where the copy of the unit will be placed:
	procedure compute_final_position is begin
		-- First we copy the coordinates
		-- from the original unit:
		position_new := get_position (unit_cursor);

		-- In the following, the rotatation remains unchanged
		-- because we copy the rotation along with other
		-- properties of the unit.

		-- Add to the original unit
		-- position the given number of
		-- relative sheet offset:
		move_by_sheets (position_new, sheet);

		-- Regard the given "destination" as offset.
		-- Move the original position by
		-- the given "destination":
		move_by (position_new, destination);

		-- Now the absolute position of
		-- the new unit is complete and
		-- can be assigned to the new unit.
	end compute_final_position;



	-- This procedure copies the given unit into
	-- the device given by argument target_device.
	-- It also connects the new unit with net segments
	-- that may start or end at the ports of the new unit:
	procedure copy_into_specified_device is

		-- Get a cursor to the target device:
		target_device_cursor : constant pac_devices_electrical.cursor :=
			get_electrical_device (module_cursor, target_device);

		-- Get a cursor to the model of the target device:
		device_cursor_lib : constant pac_device_models.cursor :=
			get_device_model (target_device_cursor);



		procedure query_module (
			module_name	: in type_module_name;
			module		: in out type_generic_module)
		is
			pragma unreferenced (module_name);

			procedure query_target_device (
				device_name	: in type_device_name;
				device		: in out type_device_electrical)
			is
				-- Here we store temporarily the ports (with
				-- their positions) of the new unit:
				ports : pac_symbol_ports.map;
			begin
				-- CS log messages

				-- Copy the unit into the target device:
				copy_unit_to_device (
					unit_cursor, sheet, destination, device);

				-- Get the ports with their original positions
				-- as they are defined in the device model:
				ports := get_ports_from_symbol_model (
					device_cursor	=> device_cursor_lib,
					unit_name		=> unit_name);

				-- Move the ports to the position of the unit:
				move_ports (ports, position_new);

				-- Insert the new unit ports in the net segments:
				insert_ports (
					module_cursor	=> module_cursor,
					device_name		=> device_name,
					unit_name		=> unit_name,
					ports			=> ports,
					sheet			=> get_sheet (position_new),
					log_threshold	=> log_threshold + 2);


				-- Update the ratsnest if the target device is real:
				if is_real (device_cursor_lib) then
					update_ratsnest (module_cursor, log_threshold + 1);
				end if;


				-- The copy was made in the given target device.
				-- So the created device is the same as the target
				-- device:
				device_created := device_name;
			end query_target_device;


		begin
			module.devices.update_element (
				target_device_cursor, query_target_device'access);
		end query_module;


	begin
		generic_modules.update_element (
			module_cursor, query_module'access);

	end copy_into_specified_device;



begin
	log (text => "module " & to_string (module_cursor)
		& " device " & get_device_name (device_cursor)
		& " copy unit " & to_string (unit_name)
		& " by sheet(s) " & relative_to_string (sheet)
		& " offset " & to_string (destination),
		level => log_threshold);

	log_indentation_up;

	-- Now we compute the new position
	-- of the new unit.
	compute_final_position;


	-- If no target_device was given then we copy
	-- the unit and create a new device indirectly:
	if is_default_name (target_device) then
		log (text => "copy into new device",
			 level => log_threshold + 1);

		log_indentation_up;

		copy_device (
			module_cursor		=> module_cursor,
			device_name			=> key (device_cursor),
			unit_name_explicit	=> key (unit_cursor),
			destination			=> position_new, -- absolute
			commit_design		=> NO_COMMIT,
			device_created		=> device_created,
			log_threshold		=> log_threshold + 2);

		log_indentation_down;

	else
	-- If a target_device was specified then we copy
	-- the unit into the given device:
		log (text => "copy into explicitly specified target device "
			 & to_string (target_device),
			 level => log_threshold + 1);

		log_indentation_up;
		copy_into_specified_device;
		log_indentation_down;

	end if;


	log_indentation_down;
end copy_unit;


-- Soli Deo Gloria


-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
