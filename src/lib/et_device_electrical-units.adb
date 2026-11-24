------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DEVICE ELECTRICAL / UNITS                           --
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
-- To Do:
--
--
--
--
--

with et_device_placeholders.symbols;



package body et_device_electrical.units is

	use pac_units;



-- POSITION:

	function get_unit_positions (
		device	: in type_device_electrical)
		return pac_unit_positions.map
	is
		result : pac_unit_positions.map;
	begin
		result := get_unit_positions (device.units);
		return result;
	end get_unit_positions;





	procedure move_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name;
		coordinates	: in type_coordinates;
		destination	: in type_vector_model;
		sheet		: in type_sheet_relative)
	is
		-- Locate the targeted unit.
		-- The unit must exist. Otherwise an exception
		-- will be raised here:
		unit_cursor : constant pac_units.cursor :=
			locate_unit (device, unit_name);


		procedure query_unit (
			unit_name	: in type_unit_name;
			unit		: in out type_unit)
		is
			pragma unreferenced (unit_name);
		begin
			case coordinates is
				when ABSOLUTE =>
					-- build the new position while preserving rotation:
					set_place (unit, destination);
					set_sheet (unit, type_sheet (sheet));

				when RELATIVE =>
					move_by_sheets (unit, sheet);
					move_by (unit, destination);
					-- rotation remains as it is
			end case;
		end query_unit;


	begin
		device.units.update_element (
			position	=> unit_cursor,
			process		=> query_unit'access);

	end move_unit;





	procedure drag_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name;
		coordinates	: in type_coordinates;
		destination	: in type_vector_model;
		sheet		: in type_sheet_relative)
	is
		-- Locate the targeted unit.
		-- The unit must exist. Otherwise an exception
		-- will be raised here:
		unit_cursor : constant pac_units.cursor :=
			locate_unit (device, unit_name);


		procedure query_unit (
			unit_name	: in type_unit_name;
			unit		: in out type_unit)
		is
			pragma unreferenced (unit_name);
		begin
			case coordinates is
				when ABSOLUTE =>
					-- build the new position. rotation not changed:
					move_to (unit, destination);
					set_sheet (unit, sheet);

				when RELATIVE =>
					move_by (unit, destination);
					-- The relative sheet change is 0 because
					-- we only drag the unit across the current sheet.
					-- The rotation remains as it is.
			end case;
		end query_unit;


	begin
		device.units.update_element (
			position	=> unit_cursor,
			process		=> query_unit'access);

	end drag_unit;







	procedure rotate_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name;
		coordinates	: in type_coordinates;
		rotation	: in type_rotation_model)
	is
		-- Locate the targeted unit.
		-- The unit must exist. Otherwise an exception
		-- will be raised here:
		unit_cursor : constant pac_units.cursor :=
			locate_unit (device, unit_name);


		procedure query_unit (
			unit_name	: in type_unit_name;
			unit		: in out type_unit)
		is
			pragma unreferenced (unit_name);

			use et_device_placeholders.symbols;
		begin
			case coordinates is
				when ABSOLUTE =>
					set_rotation (unit, rotation);

				when RELATIVE =>
					rotate_by (unit, rotation);
			end case;

			-- According to the new rotation of the unit,
			-- the placeholders must be rotated also. This is
			-- about the rotation of the about their own origin
			-- and the rotation about the origin of the unit:
			rotate_placeholders (unit.placeholders, rotation);
		end query_unit;


	begin
		device.units.update_element (
			position	=> unit_cursor,
			process		=> query_unit'access);

	end rotate_unit;







	procedure mirror_unit (
		device		: in out type_device_electrical;
		unit_name	: in type_unit_name)
	is
		-- Locate the targeted unit.
		-- The unit must exist. Otherwise an exception
		-- will be raised here:
		unit_cursor : constant pac_units.cursor :=
			locate_unit (device, unit_name);


		procedure query_unit (
			unit_name	: in type_unit_name;
			unit		: in out type_unit)
		is
			pragma unreferenced (unit_name);
		begin
			toggle_mirror_status (unit);

			-- The placeholders are left as they are.
			-- CS: in case this is required indeed, then use
			-- rotate_placeholders (unit.placeholders, rotation);
		end query_unit;


	begin
		device.units.update_element (
			position	=> unit_cursor,
			process		=> query_unit'access);

	end mirror_unit;








	procedure move_units_by_sheets (
		device		: in out type_device_electrical;
		sheet		: in type_sheet;
		offset		: in type_sheet_relative)
	is
		unit_cursor : pac_units.cursor := get_first_unit (device);


		-- Moves the unit candidate by the given offset
		-- if it is on the given sheet:
		procedure query_unit (
			unit_name	: in type_unit_name;
			unit		: in out type_unit)
		is
			pragma unreferenced (unit_name);
		begin
			if get_sheet (unit) = sheet then
				move_by_sheets (unit, offset);
			end if;
		end query_unit;


	begin
		-- Iterate trough the units:
		while has_element (unit_cursor) loop
			device.units.update_element (unit_cursor, query_unit'access);

			next (unit_cursor);
		end loop;
	end move_units_by_sheets;







	procedure copy_unit_to_device (
		source_unit_cursor	: in pac_units.cursor;
		sheet				: in type_sheet_relative;
		offset				: in type_vector_model;
		target_device		: in out type_device_electrical)
	is
		-- Make a copy of the given unit.
		-- This includes placeholders, position,
		-- rotation and mirror status:
		unit : type_unit := get_unit (source_unit_cursor);

		c : pac_units.cursor;
		inserted : boolean;
	begin
		-- Move the unit by the given offset
		-- and the number of given sheets:
		move_by (unit, offset);
		move_by_sheets (unit, sheet);

		-- Insert the unit in the given target device:
		target_device.units.insert (
			key			=> key (source_unit_cursor),
			new_item	=> unit,
			position	=> c,
			inserted	=> inserted);

		if not inserted then
			raise constraint_error; -- CS text message ?
		end if;
	end copy_unit_to_device;







	procedure select_unit (
		device		: in out type_device_electrical;
		all_units	: in boolean;
		unit_name	: in type_unit_name)
	is
		unit_cursor : pac_units.cursor := device.units.first;


		procedure query_unit (
			unit_name	: in type_unit_name;
			unit		: in out type_unit)
		is
			pragma unreferenced (unit_name);
		begin
			set_selected (unit);
		end query_unit;


	begin
		if all_units then
			-- Iterate though all units and set each of them
			-- as selected:
			while has_element (unit_cursor) loop
				device.units.update_element (unit_cursor, query_unit'access);
				next (unit_cursor);
			end loop;

		else
			-- Set only the given unit as selected:
			unit_cursor := locate_unit (device, unit_name);
			device.units.update_element (unit_cursor, query_unit'access);
		end if;
	end select_unit;



end et_device_electrical.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
