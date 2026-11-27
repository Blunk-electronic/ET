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



	function to_string (
		device		: in type_device_name;
		unit		: in type_unit_name)
		return string
	is begin
		return "device " & to_string (device)
			& " unit " & to_string (unit);
	end to_string;



	function get_full_name (
		device		: in type_device_name;
		unit		: in type_unit_name;
		unit_count	: in type_unit_count)
		return string
	is begin
		if unit_count > 1 then
			return to_string (device)
				& device_unit_separator
				& to_string (unit);
		else
			return to_string (device);
		end if;
	end get_full_name;





	function to_string (
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name; -- C
		query_result	: in type_unit_query)
		return string
	is
	begin
		if query_result.exists then
			if get_length (unit_name) > 0 then
				return "Location of device " & to_string (device_name)
					& " unit " & to_string (unit_name)
					& " :" & get_position (query_result.position);
			else
				return "Location of device " & to_string (device_name)
					& " :" & get_position (query_result.position);
			end if;
		else
			return "device " & to_string (device_name)
				& " unit " & to_string (unit_name)
				& " does not exist !";
		end if;
	end to_string;








	function get_unit_names_deployed (
		device : in type_device_electrical)
		return pac_unit_names.list
	is
		use pac_units;
		use pac_unit_names;
		result : pac_unit_names.list;

		procedure query_unit (c : in pac_units.cursor) is begin
			result.append (key (c));
		end query_unit;

	begin
		device.units.iterate (query_unit'access);
		return result;
	end get_unit_names_deployed;





	function get_unit_count (
		device : in type_device_electrical)
		return natural
	is
		result : natural;
		cursor : pac_device_models.cursor;
	begin
		-- Get a cursor to the device in the library.
		-- Map from schematic cursor to library cursor:
		cursor := get_device_model (device);

		-- Get the number of units from the device model:
		result := get_unit_count (cursor);

		return result;
	end get_unit_count;




	function get_unit_count_deployed (
		device : in type_device_electrical)
		return natural
	is (natural (device.units.length));




	function locate_unit (
		device	: in type_device_electrical;
		unit	: in type_unit_name)
		return pac_units.cursor
	is
		use pac_units;
	begin
		return find (device.units, unit);
	end locate_unit;



	function is_unit_deployed (
		device	: in type_device_electrical;
		unit	: in type_unit_name)
		return boolean
	is
		use pac_units;
	begin
		return device.units.contains (unit);
	end is_unit_deployed;





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


	procedure add_unit (
		device	: in out type_device_electrical;
		name	: in type_unit_name;
		unit	: in type_unit)
	is
		inserted : boolean;
		cursor : pac_units.cursor;
	begin
		device.units.insert (
			key			=> name,
			position	=> cursor,
			inserted	=> inserted,
			new_item	=> unit);
	end add_unit;



	procedure delete_unit (
		device	: in out type_device_electrical;
		unit	: in type_unit_name)
	is
		c : pac_units.cursor;
	begin
		c := device.units.find (unit);
		device.units.delete (c);
	end delete_unit;




	function get_first_unit (
		device	: in type_device_electrical)
		return pac_units.cursor
	is (device.units.first);



	procedure iterate_units (
		device	: in type_device_electrical;
		process	: not null access procedure (position : in pac_units.cursor))
	is
		use pac_units;
		c : pac_units.cursor := device.units.first;
	begin
		while has_element (c) loop
			process (c);
			next (c);
		end loop;
	end iterate_units;



	procedure iterate_units (
		device	: in type_device_electrical;
		process	: not null access procedure (position : in pac_units.cursor);
		proceed	: not null access boolean)
	is
		use pac_units;
		c : pac_units.cursor := device.units.first;
	begin
		while has_element (c) and proceed.all loop
			process (c);
			next (c);
		end loop;
	end iterate_units;








-- POSITION:



	function get_position (
		device	: in type_device_electrical;
		unit	: in type_unit_name)
		return type_schematic_object
	is
		-- Locate the given unit in the device:
		unit_cursor : constant pac_units.cursor :=
			locate_unit (device, unit);

		result : type_schematic_object;


		procedure query_unit (
			unit_name	: in type_unit_name;
			unit		: in type_unit)
		is
			pragma unreferenced (unit_name);
		begin
			result := get_position (unit);
		end query_unit;

	begin
		query_element (unit_cursor, query_unit'access);
		return result;
	end get_position;





	function get_unit_positions (
		device	: in type_device_electrical)
		return pac_unit_positions.map
	is
		result : pac_unit_positions.map;
	begin
		result := get_unit_positions (device.units);
		return result;
	end get_unit_positions;




	procedure log_unit_positions (
		positions		: in pac_unit_positions.map;
		log_threshold	: in type_log_level)
	is

		procedure write (
			cursor : in pac_unit_positions.cursor)
		is begin
			log (text => "unit "
				& to_string (pac_unit_positions.key (cursor)) -- unit name
				& get_position (pac_unit_positions.element (cursor)), -- sheet x y
				level => log_threshold);
		end write;

	begin
		log (text => "location(s) in schematic:", level => log_threshold);
		log_indentation_up;
		pac_unit_positions.iterate (positions, write'access);
		log_indentation_down;
	end log_unit_positions;









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




	procedure modify_status (
		device		: in out type_device_electrical;
		unit_cursor	: in pac_units.cursor;
		operation	: in type_status_operation)
	is

		procedure query_unit (
			unit_name	: in type_unit_name;
			unit		: in out type_unit)
		is
			pragma unreferenced (unit_name);
		begin
			modify_status (unit, operation);
		end query_unit;


	begin
		device.units.update_element (unit_cursor, query_unit'access);
	end modify_status;




	procedure modify_unit (
		device		: in out type_device_electrical;
		unit_cursor	: in pac_units.cursor;
		process		: not null access procedure (
				name	: in type_unit_name;
				unit	: in out type_unit))
	is begin
		-- CS: not sure if this is correct:
		device.units.update_element (unit_cursor, process);
	end modify_unit;




end et_device_electrical.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
