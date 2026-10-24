------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          DEVICE UNITS IN SCHEMATIC                       --
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
with et_exceptions;						use et_exceptions;


package body et_units is


	function get_unit (
		unit_cursor : in pac_units.cursor)
		return type_unit
	is (element (unit_cursor));





	function get_position (
		unit	: in pac_units.cursor)
		return type_schematic_object
	is (get_position (element (unit)));



	function get_rotation (
		unit	: in pac_units.cursor)
		return type_rotation_model
	is (get_rotation (element (unit)));



	function get_mirror_status (
		unit	: in pac_units.cursor)
		return type_mirror
	is (element (unit).mirror_status);




	procedure iterate (
		units	: in pac_units.map;
		process	: not null access procedure (position : in pac_units.cursor);
		proceed	: not null access boolean)
	is
		c : pac_units.cursor := units.first;
	begin
		while c /= pac_units.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;




	function get_unit_name (
		unit : in pac_units.cursor)
		return type_unit_name
	is (key (unit));





	function get_unit_name (
		unit : in pac_units.cursor)
		return string
	is (to_string (key (unit)));





	function to_string (
		unit : in pac_units.cursor)
		return string
	is
		use pac_units;
	begin
		return get_unit_name (unit)
			--& to_string (type_vector_model (element (unit).position));
			& to_string (get_place (element (unit)));
		-- CS output sheet number and rotation ?
		-- CS use function get_position (et_basic_objects) ?
	end to_string;





	function is_proposed (
		unit : in pac_units.cursor)
		return boolean
	is (is_proposed (element (unit)));


	function is_selected (
		unit : in pac_units.cursor)
		return boolean
	is (is_selected (element (unit)));





	function is_moving (
		unit : in pac_units.cursor)
		return boolean
	is (is_moving (element (unit)));





	function extract_sheets (
		positions	: in pac_unit_positions.map)
		return pac_sheet_numbers.list
	is
		use pac_sheet_numbers;
		use pac_sheet_sorting;

		result : pac_sheet_numbers.list;


		procedure query_position (
			c : in pac_unit_positions.cursor)
		is
			use pac_unit_positions;
			pos : constant type_schematic_object := element (c);
			sheet : type_sheet;
		begin
			sheet := get_sheet (pos);

			if not result.contains (sheet) then
				result.append (sheet);
			end if;
		end query_position;

	begin
		positions.iterate (query_position'access);

		-- Sort the sheets in ascending order:
		sort (result);

		return result;
	end extract_sheets;







	function extract_sheets (
		units	: in pac_units.map)
		return pac_sheet_numbers.list
	is
		use pac_sheet_numbers;
		use pac_sheet_sorting;

		result : pac_sheet_numbers.list;


		procedure query_unit (
			c : in pac_units.cursor)
		is
			use pac_units;
			unit : type_unit renames element (c);
			sheet : type_sheet;
		begin
			sheet := get_sheet (unit);

			if not result.contains (sheet) then
				result.append (sheet);
			end if;
		end query_unit;


	begin
		units.iterate (query_unit'access);

		-- Sort the sheets in ascending order:
		sort (result);

		return result;
	end extract_sheets;







	function get_unit_positions (
		units : in pac_units.map)
		return pac_unit_positions.map
	is
		list : pac_unit_positions.map; -- to be returned
		use pac_units;
		use pac_unit_positions;

		procedure query_unit (
			cursor : pac_units.cursor)
		is begin
			list.insert (
				key (cursor),
				get_position (cursor));
		end query_unit;

	begin
		iterate (units, query_unit'access);
		return list;
	end get_unit_positions;





	function get_port_name (
		port : in pac_dragged_ports.cursor)
		return type_port_name
	is (key (port));





	function make_drag_list (
		ports_old : in pac_symbol_ports.map;
		ports_new : in pac_symbol_ports.map)
		return pac_dragged_ports.map
	is
		use et_symbol_ports;
		use pac_dragged_ports;
		drag_list : pac_dragged_ports.map;

		-- ports_old and ports_new are both equally long and contain
		-- equal keys (the port names). So we use two cursors and advance them
		-- simultaneously in a loop (see below).
		use pac_symbol_ports;

		cursor_old : pac_symbol_ports.cursor := ports_old.first;
		cursor_new : pac_symbol_ports.cursor := ports_new.first;

		drag : type_drag;
	begin
		-- Iterate through list ports_old, copy the port name to
		-- the drag list.
		-- Take the old position from ports_old and
		-- the new position from ports_new:
		while has_element (cursor_old) loop
			drag := (before => element (cursor_old).position,
					after	=> element (cursor_new).position);

			insert (
				container	=> drag_list,
				key			=> key (cursor_old), -- the port name
				new_item	=> drag);

			next (cursor_old);
			next (cursor_new);
		end loop;

		return drag_list;
	end make_drag_list;




end et_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
