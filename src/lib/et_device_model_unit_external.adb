------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     DEVICE MODEL / EXTERNAL UNIT                         --
--                                                                          --
--                              B o d y                                     --
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

with ada.text_io;				use ada.text_io;

-- with et_exceptions;				use et_exceptions;


package body et_device_model_unit_external is




	function get_ports_external (
		unit_cursor	: in pac_units_external.cursor)
		return pac_symbol_ports.map
	is 
		result : pac_symbol_ports.map; -- to be returned

		unit : type_unit_external renames element (unit_cursor);
		
		
		procedure query_symbol (
			symbol_name	: in pac_symbol_model_name.bounded_string;
			symbol		: in type_symbol) 
		is begin
			result := symbol.ports;
		end query_symbol;

		
	begin
		-- Fetch the ports of the external unit.
		-- CS: constraint_error arises here if symbol model could not be located.
		pac_symbol_models.query_element (
			position	=> unit.model_cursor,
			process		=> query_symbol'access);
		
		return result;
	end get_ports_external;

	

	

	function get_symbol_model_file (
		unit	: in pac_units_external.cursor)
		return pac_symbol_model_name.bounded_string
	is 
		u : type_unit_external renames element (unit);
		use pac_symbol_models;
	begin
		return key (u.model_cursor);
	end get_symbol_model_file;
	


	
	
	function get_symbol_model_name (
		unit	: in pac_units_external.cursor)
		return string
	is begin
		return pac_symbol_model_name.to_string (
			get_symbol_model_file (unit));
	end;


	
	
	

	
	function get_symbol (
		unit	: in pac_units_external.cursor)
		return pac_symbol_models.cursor
	is
		result : pac_symbol_models.cursor;
		symbol_file : pac_symbol_model_name.bounded_string; -- *.sym
	begin
		symbol_file := get_symbol_model_file (unit);

		get_symbol_model (symbol_file, result);
		return result;
	end;




	

	function get_port_positions (
		unit	: in pac_units_external.cursor)
		return pac_points.list
	is
		result : pac_points.list;
		
		-- The name of the associated symbol model file:
		sym_name : pac_symbol_model_name.bounded_string;
		-- like /libraries/symbols/NAND.sym

		-- The cursor of the actual symbol in
		-- the symbol library:
		sym_cursor : pac_symbol_models.cursor;
	
	begin
		-- If the given cursor points to a unit, then
		-- extract the port positions. Otherwise return
		-- an empty list:
		if has_element (unit) then
			
			-- Get the name of the symbol model file
			-- of the given external unit:
			sym_name := get_symbol_model_file (unit);

			-- Locate the symbol in the rig wide 
			-- symbol model library:
			get_symbol_model (sym_name, sym_cursor);

			-- Get the port positions via the symbol cursor:
			result := get_port_positions (sym_cursor);
		end if;

		return result;
	end get_port_positions;

	

	
end et_device_model_unit_external;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
