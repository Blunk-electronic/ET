------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            DEVICE MODEL                                  --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                               --
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


package body et_device_model is



	function get_port_positions (
		unit	: in pac_units_internal.cursor)
		return pac_points.list
	is
		result : pac_points.list;
	begin
		-- If the given cursor points to a unit, then
		-- extract the port positions. Otherwise return
		-- an empty list:		
		if has_element (unit) then
			result := get_port_positions (element (unit).symbol);
		end if;
		
		return result;
	end get_port_positions;




	function get_symbol_model_file (
		unit	: in pac_units_external.cursor)
		return pac_symbol_model_file.bounded_string
	is begin
		return element (unit).model;
	end get_symbol_model_file;
	

	

	function get_port_positions (
		unit	: in pac_units_external.cursor)
		return pac_points.list
	is
		result : pac_points.list;
		
		-- The name of the associated symbol model file:
		sym_name : pac_symbol_model_file.bounded_string;
		-- like /libraries/symbols/NAND.sym

		-- The cursor of the actual symbol in
		-- the symbol library:
		sym_cursor : pac_symbols.cursor;
	
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
			locate_symbol (sym_name, sym_cursor);

			-- Get the port positions via the symbol cursor:
			result := get_port_positions (sym_cursor);
		end if;

		return result;
	end get_port_positions;

	

	
	procedure locate_internal (
		model	: in type_device_model;
		unit	: in pac_unit_name.bounded_string;
		cursor	: in out pac_units_internal.cursor)
	is begin
		cursor := model.units_internal.find (unit);
	end locate_internal;



	procedure locate_external (
		model	: in type_device_model;
		unit	: in pac_unit_name.bounded_string;
		cursor	: in out pac_units_external.cursor)
	is begin
		cursor := model.units_external.find (unit);
	end locate_external;

	
	

	function get_unit_count (
		device_model : in type_device_model)
		return type_unit_count
	is
		result : type_unit_count;
	begin
		result := type_unit_count (
			device_model.units_internal.length +
			device_model.units_external.length);
		
		return result;
	end get_unit_count;





	function get_first_package_variant (
		device_model : in type_device_model)
		return pac_package_variant_name.bounded_string
	is begin
		return get_first_package_variant (device_model.variants);
	end;



	function get_default_value (
		device_model : in type_device_model)
		return pac_device_value.bounded_string
	is begin
		return device_model.value;
	end;

	
	
end et_device_model;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
