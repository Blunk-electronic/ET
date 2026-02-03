------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        DEVICE MODEL / READ UNIT                          --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- 
-- To Do:
-- - clean up
--
--


with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;

with ada.exceptions;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_schematic_geometry;			use et_schematic_geometry;
use et_schematic_geometry.pac_geometry_2;

with et_file_sections;				use et_file_sections;

with et_symbol_read;				use et_symbol_read;
with et_symbol_name;				use et_symbol_name;
with et_symbol_library;				use et_symbol_library;

with et_unit_name;					use et_unit_name;
with et_unit_swap_level;
with et_unit_add_level;

with et_keywords;					use et_keywords;


package body et_device_read_unit is
	
	
	unit_name		: pac_unit_name.bounded_string; -- IO_BANK_2
	unit_position	: type_vector_model := origin; -- the position of the unit inside the device editor
	unit_swap_level	: et_unit_swap_level.type_swap_level := et_unit_swap_level.swap_level_default;
	unit_add_level	: et_unit_add_level.type_add_level := et_unit_add_level.add_level_default;

	unit_external 	: type_unit_external;
	unit_external_model_name : pac_symbol_model_name.bounded_string;
	
	


-- INTERNAL UNIT:
	
	procedure read_unit_internal (
		line : in type_fields_of_line)
	is
		kw : string := f (line, 1);
		use et_unit_swap_level;
		use et_unit_add_level;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_name then
			expect_field_count (line, 2);
			unit_name := to_unit_name (f (line, 2));

			-- Create a new symbol where symbol_model is pointing at.
			-- The symbol assumes the appearance of the device.
			-- The symbol will be copied to the current unit later.
			case appearance is
				when APPEARANCE_VIRTUAL =>
					symbol_model := new type_symbol_model' (
						appearance	=> APPEARANCE_VIRTUAL,
						others		=> <>);

				when APPEARANCE_PCB =>
					symbol_model := new type_symbol_model' (
						appearance	=> APPEARANCE_PCB,
						others		=> <>);

				when others => 
					raise constraint_error; -- CS

			end case;
			
		elsif kw = keyword_position then -- position x 0.00 y 0.00
			expect_field_count (line, 5);

			-- extract unit position starting at field 2
			-- NOTE: this is the position of the unit inside the device editor !
			unit_position := to_vector_model (line, 2);

		elsif kw = keyword_swap_level then
			expect_field_count (line, 2);
			unit_swap_level := to_swap_level (f (line, 2));

		elsif kw = keyword_add_level then
			expect_field_count (line, 2);
			unit_add_level := to_add_level (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_unit_internal;
	


	
	
	
	procedure insert_unit_internal (
		symbol			: in type_symbol_model_access;
		log_threshold	: in type_log_level)
	is
		position : pac_units_internal.cursor;
		inserted : boolean;

		use pac_unit_name;
		use et_unit_swap_level;
		use et_unit_add_level;
	begin
		-- Depending on the appearance of the device, a unit with the same
		-- appearance is inserted in units_internal.
		case appearance is 
			when APPEARANCE_VIRTUAL =>
				pac_units_internal.insert (
					container	=> units_internal,
					position	=> position,
					inserted	=> inserted,
					key			=> unit_name,
					new_item	=> (
							appearance	=> APPEARANCE_VIRTUAL,
							symbol		=> symbol_model.all,
							position	=> unit_position,
							swap_level	=> unit_swap_level,
							add_level	=> unit_add_level));

			when APPEARANCE_PCB =>
				pac_units_internal.insert (
					container	=> units_internal,
					position	=> position,
					inserted	=> inserted,
					key			=> unit_name,
					new_item	=> (
							appearance	=> APPEARANCE_PCB,
							symbol		=> symbol_model.all,
							position	=> unit_position,
							swap_level	=> unit_swap_level,
							add_level	=> unit_add_level));

		end case;

		-- A unit name must occur only once. 
		-- Make sure the unit_name is not in use by any internal or external units:
		
		-- Test occurence in internal units:
		if not inserted then
			log (ERROR, "unit " & to_string (unit_name) 
				& " already used by another internal unit !", console => true);
			raise constraint_error;
		end if;

		-- Make sure the unit name is not in use by any external unit:
		if pac_units_external.contains (units_external, unit_name) then
			log (ERROR, "unit name " & to_string (unit_name) 
				& " already used by an external unit !", console => true);
			raise constraint_error;
		end if;
		
		-- clean up for next unit
		unit_name := to_unit_name ("");
		unit_position := origin;
		unit_swap_level := swap_level_default;
		unit_add_level := add_level_default;
		symbol_model := null;
		
	end insert_unit_internal;

	
	

	


-- EXTERNAL UNIT:
	
	
	procedure read_unit_external (
		line : in type_fields_of_line)
	is
		use et_unit_swap_level;
		use et_unit_add_level;
		kw : string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_name then -- name A, B, ...
			expect_field_count (line, 2);
			unit_name := to_unit_name (f (line, 2));

		elsif kw = keyword_position then -- position x 0.00 y 0.00
			expect_field_count (line, 5);

			-- extract unit position starting at field 2
			-- NOTE: this is the position of the unit inside the device editor !
			unit_external.position := to_vector_model (line, 2);

		elsif kw = keyword_swap_level then -- swap_level 1
			expect_field_count (line, 2);
			unit_external.swap_level := to_swap_level (f (line, 2));

		elsif kw = keyword_add_level then -- add_level next
			expect_field_count (line, 2);
			unit_external.add_level := to_add_level (f (line, 2));

		elsif kw = keyword_symbol_file then -- symbol_model libraries/symbols/nand.sym
			expect_field_count (line, 2);
			unit_external_model_name := to_file_name (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_unit_external;
	
	

	
	

	
	procedure insert_unit_external (
		log_threshold	: in type_log_level)
	is
		use pac_unit_name;
		
		-- CS log messages
		
		
		procedure read_symbol_model is
		begin
			-- read the symbol model (like ../libraries/symbols/power_gnd.sym)
			read_symbol (unit_external_model_name,
				log_threshold + 1);			
		end read_symbol_model;
		
		
		
		inserted : boolean;
		
		-- Adds the internal unit to the device:
		procedure add_to_device is 
			cursor : pac_units_external.cursor;
		begin
			pac_units_external.insert (
				container	=> units_external,
				position	=> cursor,
				inserted	=> inserted,
				key			=> unit_name,
				new_item	=> unit_external);
		end add_to_device;
		
		
		
		-- Tests the "inserted" flag and issues a log message.
		-- The inserted-flag indicates that the unit does not exist
		-- already:			
		procedure check_for_name_in_use is begin
			-- A unit name must occur only once. 
			-- Make sure the unit_name is not in use by any internal or external units:

			-- Test occurence in external units:
			if not inserted then
				log (ERROR, "unit name " & to_string (unit_name) 
					& " already used by another external unit !");
				raise constraint_error;
			end if;

			-- Make sure the unit name is not in use by any internal unit:
			if pac_units_internal.contains (units_internal, unit_name) then
				log (ERROR, "unit name " & to_string (unit_name) 
					& " already used by an internal unit !");
				raise constraint_error;
			end if;			
		end check_for_name_in_use;
		
		
		
		procedure clean_up is begin
			-- clean up for next unit
			unit_name := to_unit_name ("");
			-- CS unit_external_model_name := 
			unit_external := (others => <>);		
		end clean_up;
		
		
	begin
		
		read_symbol_model;			
		
		-- Get the cursor to the symbol model:
		unit_external.model_cursor := get_symbol_model (unit_external_model_name);
		
		add_to_device;
		
		check_for_name_in_use;

		clean_up;
	end insert_unit_external;
		
		
	
end et_device_read_unit;
