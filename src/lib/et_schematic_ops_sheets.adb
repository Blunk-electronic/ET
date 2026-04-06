------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON SHEETS                        --
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


with et_module_names;					use et_module_names;
with et_schematic_ops_nets;
with et_schematic_ops_units;
with et_schematic_ops_netchangers;

with et_board_ops_ratsnest;
with et_modes.schematic;
with et_undo_redo;
with et_commit;



package body et_schematic_ops_sheets is


	use pac_text_schematic;

	
	procedure sheet_not_found (
		sheet : in type_sheet)
	is begin
		log (WARNING, "Sheet no. " & to_string (sheet) & " not found !");
	end;

	
	
	
	
	function get_sheet_count (
		module	: in pac_generic_modules.cursor)
		return type_sheet
	is
		result : type_sheet;

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			result := get_sheet_count (module.frames);
		end query_module;
		
	begin
		query_element (module, query_module'access);
		return result;
	end get_sheet_count;
	

	
	
	
	
	
	
	
	function sheet_exists (
		module	: in pac_generic_modules.cursor;
		sheet	: in type_sheet)
		return boolean
	is
		result : boolean := false;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			result := sheet_exists (module.frames, sheet);
		end query_module;

		
	begin
		query_element (module, query_module'access);
		return result;
	end sheet_exists;
		
		
	
	
	
	
	function get_sheet_description (
		module	: in pac_generic_modules.cursor;
		sheet	: in type_sheet)
		return type_schematic_description 
	is

		use pac_schematic_descriptions;
		cursor : pac_schematic_descriptions.cursor;


		
		procedure query_descriptions (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			cursor := find (module.frames.descriptions, sheet);
		end query_descriptions;

		
	begin
		query_element (
			position	=> module,
			process		=> query_descriptions'access);
		
		if cursor /= pac_schematic_descriptions.no_element then
			return element (cursor);
		else
		-- If the sheet has no description, then return the defaults.
			return (others => <>);
		end if;
	end get_sheet_description;








	


	procedure set_sheet_category (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		category		: in type_schematic_sheet_category;
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			set_category (
				frames	=> module.frames,
				sheet	=> sheet,
				cat		=> category);

		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " set sheet " & to_string (sheet) 
			 & " category " & to_string (category),
			level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);
		
		log_indentation_down;
	end set_sheet_category;


	
	
	
	
	
	
	
	procedure delete_sheet (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is
		use et_board_ops_ratsnest;
		use et_modes.schematic;
		use et_undo_redo;
		use et_commit;
	
	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is 
			use et_schematic_ops_nets;
			use et_schematic_ops_units;
			use et_schematic_ops_netchangers;
		begin
			-- Delete all nets on the given sheet:
			delete_nets (module_cursor, sheet, log_threshold + 1);
			
			-- Move all strands of nets on following sheets
			-- downward by one sheet:
			move_strands_on_sheet_delete (
				module_cursor	=> module_cursor, 
				sheet_delete	=> sheet,
				log_threshold	=> log_threshold + 1);
				
			
			-- Delete all units on the given sheet:
			delete_units (module_cursor, sheet, log_threshold + 1);

			-- Move all units on following sheets
			-- downward by one sheet:
			move_units_on_sheet_delete (
				module_cursor	=> module_cursor, 
				sheet_delete	=> sheet,
				log_threshold	=> log_threshold + 1);

			
			-- Delete all netchangers on the given sheet:
			delete_netchangers (module_cursor, sheet, log_threshold + 1);

			-- CS: delete submodules, texts, ...
			
			-- CS delete_sheet (module.frames, sheet);
			-- This procedure is not complete yet.
		end query_module;

	
	
	begin
		log (text => "module " & to_string (module_cursor)
			 & " delete sheet " & to_string (sheet), 
			level => log_threshold);

		log_indentation_up;

		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold + 1);
		end if;
		
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);
		
		
		
		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold + 1);
		end if;

		
		update_ratsnest (module_cursor, log_threshold + 1);
				
		log_indentation_down;
	end delete_sheet;

	
end et_schematic_ops_sheets;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
