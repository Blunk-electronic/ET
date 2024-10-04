------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS GRID                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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

with ada.text_io;					use ada.text_io;


package body et_schematic_ops.grid is

	
	procedure set_grid (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		grid			: in pac_grid.type_grid;
		log_threshold	: in type_log_level) 
	is
		use pac_generic_modules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is begin
			-- Set the grid in the database:
			module.grid := grid;
		end;

		
	begin -- set_grid
		log (text => "module " & enclose_in_quotes (to_string (module_name))
			& " setting schematic grid" & to_string (grid.spacing),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

	end set_grid;


	
	
	procedure set_grid (
		module_cursor	: in pac_generic_modules.cursor;
		grid			: in pac_grid.type_grid;
		log_threshold	: in type_log_level) 
	is
		use pac_generic_modules;

		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is begin
			-- Set the grid in the database:
			module.grid := grid;
		end;

		
	begin -- set_grid
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor)))
			& " setting schematic grid" & to_string (grid.spacing),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

	end set_grid;

	


	function get_grid (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return pac_grid.type_grid
	is
		use pac_grid;
		use pac_generic_modules;

		result : type_grid;

		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is begin
			-- Get the grid from the database:
			result := module.grid;
		end;

		
	begin
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor)))
			& " getting schematic grid",
			level => log_threshold);

		query_element (
			position	=> module_cursor,
			process		=> do_it'access);

		return result;
	end get_grid;

	
	
end et_schematic_ops.grid;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
