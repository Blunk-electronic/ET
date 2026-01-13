------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE WRITE / DESIGN RULES                           --
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
--                                                                          --
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
-- ToDo:
-- - clean up
--
--
--

with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;

with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_file_sections;				use et_file_sections;

with et_design_rules;				use et_design_rules;
with et_design_rules_board;			use et_design_rules_board;

with et_file_write;					use et_file_write;


package body et_module_write_design_rules is

	use pac_generic_modules;


	

	procedure write_design_rules (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 
			rules : type_design_rules := get_design_rules (module);
		begin
			section_mark (section_rules, HEADER);

			-- Write the board design rules. If none assigned to the
			-- module, write nothing:
			if design_rules_board_assigned (module) then
				write (keyword => keyword_layout, parameters => to_string (rules.layout));
				-- CS keyword should be "board" instead of "layout"
			end if;

			-- CS schematic rules
			
			section_mark (section_rules, FOOTER);
		end;
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " write design rules",
			level => log_threshold);
		
		log_indentation_up;
		
		query_element (module_cursor, query_module'access);

		log_indentation_down;
	end write_design_rules;



	
	
end et_module_write_design_rules;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
