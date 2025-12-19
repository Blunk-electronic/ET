------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / DESIGN RULES                            --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
-- - rename global variables
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;

with et_design_rules;				use et_design_rules;
with et_design_rules_board;			use et_design_rules_board;


with et_general_rw;					use et_general_rw;



package body et_module_read_design_rules is

	use pac_generic_modules;


	rules : type_design_rules := (others => <>);
	-- 	rules_layout	: et_design_rules.pac_file_name.bounded_string;
		-- CS ERC rules ?


		

	procedure read_rules (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		if kw = keyword_layout then -- layout JLP_ML4_standard.dru
			rules.layout := to_file_name (f (line, 2));
		end if;
	end read_rules;

	
	

		

	procedure set_rules (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			-- assign rules
			module.rules := rules;

			-- log and read layout design rules if specified. otherwise skip:
			if not is_empty (rules.layout) then
				log (text => keyword_layout & space & to_string (module.rules.layout),
					level => log_threshold + 2);

				-- Read the DRU file like JLP_ML4_standard.dru and store it
				-- in project wide container et_design_rules.design_rules.
				read_rules (rules.layout, log_threshold + 3);
			else
				log (WARNING, "No layout design rules specified ! Defaults will be applied !");
			end if;
				
			-- CS module.rules.erc ?
		end;
		
	begin
		log (text => "design rules ...", level => log_threshold + 1);
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		log_indentation_down;
	end set_rules;



	
	
end et_module_read_design_rules;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
