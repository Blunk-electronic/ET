------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE WRITE / NET CLASSES                            --
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

with et_board_geometry;				use et_board_geometry;

with et_net_class;					use et_net_class;
with et_net_class_name;				use et_net_class_name;
with et_net_class_description;		use et_net_class_description;
with et_net_classes;				use et_net_classes;

with et_file_write;					use et_file_write;


package body et_module_write_net_classes is

	use pac_generic_modules;
		
	use pac_net_classes;
	use pac_geometry_2;


	
		

	procedure write_net_classes (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is


		procedure query_class (class_cursor : in pac_net_classes.cursor) is 
			name : string renames get_net_class_name (class_cursor);
			net_class : type_net_class renames element (class_cursor);
		begin
			log (text => "net class " & name, level => log_threshold + 1);
			section_mark (section_net_class, HEADER);

			write (keyword => keyword_name, parameters => name);
			write (keyword => keyword_description, parameters => to_string (net_class.description), wrap => true);
			write (keyword => keyword_clearance, parameters => to_string (net_class.clearance));
			write (keyword => keyword_track_width_min, parameters => to_string (net_class.track_width_min));
			write (keyword => keyword_via_drill_min, parameters => to_string (net_class.via_drill_min));
			write (keyword => keyword_via_restring_min, parameters => to_string (net_class.via_restring_min));
			write (keyword => keyword_micro_via_drill_min, parameters => to_string (net_class.micro_via_drill_min));
			write (keyword => keyword_micro_via_restring_min, parameters => to_string (net_class.micro_via_restring_min));

			section_mark (section_net_class, FOOTER);
		end;


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			section_mark (section_net_classes, HEADER);
			iterate (module.net_classes, query_class'access);
			section_mark (section_net_classes, FOOTER);		
		end query_module;

			
	begin
		log (text => "module " & to_string (module_cursor)
			& " write net classes", level => log_threshold);
			
		log_indentation_up;		
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_net_classes;


	
	
end et_module_write_net_classes;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
