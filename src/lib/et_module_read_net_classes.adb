------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / NET CLASSES                             --
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
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;

with et_board_geometry;

with et_net_class;					use et_net_class;
with et_net_class_name;				use et_net_class_name;
with et_net_class_description;		use et_net_class_description;
with et_net_classes;				use et_net_classes;

with et_drills;
with et_design_rules_board;			use et_design_rules_board;




package body et_module_read_net_classes is

	use pac_generic_modules;


	net_class 		: et_net_class.type_net_class;
	net_class_name	: et_net_class_name.pac_net_class_name.bounded_string;

	
	
	
	procedure reset_net_class is 
	begin
		net_class_name := net_class_name_default;
		net_class := (others => <>);

		-- CS reset parameter-found-flags
	end reset_net_class;


		
		
		
	procedure read_net_class (
		line 			: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is 
		use et_drills;
		use et_board_geometry.pac_geometry_2;
		kw : constant string := f (line, 1);
	begin
		log (text => "read net class", level => log_threshold + 1);
		log_indentation_up;

		
		if kw = keyword_name then
			expect_field_count (line, 2);
			net_class_name := to_net_class_name (f (line,2));

		-- CS: In the following: set a corresponding parameter-found-flag
		elsif kw = keyword_description then
			expect_field_count (line, 2);
			net_class.description := to_net_class_description (f (line,2));
			
		elsif kw = keyword_clearance then
			expect_field_count (line, 2);
			net_class.clearance := to_distance (f (line,2));
			validate_track_clearance (net_class.clearance);
			-- CS validate against dru settings
												
		elsif kw = keyword_track_width_min then
			expect_field_count (line, 2);
			net_class.track_width_min := to_distance (f (line,2));
			validate_track_width (net_class.track_width_min);
			-- CS validate against dru settings
			
		elsif kw = keyword_via_drill_min then
			expect_field_count (line, 2);
			net_class.via_drill_min := to_distance (f (line,2));
			validate_drill_size (net_class.via_drill_min);
			-- CS validate against dru settings
			
		elsif kw = keyword_via_restring_min then
			expect_field_count (line, 2);
			net_class.via_restring_min := to_distance (f (line,2));
			validate_restring_width (net_class.via_restring_min);
			-- CS validate against dru settings
			
		elsif kw = keyword_micro_via_drill_min then
			expect_field_count (line, 2);
			net_class.micro_via_drill_min := to_distance (f (line,2));
			validate_drill_size (net_class.micro_via_drill_min);
			-- CS validate against dru settings
			
		elsif kw = keyword_micro_via_restring_min then
			expect_field_count (line, 2);
			net_class.micro_via_restring_min := to_distance (f (line,2));
			validate_restring_width (net_class.micro_via_restring_min);
			-- CS validate against dru settings
		else
			invalid_keyword (kw);
		end if;

		
		log_indentation_down;
	end read_net_class;



		
		
		
		

	procedure assign_net_class (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			inserted : boolean;
			cursor : pac_net_classes.cursor;
		begin
			log (text => "net class " & to_string (net_class_name), level => log_threshold + 1);

			-- CS: notify about missing parameters (by reading the parameter-found-flags)
			-- If a parameter is missing, the default is assumed. See type_net_class spec.
			
			pac_net_classes.insert (
				container	=> module.net_classes,
				key			=> net_class_name,
				new_item	=> net_class,
				inserted	=> inserted,
				position	=> cursor);

			if not inserted then
				log (ERROR, "net class '" & to_string (net_class_name) 
						& "' already exists !");
				raise constraint_error;
			end if;			
		end query_module;

			
	begin
		log (text => "module " & to_string (module_cursor)
			& " assign net class", level => log_threshold);
			
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		reset_net_class; -- clean up for next net class
		
		log_indentation_down;
	end assign_net_class;


	
	
end et_module_read_net_classes;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
