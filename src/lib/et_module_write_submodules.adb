------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     MODULE WRITE / SUBMODULES                            --
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
with et_module_instance;			use et_module_instance;
with et_keywords;					use et_keywords;
with et_package_sections;			use et_package_sections;

with et_schematic_geometry;
with et_board_geometry;

with et_schematic_coordinates;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_netchangers;				use et_netchangers;
with et_submodules;					use et_submodules;
with et_net_names;					use et_net_names;

with et_file_write;					use et_file_write;


package body et_module_write_submodules is

	use pac_generic_modules;

	

	

	procedure write_submodules (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use et_schematic_geometry.pac_geometry_2;

		use pac_submodules;
		use pac_net_name;


		
		procedure query_ports (port_cursor : in pac_submodule_ports.cursor) is
			use pac_submodule_ports;
		begin
			section_mark (section_port, HEADER);
			write (keyword => keyword_name, parameters => to_string (key (port_cursor))); -- name clk_out

			write (keyword => keyword_position, 
				parameters => to_string (element (port_cursor).position, FORMAT_2)); -- position x 0 y 10
			
			write (keyword => keyword_direction, parameters => to_string (element (port_cursor).direction)); -- direction master/slave
			section_mark (section_port, FOOTER);
		end;


		
		procedure write (submodule_cursor : in pac_submodules.cursor) is 
			use et_schematic_coordinates;
			use et_schematic_geometry.pac_geometry_2;
		begin
			section_mark (section_submodule, HEADER);
			write (keyword => keyword_name, parameters => to_string (key (submodule_cursor))); -- name stepper_driver_1
			write (keyword => keyword_file, parameters => pac_submodule_path.to_string (element (submodule_cursor).file)); -- file $ET_TEMPLATES/motor_driver.mod

			write (keyword => keyword_position, 
				parameters => to_string (element (submodule_cursor).position, FORMAT_2));
			
			write (keyword => keyword_size, parameters => 
				space & keyword_x & to_string (element (submodule_cursor).size.x) &
				space & keyword_y & to_string (element (submodule_cursor).size.y)); -- size x 50 y 70
			
			write (keyword => keyword_position_in_board, parameters => -- position_in_board x 23 y 0.2 rotation 90.0
				et_board_geometry.pac_geometry_2.to_string (element (submodule_cursor).position_in_board));

			write (keyword => keyword_view_mode, parameters => to_string (element (submodule_cursor).view_mode));

			section_mark (section_ports, HEADER);
			et_submodules.pac_submodule_ports.iterate (element (submodule_cursor).ports, query_ports'access);
			section_mark (section_ports, FOOTER);
			
			section_mark (section_submodule, FOOTER);				
		end write;


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			section_mark (section_submodules, HEADER);
			iterate (module.submods, write'access);
			section_mark (section_submodules, FOOTER);
		end query_module;
	
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " write submodules",
		level => log_threshold);
		
		log_indentation_up;
		
		query_element (module_cursor, query_module'access);
		log_indentation_down;		
	end write_submodules;
	
	
		
	
	
end et_module_write_submodules;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
