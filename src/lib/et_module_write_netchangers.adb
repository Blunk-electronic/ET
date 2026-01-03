------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      MODULE WRITE / NETCHANGERS                          --
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
with et_section_headers;			use et_section_headers;

with et_schematic_geometry;
with et_board_geometry;
with et_pcb_signal_layers;			use et_pcb_signal_layers;

with et_schematic_coordinates;
with et_netchangers;				use et_netchangers;
with et_net_names;					use et_net_names;
with et_coordinates_formatting;		use et_coordinates_formatting;


with et_general_rw;					use et_general_rw;



package body et_module_write_netchangers is

	use pac_generic_modules;


	procedure write_netchangers (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		use pac_netchangers;


		procedure query_netchanger (cursor : pac_netchangers.cursor) is
			use et_schematic_geometry;
			use pac_geometry_2;
			use et_schematic_coordinates;	
		begin
			section_mark (section_netchanger, HEADER);
			write (keyword => keyword_name,	parameters => to_string (key (cursor))); -- 1, 2, 201, ...
			write (keyword => keyword_position_in_schematic, 
				parameters => to_string (element (cursor).position_sch, FORMAT_2)); -- position_in_schematic sheet 1 x 147.32 y 96.97

			write (
				keyword => keyword_rotation_in_schematic, 
				parameters => to_string (get_rotation (element (cursor).position_sch))); -- rotation_in_schematic 90.0

			write (
				keyword => keyword_position_in_board, 
				parameters => et_board_geometry.pac_geometry_2.to_string (
					element (cursor).position_brd, FORMAT_2)); -- position_in_board x 1.32 y 6.97
			
			write (keyword => keyword_layer, parameters => to_string (element (cursor).layer)); -- layer 2
			section_mark (section_netchanger, FOOTER);
		end query_netchanger;


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			section_mark (section_netchangers, HEADER);
			iterate (module.netchangers, query_netchanger'access);
			section_mark (section_netchangers, FOOTER);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " write netchangers", level => log_threshold);
			
		log_indentation_up;		
		query_element (module_cursor, query_module'access);
		log_indentation_down;

	end write_netchangers;

	
	
	
end et_module_write_netchangers;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
