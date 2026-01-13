------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE WRITE / PCB LAYER STACK                        --
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

with et_board_ops;
with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_package_sections;			use et_package_sections;

-- with et_pcb_stack;					use et_pcb_stack;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_design_rules_board;			use et_design_rules_board;
with et_board_geometry;				use et_board_geometry;

with et_file_write;					use et_file_write;


package body et_module_write_pcb_layer_stack is

	use pac_generic_modules;
	use pac_geometry_2;

	use package_layers;
	

	
	procedure query_layers (cursor : in package_layers.cursor) is
		layer : type_layer := element (cursor);
	begin
		-- write: "conductor   1 0.035"
		write (keyword => keyword_conductor,
			parameters => "  "
			& to_string (to_index (cursor)) 
			& to_string (layer.conductor.thickness));

		-- write "dielectric  1 0.200"
		write (keyword => keyword_dielectric, 
			parameters => " "
			& to_string (to_index (cursor)) 
			& to_string (layer.dielectric.thickness));

	end;


	

	

	procedure write_layer_stack (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			bottom_layer : type_signal_layer;
			bottom_layer_thickness : type_conductor_thickness;
		begin
			
			section_mark (section_board_layer_stack, HEADER);

			-- iterate layers starting at top layer (1) until the deepest inner layer.
			-- The bottom layer is not part of the layer list and will be written later.
			iterate (module.board.stack.layers, query_layers'access);

			-- The bottom layer number is the deepest inner layer plus one:
			bottom_layer := last_index (module.board.stack.layers) + 1;

			-- Get the bottom conductor thickness:
			bottom_layer_thickness := module.board.stack.bottom.thickness;

			-- Write the bottom layer in the file.
			write (keyword => keyword_conductor,
				parameters => space & to_string (bottom_layer) & to_string (bottom_layer_thickness) &
				space & comment_mark_default & " bottom signal layer");
			
			section_mark (section_board_layer_stack, FOOTER);
		end query_module;


		
	begin	
		log (text => "module " & to_string (module_cursor)
			 & " write layer stack",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_layer_stack;


	
end et_module_write_pcb_layer_stack;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
