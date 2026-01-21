------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    BOARD OPERATIONS / SIGNAL LAYERS                      --
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
-- To Do:
-- - rework



with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;
-- with ada.exceptions;				use ada.exceptions;

with et_string_processing;			use et_string_processing;




package body et_board_ops_signal_layers is


	
	
	procedure add_layer (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		layer			: in et_pcb_stack.type_layer; -- incl. conductor and dieelectic thickness
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified


		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use et_pcb_stack.package_layers;
		begin
			append (module.board.stack.layers, layer);
		end add;

		
	begin -- add_layer
		log (text => "module " & to_string (module_name) &
			" adding layer conductor thickness" & to_string (layer.conductor.thickness) &
			" dielectic thickness" & to_string (layer.dielectric.thickness),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);
		
	end add_layer;


	
	
	function get_layer_count (
		module_cursor	: in pac_generic_modules.cursor) 
		return type_signal_layer 
	is
		use package_layers;
	begin
		return last_index (element (module_cursor).board.stack.layers) + 1;
	end;



	
	procedure test_layer (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in type_signal_layer) 
	is
		layers_used : type_signal_layer := get_layer_count (module_cursor);
	begin
		if layer > layers_used then
			log (ERROR, "Layer " & to_string (layer) & " invalid !" &
				 " The current layer stack allows only " & to_string (layers_used) & " layers !",
				 console => true);
			raise constraint_error;
		end if;
	end;


	
	
	procedure delete_layer (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		layer			: in type_signal_layer;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use package_layers;

			-- get the total number of layers used by the module
			layers_used : type_signal_layer := get_layer_count (module_cursor);

			old_stack : package_layers.vector := element (module_cursor).board.stack.layers;
			new_stack : package_layers.vector;
		begin
			-- The bottom layer can not be deleted:
			if layer = layers_used then
				log (WARNING, "The bottom layer" & to_string (layer) & " can not be deleted !");

			-- The layer must not be greater than the total number of layers:
			elsif layer > layers_used then
				log (WARNING, "layer" & to_string (layer) & " does not exist. " &
					 "The board uses only" & to_string (layers_used) & " layers !");
			else
				-- Rebuild the layer stack by copying the old layers one by one
				-- to the new layer stack. The layer to be deleted is skipped:
				for i in first_index (old_stack) .. last_index (old_stack) loop
					if i /= layer then
						append (new_stack, element (old_stack, i));
					end if;
				end loop;

				module.board.stack.layers := new_stack;
			end if;
		end delete;

		
	begin -- delete_layer
		log (text => "module " & to_string (module_name) &
			" deleting layer" & to_string (layer),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_layer;
	


	





	
	
	procedure test_layers (
		module_cursor	: in pac_generic_modules.cursor;
		layers 			: in pac_signal_layers.set)
	is
		use pac_signal_layers;
		
		procedure query_layer (cursor : in pac_signal_layers.cursor) is
		begin
			test_layer (module_cursor, element (cursor));
		end;
		
	begin
		iterate (layers, query_layer'access);
	end;



	

	function get_deepest_conductor_layer (
		module	: in pac_generic_modules.cursor)
		return type_signal_layer 
	is 
		result : type_signal_layer;
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is begin
			result := get_deepest_layer (module.board.stack);
		end query_module;
		
		
	begin
		query_element (module, query_module'access);
		return result;
	end get_deepest_conductor_layer;



		

end et_board_ops_signal_layers;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
