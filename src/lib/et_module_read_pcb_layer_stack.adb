------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / PCB LAYER STACK                         --
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
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_design_rules_board;			use et_design_rules_board;
with et_board_geometry;				use et_board_geometry;




package body et_module_read_pcb_layer_stack is

	use pac_generic_modules;
	use pac_geometry_2;



	conductor_layer, dielectric_layer : type_signal_layer := 
		type_signal_layer'first;
	
	conductor_thickness : type_conductor_thickness := 
		conductor_thickness_outer_default;

	
	board_layer : type_layer;

	board_layers : package_layers.vector;
	
	dielectric_found : boolean := false;



	

	procedure read_layer (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level)					 
	is
		kw : constant string := f (line, 1);
		use package_layers;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_conductor then -- conductor 1 0.035
			expect_field_count (line, 3);
			conductor_layer := to_signal_layer (f (line, 2));
			conductor_thickness := to_distance (f (line, 3));
			board_layer.conductor.thickness := conductor_thickness;

			-- Layer numbers must be continuous from top to bottom.
			-- After the dielectric of a layer the next conductor layer must
			-- have the next number:
			if dielectric_found then
				if to_index (board_layers.last) /= conductor_layer - 1 then
					log (ERROR, "expect conductor layer number" &
						to_string (to_index (board_layers.last) + 1) & " !",
						console => true);
					raise constraint_error;
				end if;
			end if;
			
			dielectric_found := false;

		elsif kw = keyword_dielectric then -- dielectric 1 1.5
			expect_field_count (line, 3);
			dielectric_layer := to_signal_layer (f (line, 2));
			board_layer.dielectric.thickness := to_distance (f (line, 3));
			dielectric_found := true;
			
			if dielectric_layer = conductor_layer then
				append (board_layers, board_layer);
			else
				log (ERROR, "expect dielectric layer number" & to_string (conductor_layer) & " !", console => true);
				raise constraint_error;
			end if;
		else
			invalid_keyword (kw);
		end if;
	end;





	

	procedure add_board_layer (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			log (text => "board layer stack", level => log_threshold + 1);

			-- Copy the collected layers (except the bottom conductor layer) into the module:
			module.board.stack.layers := board_layers;

			-- If the last entry was "conductor n t" then we assume that this
			-- was the bottom conductor layer (it does not have a dielectric layer underneath).
			if not dielectric_found then
				module.board.stack.bottom.thickness := conductor_thickness;
			else
				log (ERROR, "dielectric not allowed underneath the bottom conductor layer !", console => true);
				raise constraint_error;
			end if;
			
			-- reset layer values:
			dielectric_found := false;
			conductor_layer := type_signal_layer'first;
			dielectric_layer := type_signal_layer'first;
			conductor_thickness := et_pcb_stack.conductor_thickness_outer_default;
			board_layer := (others => <>);
			package_layers.clear (board_layers);

		end do_it;


	begin	
		log (text => "module " & to_string (module_cursor)
			 & " add signal layer",
			 level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		log_indentation_down;
	end add_board_layer;


	
end et_module_read_pcb_layer_stack;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
