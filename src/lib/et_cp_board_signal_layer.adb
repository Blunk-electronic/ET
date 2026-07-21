------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--             COMMAND PROCESSOR / BOARD / SIGNAL LAYER                     --
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

--   For correct displaying set tab with in your editor to 4.

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
--
--




with et_string_processing;			use et_string_processing;
with et_board_ops_signal_layers;	use et_board_ops_signal_layers;
with et_pcb_stack;						use et_pcb_stack;
with et_pcb_signal_layers;				use et_pcb_signal_layers;
with et_board_geometry;					use et_board_geometry;

with et_cmd_origin_to_commit;			use et_cmd_origin_to_commit;



package body et_cp_board_signal_layer is

	use pac_generic_modules;
	use pac_geometry_2;


	
	
	procedure add_signal_layer (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		
		procedure do_it is
			layer : type_layer;
		begin
			layer.conductor.thickness := to_distance (get_field (cmd, 5));
			layer.dielectric.thickness := to_distance (get_field (cmd, 6));
			
			add_layer (
				module_name 	=> key (module),
				layer			=> layer,

				-- Depending on the origin of the command,
				-- the design state is to be commited or not:
				commit_design	=> to_commit_design (cmd),
				log_threshold	=> log_threshold + 1);
		end do_it;
			

	begin
		log (text => "add signal layer", level => log_threshold);
		log_indentation_up;

		
		case cmd_field_count is
			when 6 =>
				do_it;

			when 7 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;		


		log_indentation_down;
	end add_signal_layer;




	
	

	

	
	procedure delete_signal_layer (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		procedure do_it is begin		
			delete_layer (
				module_name 	=> key (module),
				layer			=> to_signal_layer (get_field (cmd, 5)),

				-- Depending on the origin of the command,
				-- the design state is to be commited or not:
				commit_design	=> to_commit_design (cmd),
				log_threshold	=> log_threshold + 1);

		end do_it;

		
	begin
		log (text => "delete signal layer", level => log_threshold);
		log_indentation_up;

		
		case cmd_field_count is
			when 5 =>
				do_it;
		
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;


		log_indentation_down;
	end delete_signal_layer;


	
	
end et_cp_board_signal_layer;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
