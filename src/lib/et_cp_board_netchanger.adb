------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                 COMMAND PROCESSOR / BOARD / NETCHANGER                   --
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
-- - propose arguments if command incomplete
-- - set exit code if targeted object does not exist

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_runmode;						use et_runmode;

with et_board_coordinates;				use et_board_coordinates;
with et_schematic_coordinates;

with et_board_geometry;					use et_board_geometry;
with et_schematic_geometry;

with et_canvas_board;
with et_canvas_schematic;

with et_netchangers;					use et_netchangers;
-- with et_schematic_ops_submodules;		use et_schematic_ops_submodules;

with et_coordinates_abs_rel;			use et_coordinates_abs_rel;
with et_schematic_ops_netchangers;		use et_schematic_ops_netchangers;
with et_board_ops_netchangers;			use et_board_ops_netchangers;

with et_schematic_ops_groups;
with et_board_ops_groups;

with et_pcb_signal_layers;				use et_pcb_signal_layers;

with et_cmd_origin_to_commit;			use et_cmd_origin_to_commit;



package body et_cp_board_netchanger is

	use pac_generic_modules;
	use pac_geometry_2;




	procedure show_netchanger (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		


		
		procedure runmode_module is
			index : type_netchanger_id;

			-- This procedure shows the netchanger
			-- in the schematic drawing:
			procedure zoom_in_schematic is
				use et_schematic_coordinates;
				
				-- In order to zoom on the targeted netchanger
				-- we need its position in the schematic:
				position : type_object_position;
				
				use et_canvas_schematic;
				use pac_canvas;
			begin
				-- Zoom on the netchanger:
				position := get_netchanger_position (module, index);
				active_sheet := get_sheet (position);
				update_sheet_number_display;

				-- Zoom on the netchanger and leave the
				-- zoom factor as it is:
				zoom_to (get_place (position), S);

				-- Mark the netchanger as "selected":
				show_netchanger (
					module_cursor	=> module,
					index			=> index,
					log_threshold	=> log_threshold + 1);

			end zoom_in_schematic;


			procedure zoom_in_board is
				-- In order to zoom on the targeted netchanger
				-- we need its position on the board
				position : type_vector_model;
				
				use et_canvas_board;
				use et_canvas_board.pac_canvas;
			begin
				-- Zoom on the netchanger:
				position := get_netchanger_position (module, index);

				-- Zoom on the netchanger and leave the
				-- zoom factor as it is:
				zoom_to (position, S);
			end zoom_in_board;

			
		begin
			case cmd_field_count is
				-- "show netchanger 44"
				
				when 5 =>
					index := to_netchanger_id (get_field (cmd, 5)); -- 44

					-- Test whether the given netchanger exists:
					if netchanger_exists (module, index) then

						-- Show the netchanger in the
						-- schematic and in the board drawing:
						zoom_in_schematic;
						zoom_in_board;
						
					else
						netchanger_not_found (index);
					end if;
					
						
				when 6 .. type_field_count'last =>
					command_too_long (cmd, cmd_field_count - 1);
					
				when others => command_incomplete (cmd);
			end case;
		end runmode_module;


		
	begin
		log (text => "show netchanger", level => log_threshold);
		log_indentation_up;


		-- Show operations are only useful and possible in graphical
		-- runmode:
		case runmode is
			when MODE_MODULE =>

				-- Deselect all objects in the schematic
				-- and board drawing. This is required in case
				-- the specified netchanger does not exist. 
				-- It is redundant in case the specified netchanger
				-- does exist. The reset would be executed twice,
				-- the first time here and the second time
				-- by procedure show_netchanger in package 
				-- et_schematic_ops_netchangers:
				et_schematic_ops_groups.reset_objects (
					module, log_threshold + 1);
					
				et_board_ops_groups.reset_objects (
					module, log_threshold + 1);

				runmode_module;

			when others =>
				skipped_in_this_runmode (log_threshold + 1);
					
		end case;				


		log_indentation_down;
	end show_netchanger;


	



	
	

	procedure move_netchanger (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		index : type_netchanger_id;
	begin
		-- CS log message

		
		case cmd_field_count is
			when 8 =>
				-- Extract the index of the targeted netchanger:
				index := to_netchanger_id (get_field (cmd, 5)); -- 1,2,3, ...

				
				if netchanger_exists (module, index) then
					
					move_netchanger (
						module_cursor	=> module,
						index			=> index,
						coordinates		=> to_coordinates (get_field (cmd, 6)),  -- relative/absolute
						point			=> type_vector_model (set (
											x => to_distance (get_field (cmd, 7)),
											y => to_distance (get_field (cmd, 8)))),						

						-- Depending on the origin of the command,
						-- the design state is to be commited or not:
						commit_design	=> to_commit_design (cmd),

						log_threshold	=> log_threshold + 1);
				else
					netchanger_not_found (index);
				end if;

				
			when 9 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end move_netchanger;



	




	procedure drag_netchanger (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		-- CS log message
		null;
		
-- 		case cmd_field_count is
-- 			when 8 =>
-- 				drag_netchanger (
-- 					module_name 	=> key (module),
-- 					index			=> to_netchanger_id (get_field (cmd, 5)), -- 1,2,3,...
-- 					coordinates		=> to_coordinates (get_field (cmd, 6)), -- relative/absolute
-- 					point			=> type_vector_model (set (
-- 										x => to_distance (get_field (cmd, 7)),
-- 										y => to_distance (get_field (cmd, 8)))),
-- 					log_threshold	=> log_threshold + 1);
-- 
-- 			when 9 .. type_field_count'last =>
-- 				command_too_long (cmd, cmd_field_count - 1);
-- 				
-- 			when others => command_incomplete (cmd);
-- 		end case;		
	end drag_netchanger;




	


	procedure set_netchanger_layer (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		index : type_netchanger_id;
		layer : type_signal_layer;
	begin
		-- CS log message

		
		case cmd_field_count is
			when 7 =>
				-- Extract the index of the targeted netchanger:
				index := to_netchanger_id (get_field (cmd, 5)); -- 1,2,3, ...

				
				if netchanger_exists (module, index) then

					-- CS: test keyword "layer" on place 6
					
					layer := to_signal_layer (get_field (cmd, 7));
					-- CS: make sure the signal layer 
					-- exists in the module.
					
					set_netchanger_layer (
						module_cursor	=> module,
						index			=> index,
						layer			=> layer,

						-- Depending on the origin of the command,
						-- the design state is to be commited or not:
						commit_design	=> to_commit_design (cmd),

						log_threshold	=> log_threshold + 1);
				else
					netchanger_not_found (index);
				end if;

				
			when 8 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end set_netchanger_layer;


	
	
	
end et_cp_board_netchanger;


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
