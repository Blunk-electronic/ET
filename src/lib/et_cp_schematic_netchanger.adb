------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              COMMAND PROCESSOR / SCHEMATIC / NETCHANGER                  --
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
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_sheets;							use et_sheets;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_schematic_geometry;				use et_schematic_geometry;

with et_netchangers;					use et_netchangers;
with et_schematic_ops_netchangers;		use et_schematic_ops_netchangers;



package body et_cp_schematic_netchanger is

	use pac_generic_modules;
	use pac_geometry_2;




	procedure add_netchanger (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		-- CS log message 
		
		case cmd_field_count is
			when 8 =>
				add_netchanger (
					module_name 	=> key (module),
					place			=> to_position (
						sheet => to_sheet (get_field (cmd, 5)),
						point => type_vector_model (set 
									(
									x => to_distance (get_field (cmd, 6)),
									y => to_distance (get_field (cmd, 7))
									)),
						rotation		=> to_rotation (get_field (cmd, 8))),
					log_threshold	=> log_threshold + 1);
				

			when 9 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;		
	end add_netchanger;

	


	


	

	procedure move_netchanger (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 9 =>
				move_netchanger (
					module_name 	=> key (module),
					index			=> to_netchanger_id (get_field (cmd, 5)), -- 1,2,3, ...
					coordinates		=> to_coordinates (get_field (cmd, 6)),  -- relative/absolute
					sheet			=> to_sheet_relative (get_field (cmd, 7)),
					point			=> type_vector_model (set (
										x => to_distance (get_field (cmd, 8)),
										y => to_distance (get_field (cmd, 9)))),
						
					log_threshold	=> log_threshold + 1);

			when 10 .. type_field_count'last =>
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
		
		case cmd_field_count is
			when 8 =>
				drag_netchanger (
					module_name 	=> key (module),
					index			=> to_netchanger_id (get_field (cmd, 5)), -- 1,2,3,...
					coordinates		=> to_coordinates (get_field (cmd, 6)), -- relative/absolute
					point			=> type_vector_model (set (
										x => to_distance (get_field (cmd, 7)),
										y => to_distance (get_field (cmd, 8)))),
					log_threshold	=> log_threshold + 1);

			when 9 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;		
	end drag_netchanger;



	





	

	

	procedure delete_netchanger (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		-- CS log message

		case cmd_field_count is
			when 5 =>
				delete_netchanger (
					module_name		=> key (module),
					index			=> to_netchanger_id (get_field (cmd, 5)), -- 1,2,3,...
					log_threshold	=> log_threshold + 1);

			when 6 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end delete_netchanger;

	



	


	

	procedure rotate_netchanger (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);
	begin
		-- CS log message

		case cmd_field_count is
			when 6 =>
				rotate_netchanger (
					module_name 	=> key (module),
					index			=> to_netchanger_id (get_field (cmd, 5)), -- 1,2,3,...
					rotation		=> to_rotation (get_field (cmd, 6)), -- 90
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;		
	end rotate_netchanger;



	
	
end et_cp_schematic_netchanger;


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
