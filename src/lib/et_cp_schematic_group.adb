------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                COMMAND PROCESSOR / SCHEMATIC / GROUP                     --
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
--
-- - propose arguments if command incomplete
--
--
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_sheets;							use et_sheets;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_ops_groups;			use et_schematic_ops_groups;

with et_cmd_origin_to_commit;			use et_cmd_origin_to_commit;


package body et_cp_schematic_group is

	use pac_generic_modules;
	use pac_geometry_2;




	procedure define_group (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		
		procedure rectangular_group is
			sheet : type_sheet;
			area : type_area;			
		begin
			-- Get the targeted sheet:
			sheet := to_sheet (get_field (cmd, 5));

			-- Set the position (lower-left corner) of
			-- the rectangular area:
			set_position (area, 
				to_vector_model (get_field (cmd, 6), get_field (cmd, 7))); -- x y

			-- Set the width of the area:
			set_width (area,
				to_distance (get_field (cmd, 8)));

			-- Set the height of the area:
			set_height (area,
				to_distance (get_field (cmd, 9)));

			define_group_rectangular (
				module, sheet, area, log_threshold + 1);
			
		end rectangular_group;

		
	begin
		log (text => "define group", level => log_threshold);
		log_indentation_up;
		

		case cmd_field_count is
			when 9 =>
				rectangular_group;

			-- CS circular group ?
				
			when 10 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

		
		log_indentation_down;
	end define_group;



	


	

	procedure clear_group (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

	begin
		log (text => "clear group", level => log_threshold);
		log_indentation_up;
		

		case cmd_field_count is
			when 4 =>
				reset_objects (module, log_threshold + 1);

			when 5 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

		
		log_indentation_down;
	end clear_group;

	


	

	

	procedure delete_group (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

	begin
		log (text => "delete group", level => log_threshold);
		log_indentation_up;
		

		case cmd_field_count is
			when 4 =>
				delete_group (
					module_cursor	=> module, 

					-- Depending on the origin of the command,
					-- the design state is to be commited or not:
					commit_design	=> to_commit_design (cmd),
					log_threshold	=> log_threshold + 1);

			when 5 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

		
		log_indentation_down;
	end delete_group;


	
	
	
end et_cp_schematic_group;


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
