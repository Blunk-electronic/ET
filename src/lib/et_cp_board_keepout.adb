------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                COMMAND PROCESSOR / BOARD / KEEPOUT                       --
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
--
--


with et_string_processing;		use et_string_processing;
with et_pcb_sides;						use et_pcb_sides;

with et_keywords;						use et_keywords;

with et_board_geometry;
with et_board_ops_keepout;				use et_board_ops_keepout;

with et_cmd_origin_to_commit;			use et_cmd_origin_to_commit;



package body et_cp_board_keepout is

	use et_board_geometry.pac_contours;

	

	
	procedure draw_keepout_zone (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		-- Extract from the given command the zone 
		-- arguments (everything after "keepout"):
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			face : type_face;
		begin
			face := to_face (get_field (cmd, 5));
			
			add_zone (
				module_cursor	=> module,
				zone			=> (c with null record),
				face			=> face,

				-- Depending on the origin of the command,
				-- the design state is to be commited or not:
				commit_design	=> to_commit_design (cmd),
				log_threshold	=> log_threshold + 1);

		end build_zone;
	

	begin
		log (text => "draw keepout zone", level => log_threshold);
		log_indentation_up;
		
		-- Convert the contour to a keepout zone
		-- and assign it to the module:
		if get_field (cmd, 6) = keyword_zone then
			build_zone;
		else
			null;
			-- CS error. only zone allowed here
		end if;

		log_indentation_down;
	end draw_keepout_zone;







	
	procedure draw_keepout_zone_cutout (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		-- Extract from the given command the zone 
		-- arguments (everything after "keepout"):
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			face : type_face;
		begin
			face := to_face (get_field (cmd, 5));

			-- CS
			-- add_zone (
			-- 	module_cursor	=> module,
			-- 	zone			=> (c with null record),
			-- 	face			=> face,

			-- Depending on the origin of the command,
			-- the design state is to be commited or not:
			--	commit_design	=> to_commit_design (cmd),
			-- 	log_threshold	=> log_threshold + 1);

		end build_zone;
	

	begin
		log (text => "draw keepout cutout zone", level => log_threshold);
		log_indentation_up;
		
		-- Convert the contour to a keepout zone
		-- and assign it to the module:
		if get_field (cmd, 6) = keyword_zone then
			build_zone;
		else
			null;
			-- CS error. only zone allowed here
		end if;

		log_indentation_down;
	end draw_keepout_zone_cutout;

	






	procedure delete_keepout (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);
	begin
		log (text => "delete keepout zone", level => log_threshold);
		log_indentation_up;

		-- CS
		null;

		log_indentation_down;
	end delete_keepout;

	
		
	
end et_cp_board_keepout;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
