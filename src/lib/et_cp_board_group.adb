------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   COMMAND PROCESSOR / BOARD / GROUP                      --
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


-- with ada.text_io;			use ada.text_io;
with et_string_processing;				use et_string_processing;

with et_board_geometry;					use et_board_geometry;
with et_board_ops_groups;				use et_board_ops_groups;

with et_cmd_origin_to_commit;			use et_cmd_origin_to_commit;


package body et_cp_board_group is

	use pac_geometry_2;




	procedure define_group (
		module			: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		procedure rectangular_group is
			area : type_area;
		begin
			-- Set the position (lower-left corner) of
			-- the rectangular area:
			set_position (area,
				to_vector_model (get_field (cmd, 5), get_field (cmd, 6))); -- x y

			-- Set the width of the area:
			set_width (area,
				to_distance (get_field (cmd, 7)));

			-- Set the height of the area:
			set_height (area,
				to_distance (get_field (cmd, 8)));

			-- define_group_rectangular (
			-- 	module, area, log_threshold + 1);

		end rectangular_group;


	begin
		log (text => "define group", level => log_threshold);
		log_indentation_up;


		case cmd_field_count is
			when 8 =>
				rectangular_group;

			-- CS circular group ?

			when 9 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);

			when others => command_incomplete (cmd);
		end case;


		log_indentation_down;
	end define_group;








	procedure clear_group (
		module			: in pac_generic_modules.cursor;
		cmd			: in out type_single_cmd;
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
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

	begin
		log (text => "delete group", level => log_threshold);
		log_indentation_up;


		case cmd_field_count is
			when 4 =>
				null;
				-- delete_group (
				-- 	module_cursor	=> module,
				--
				-- 	-- Depending on the origin of the command,
				-- 	-- the design state is to be commited or not:
				-- 	commit_design	=> to_commit_design (cmd),
				-- 	log_threshold	=> log_threshold + 1);

			when 5 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);

			when others => command_incomplete (cmd);
		end case;


		log_indentation_down;
	end delete_group;










	procedure move_group (
		module			: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		procedure do_it is
			offset : type_vector_model;
		begin
			offset := to_vector_model (
				x => get_field (cmd, 5),
				y => get_field (cmd, 6));

			-- move_group (
			-- 	module_cursor	=> module,
			-- 	offset			=> offset,
			--
			-- 	-- Depending on the origin of the command,
			-- 	-- the design state is to be commited or not:
			-- 	commit_design	=> to_commit_design (cmd),
			--
			-- 	log_threshold	=> log_threshold + 1);
		end do_it;


	begin
		log (text => "move group", level => log_threshold);
		log_indentation_up;

		case cmd_field_count is
			when 6 =>
				do_it;

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);

			when others => command_incomplete (cmd);
		end case;

		log_indentation_down;
	end move_group;








	procedure copy_group (
		module			: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		-- This procedure makes a simple copy
		-- of the current group:
		procedure do_simple_copy is
			offset	: type_vector_model;
		begin
			offset := to_vector_model (
				x => get_field (cmd, 5),
				y => get_field (cmd, 6));


			-- copy_group_simple (
			-- 	module_cursor	=> module,
			-- 	sheet			=> sheet,
			-- 	offset			=> offset,
			--
			-- 	-- Depending on the origin of the command,
			-- 	-- the design state is to be commited or not:
			-- 	commit_design	=> to_commit_design (cmd),
			-- 	log_threshold	=> log_threshold + 1);

		end do_simple_copy;



		-- This procedure copies the current group into
		-- the clipboard with the center of the group
		-- as reference point:
		procedure copy_to_clipboard_center is begin
			null;
			-- copy_group_to_clipboard (
			-- 	module_cursor	=> module,
			-- 	log_threshold	=> log_threshold + 2);

		end copy_to_clipboard_center;



		-- This procedure copies the current group into
		-- the clipboard with a user specified reference point:
		procedure copy_to_clipboard_ref_point is
			reference_point	: type_vector_model;
		begin
			reference_point := to_vector_model (
				x => get_field (cmd, 5),
				y => get_field (cmd, 6));

			-- copy_group_to_clipboard (
			-- 	module_cursor	=> module,
			-- 	auto_center		=> false,
			-- 	reference_point	=> reference_point,
			-- 	log_threshold	=> log_threshold + 2);

		end copy_to_clipboard_ref_point;


	begin
		log (text => "copy group", level => log_threshold);
		log_indentation_up;


		case cmd_field_count is
			when 4 =>
				copy_to_clipboard_center;

			when 6 =>
				copy_to_clipboard_ref_point;

			when 7 =>
				do_simple_copy;

			when 8 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);

			when others => command_incomplete (cmd);
		end case;


		log_indentation_down;
	end copy_group;











	procedure paste_group (
		module			: in pac_generic_modules.cursor;
		cmd				: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		procedure do_it is
			place	: type_vector_model;
		begin
			place := to_vector_model (
				x => get_field (cmd, 5),
				y => get_field (cmd, 6));


			-- paste_group (
			-- 	module_cursor	=> module,
			-- 	sheet			=> sheet,
			-- 	place			=> place,
			--
			-- 	-- Depending on the origin of the command,
			-- 	-- the design state is to be commited or not:
			-- 	commit_design	=> to_commit_design (cmd),
			-- 	log_threshold	=> log_threshold + 1);

		end do_it;


	begin
		log (text => "paste group", level => log_threshold);
		log_indentation_up;


		case cmd_field_count is
			when 6 =>
				do_it;

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);

			when others => command_incomplete (cmd);
		end case;


		log_indentation_down;
	end paste_group;



end et_cp_board_group;



-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
