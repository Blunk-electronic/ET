------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS ON GROUPS                         --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo:


-- with ada.text_io;			use ada.text_io;
with et_board_ops_devices;
with et_board_ops_netchangers;
with et_board_ops_assy_doc;
with et_board_ops_silkscreen;
with et_board_ops_stopmask;
with et_board_ops_stencil;
with et_board_ops_keepout;
with et_board_ops_outline;
with et_board_ops_conductors;
with et_board_ops_vias;

with et_ripup;
with et_board_ops_ratsnest;				use et_board_ops_ratsnest;

with et_module_clipboard;

with et_modes.board;
with et_undo_redo;
with et_commit;



package body et_board_ops_groups is


	procedure reset_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure reset_conductors is
			use et_board_ops_conductors;
		begin
			log (text => "conductors",
				 level => log_threshold + 1);

			log_indentation_up;
			reset_status_objects (active_module, log_threshold + 1);
			log_indentation_down;
		end reset_conductors;


		procedure reset_devices is
			use et_board_ops_devices;
		begin
			log (text => "devices (electrical and non-electrical)",
				 level => log_threshold + 1);

			log_indentation_up;
			reset_status_objects (module_cursor, log_threshold + 2);
			log_indentation_down;
		end reset_devices;


		procedure reset_netchangers is
			use et_board_ops_netchangers;
		begin
			log (text => "netchangers",
				 level => log_threshold + 1);

			log_indentation_up;
				reset_status_objects (active_module, log_threshold + 1);
			log_indentation_down;
		end reset_netchangers;


		procedure reset_assy_doc is
			use et_board_ops_assy_doc;
		begin
			log (text => "assembly documentation",
				 level => log_threshold + 1);

			log_indentation_up;
			reset_status_objects (active_module, log_threshold + 1);
			log_indentation_down;
		end reset_assy_doc;


		procedure reset_silkscreen is
			use et_board_ops_silkscreen;
		begin
			log (text => "silkscreen",
				 level => log_threshold + 1);

			log_indentation_up;
				reset_proposed_objects (active_module, log_threshold + 1);
			log_indentation_down;
		end reset_silkscreen;


		procedure reset_stencil is
			use et_board_ops_stencil;
		begin
			log (text => "stencil",
				 level => log_threshold + 1);

			log_indentation_up;
				reset_proposed_objects (active_module, log_threshold + 1);
			log_indentation_down;
		end reset_stencil;


		procedure reset_stopmask is
			use et_board_ops_stopmask;
		begin
			log (text => "stopmask",
				 level => log_threshold + 1);

			log_indentation_up;
				reset_proposed_objects (active_module, log_threshold + 1);
			log_indentation_down;
		end reset_stopmask;


		procedure reset_keepout is
			use et_board_ops_keepout;
		begin
			log (text => "keepout",
				 level => log_threshold + 1);

			log_indentation_up;
				reset_proposed_objects (active_module, log_threshold + 1);
			log_indentation_down;
		end reset_keepout;


		procedure reset_outline is
			use et_board_ops_outline;
		begin
			log (text => "outline",
				 level => log_threshold + 1);

			log_indentation_up;
				reset_proposed_objects (active_module, log_threshold + 1);
			log_indentation_down;
		end reset_outline;


		procedure reset_vias is
			use et_board_ops_vias;
		begin
			log (text => "vias",
				 level => log_threshold + 1);

			log_indentation_up;
				reset_proposed_vias (active_module, log_threshold + 1);
			log_indentation_down;
		end reset_vias;


		procedure reset_airwires is
			use et_board_ops_ratsnest;
		begin
			log (text => "ratsnest",
				 level => log_threshold + 1);

			log_indentation_up;
				reset_proposed_airwires (active_module, log_threshold + 1);
			log_indentation_down;
		end reset_airwires;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " reset objects (board)",
			level => log_threshold);

		log_indentation_up;

		reset_devices;
		reset_netchangers;
		reset_conductors;

		reset_assy_doc;
		reset_silkscreen;
		reset_stencil;
		reset_stopmask;
		reset_keepout;
		reset_outline;
		reset_vias;

		reset_airwires;

		-- CS reset board placeholders, texts, ... ?

		et_ripup.reset_ripup_mode;

		log_indentation_down;
	end reset_objects;











	procedure define_group_rectangular (
		module_cursor	: in pac_generic_modules.cursor;
		area			: in type_area;
		log_threshold	: in type_log_level)
	is

		procedure group_devices is
		begin
			log (text => "devices (electrical and non-electrical)",
				 level => log_threshold + 1);

			log_indentation_up;
				-- CS
			log_indentation_down;
		end group_devices;


		procedure group_netchangers is
		begin
			log (text => "netchangers",
				 level => log_threshold + 1);

			log_indentation_up;
				-- CS
			log_indentation_down;
		end group_netchangers;


		procedure group_conductors is
		begin
			log (text => "conductors",
				 level => log_threshold + 1);

			log_indentation_up;
				-- CS
			log_indentation_down;
		end group_conductors;


		procedure group_assy_doc is
		begin
			log (text => "assembly documentation",
				 level => log_threshold + 1);

			log_indentation_up;
				-- CS
			log_indentation_down;
		end group_assy_doc;


		procedure group_silkscreen is
		begin
			log (text => "silkscreen",
				 level => log_threshold + 1);

			log_indentation_up;
				-- CS
			log_indentation_down;
		end group_silkscreen;


		procedure group_stencil is
		begin
			log (text => "stencil",
				 level => log_threshold + 1);

			log_indentation_up;
				-- CS
			log_indentation_down;
		end group_stencil;


		procedure group_stopmask is
		begin
			log (text => "stopmask",
				 level => log_threshold + 1);

			log_indentation_up;
				-- CS
			log_indentation_down;
		end group_stopmask;


		procedure group_keepout is
		begin
			log (text => "keepout",
				 level => log_threshold + 1);

			log_indentation_up;
				-- CS
			log_indentation_down;
		end group_keepout;


		procedure group_outline is
		begin
			log (text => "outline",
				 level => log_threshold + 1);

			log_indentation_up;
				-- CS
			log_indentation_down;
		end group_outline;


		procedure group_vias is
		begin
			log (text => "vias",
				 level => log_threshold + 1);

			log_indentation_up;
				-- CS
			log_indentation_down;
		end group_vias;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " define rectangular group (board)",
			level => log_threshold);

		log_indentation_up;

		-- Ungroup/deselect previous objects:
		reset_objects (module_cursor, log_threshold + 1);

		-- CS: this should be depended on
		-- the currently displayed layers:
		group_devices;

		group_netchangers;
		group_conductors;

		group_assy_doc;
		group_silkscreen;
		group_stencil;
		group_stopmask;
		group_keepout;
		group_outline;
		group_vias;

		-- CS ? group_airwires;

		-- CS reset board placeholders, texts, ... ?

		log_indentation_down;
	end define_group_rectangular;










	function get_center_of_group (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return type_vector_model
	is
		result : type_vector_model; -- the center of the group

		-- All places:
		all_positions : pac_points.list;

	begin
		log (text => "module " & to_string (module_cursor)
			 & " get center of group (board)",
			level => log_threshold);

		log_indentation_up;

		-- Now we have a cloud of points of which
		-- the geometrical center is to be found:
		result := get_center (all_positions);

		log_indentation_down;

		return result;
	end get_center_of_group;













	procedure delete_group (
		module_cursor	: in pac_generic_modules.cursor;
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is
		use et_commit;
		use et_undo_redo;
		use et_modes.board;



	begin
		log (text => "module " & to_string (module_cursor)
			 & " delete group (board)",
			level => log_threshold);

		log_indentation_up;

		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold);
		end if;



		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold);
		end if;

		log_indentation_down;
	end delete_group;













	procedure move_group (
		module_cursor	: in pac_generic_modules.cursor;
		offset			: in type_vector_model; -- x/y
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is
		use et_commit;
		use et_undo_redo;
		use et_modes.board;



	begin
		log (text => "module " & to_string (module_cursor)
			 & " move group (board)",
			level => log_threshold);

		log_indentation_up;

		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold);
		end if;




		-- Previously to commiting the design,
		-- the status of all objects must be reset.
		-- This is important for the "moving" flags.
		reset_objects (module_cursor, log_threshold + 1);


		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold);
		end if;


		update_ratsnest (module_cursor, log_threshold + 1);

		log_indentation_down;
	end move_group;












	procedure set_group_as_moving (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is


	begin
		log (text => "module " & to_string (module_cursor)
			 & " set group as moving",
			 level => log_threshold);

		log_indentation_up;

		log_indentation_down;
	end set_group_as_moving;











	procedure set_group_as_not_moving (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

	begin
		log (text => "module " & to_string (module_cursor)
			 & " set group as NOT moving",
			 level => log_threshold);

		log_indentation_up;

		log_indentation_down;
	end set_group_as_not_moving;












	procedure copy_group_simple (
		module_cursor	: in pac_generic_modules.cursor;
		offset			: in type_vector_model; -- x/y
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is
		use et_commit;
		use et_undo_redo;
		use et_modes.board;




	begin
		log (text => "module " & to_string (module_cursor)
			 & " simple copy group by offset "
			 & to_string (offset),
			level => log_threshold);


		log_indentation_up;


		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold);
		end if;




		-- Previously to commiting the design,
		-- the status of all objects must be reset:
		reset_objects (module_cursor, log_threshold + 1);


		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold);
		end if;


		update_ratsnest (module_cursor, log_threshold + 1);

		log_indentation_down;
	end copy_group_simple;












	procedure copy_group_to_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		auto_center		: in boolean := true;
		reference_point	: in type_vector_model := origin;
		log_threshold	: in type_log_level)
	is

		-- This procedure sets the group_reference_point
		-- according to the mode specified by argument auto_center.
		-- If auto_center is true then the geometrical center
		-- of the group is assigned to group_reference_point.
		-- If auto_center is false, then the given reference_point
		-- is assigned to group_reference_point:
		procedure set_group_reference_point is
			center : type_vector_model;
		begin
			if auto_center then

				-- Compute the center of the group:
				center := get_center_of_group (
					module_cursor	=> module_cursor,
					log_threshold	=> log_threshold + 1);

				log (text => "center of group " & to_string (center),
					level => log_threshold + 1);

				-- Set x/y of group_reference_point by
				-- the center of the group:
				group_reference_point := center;

			else
				group_reference_point := reference_point;
			end if;
		end set_group_reference_point;




	begin
		if auto_center then
			log (text => "module " & to_string (module_cursor)
				& " copy group to clipboard."
				& " reference point: auto center.",
				level => log_threshold);

		else
			log (text => "module " & to_string (module_cursor)
				& " copy group to clipboard."
				& " reference point: " & to_string (reference_point),
				level => log_threshold);

		end if;


		log_indentation_up;

		-- Clean up clipboard:
		et_module_clipboard.clear_clipboard;

		set_group_reference_point;



		log_indentation_down;
	end copy_group_to_clipboard;













	procedure paste_group (
		module_cursor	: in pac_generic_modules.cursor;
		place			: in type_vector_model; -- x/y
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is
		use et_commit;
		use et_undo_redo;
		use et_modes.board;

		offset : type_vector_model;



		procedure compute_offset is
		begin
			offset := place - group_reference_point;

			log (text => "group offset " & to_string (offset),
				 level => log_threshold + 1);
		end compute_offset;






	begin
		log (text => "module " & to_string (module_cursor)
			& " paste group at place " & to_string (place),
			level => log_threshold);

		log_indentation_up;

		-- The clipboard might be empty. In this case there is
		-- nothing to do:
		if et_module_clipboard.clipboard_is_empty then
			log (text => "clipboard is empty -> nothing to do !",
				 level => log_threshold);
		else
		-- Start paste operations:

			if commit_design = DO_COMMIT then
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold);
			end if;


			compute_offset;


			-- Transfer objects from clipboard to the
			-- given module:


			-- Previously to commiting the design,
			-- the status of all objects must be reset:
			reset_objects (module_cursor, log_threshold + 1);


			if commit_design = DO_COMMIT then
				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold);
			end if;


			update_ratsnest (module_cursor, log_threshold + 1);
		end if;


		log_indentation_down;
	end paste_group;


end et_board_ops_groups;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
