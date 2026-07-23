------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS SCHEMATIC GROUP                           --
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
-- <http://www.gnu.org/licenses/>.   
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
--
-- 
--


with et_canvas_tool;
with et_canvas_schematic;			use et_canvas_schematic;
with et_generic_modules;			use et_generic_modules;
with et_logging;					use et_logging;
with et_schematic_ops_groups;		use et_schematic_ops_groups;
with et_cmd_origin_to_commit;


package body et_canvas_schematic_group is

	use et_canvas_schematic.pac_canvas;

	
	
	procedure drag_group (
		tool	: in type_tool;
		point	: in type_vector_model)
	is

		procedure finalize is
			use et_cmd_origin_to_commit;
			offset : type_vector_model;
		begin
			-- For the subprograms that draw objects
			-- of a moving group:
			set_group_not_moving;

			-- Clear all "moving"-flags:
			set_group_as_not_moving (active_module, log_threshold);
			
			-- Compute the final offset by which the
			-- group is to be moved:
			offset := point - object_point_of_attack;

			-- Do the final drag with the group:
			drag_group (active_module,
				offset, DO_COMMIT, log_threshold);
				
			-- Prepare for a new editing process;
			reset_editing_process; 

			-- Clear the status bar:
			status_clear;
		end finalize;
			
		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then

			-- So this branch is executed on the 
			-- first call of this procedure:
			
			-- Set the tool being used:
			object_tool := tool;

			-- Set the point where the group is
			-- grabbed (or attacked):
			object_point_of_attack := point;
			
			-- Set selected objects as "moving":
			set_group_as_moving (active_module, log_threshold);

			-- For the subprograms that draw objects
			-- of a moving group:
			set_group_moving;
			
			set_edit_process_running;

		else
			-- On the second call of this procedure,
			-- we finalize the drag-group operation:
			finalize;
		end if;
	end drag_group;

	







	


	procedure copy_group (
		tool	: in type_tool;
		point	: in type_vector_model)
	is

		procedure finalize is
			use et_cmd_origin_to_commit;
			offset : type_vector_model;
		begin
			-- For the subprograms that draw objects
			-- of a group being copied:
			set_group_not_being_copied;

			-- Compute the final offset by which the
			-- group is to be copied:
			offset := point - object_point_of_attack;

			-- Do the final copying with the group:
			copy_group (
				module_cursor	=> active_module, 
				sheet			=> 0, -- we stay on the current sheet
				offset			=> offset,
				commit_design	=> DO_COMMIT,
				log_threshold	=> log_threshold);
				
			-- Prepare for a new editing process;
			reset_editing_process; 

			-- Clear the status bar:
			status_clear;
		end finalize;
			
		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then

			-- So this branch is executed on the 
			-- first call of this procedure:
			
			-- Set the tool being used:
			object_tool := tool;

			-- Set the point where the group is
			-- grabbed (or attacked):
			object_point_of_attack := point;
			
			-- For the subprograms that draw objects
			-- of a group being copied:
			set_group_being_copied;
			
			set_edit_process_running;

		else
			-- On the second call of this procedure,
			-- we finalize the drag-group operation:
			finalize;
		end if;
	end copy_group;


	
end et_canvas_schematic_group;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
