------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON GROUPS                        --
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
with et_schematic_ops_nets;
with et_schematic_ops_units;
with et_schematic_ops_netchangers;

with et_board_ops_ratsnest;					use et_board_ops_ratsnest;

with et_modes.schematic;
with et_undo_redo;
with et_commit;


package body et_schematic_ops_groups is



	procedure reset_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure reset_nets is
			use et_schematic_ops_nets;
		begin
			log (text => "nets", level => log_threshold + 1);
			log_indentation_up;
			reset_status_objects (module_cursor, log_threshold + 2);
			log_indentation_down;
		end reset_nets;


		procedure reset_devices is
			use et_schematic_ops_units;
		begin
			log (text => "electrical devices and units", level => log_threshold + 1);
			log_indentation_up;
			reset_status_objects (module_cursor, log_threshold + 2);
			log_indentation_down;
		end reset_devices;


		procedure reset_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;
			reset_status_objects (module_cursor, log_threshold + 2);
			log_indentation_down;
		end reset_netchangers;



	begin
		log (text => "module " & to_string (module_cursor)
			 & " reset objects (schematic)",
			level => log_threshold);

		log_indentation_up;

		reset_nets;
		reset_devices;
		reset_netchangers;

		-- CS reset texts, ... ?

		log_indentation_down;
	end reset_objects;








	procedure define_group_rectangular (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		area			: in type_area;
		log_threshold	: in type_log_level)
	is

		procedure group_units is
			use et_schematic_ops_units;
		begin
			log (text => "units", level => log_threshold + 1);
			log_indentation_up;

			group_units_in_rectangular_area (
				module_cursor, sheet, area, log_threshold + 2);

			log_indentation_down;
		end group_units;


		procedure group_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;

			group_netchangers_in_rectangular_area (
				module_cursor, sheet, area, log_threshold + 2);

			log_indentation_down;
		end group_netchangers;


		procedure group_net_segments is
			use et_schematic_ops_nets;
		begin
			log (text => "net segments", level => log_threshold + 1);
			log_indentation_up;

			group_segments_in_rectangular_area (
				module_cursor, sheet, area, log_threshold + 2);

			log_indentation_down;
		end group_net_segments;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " define rectangular group (schematic)",
			level => log_threshold);

		log_indentation_up;

		-- Set the sheet of the reference point.
		-- This is only relevant if the clipboard is used:
		set_sheet (group_reference_point, sheet);

		-- CS: this should be depended on
		-- the currently displayed layers:
		group_units;
		group_netchangers;
		group_net_segments;

		-- CS texts,
		-- Do not group placeholders of units !

		log_indentation_down;
	end define_group_rectangular;








	


	function get_center_of_group (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;							 
		log_threshold	: in type_log_level)
		return type_vector_model
	is
		result : type_vector_model;

		all_positions : pac_points.list;
		
		unit_positions : pac_points.list;
		-- CS
		-- text_positions : pac_points.list;
		-- segment_positions : pac_points.list;
		
		procedure query_units is
			use et_schematic_ops_units;
		begin
			log (text => "query units", level => log_threshold + 1);
			log_indentation_up;
			
			-- Get the positions (x/y) of the units
			-- of the group:
			unit_positions := get_group_unit_positions (
				module_cursor, sheet, log_threshold + 2);

			log_indentation_down;
		end query_units;

		
		procedure merge_positions is
			use pac_points;
			c : pac_points.cursor;
		begin
			-- Unit positions:
			splice (
				target	=> all_positions,
				before	=> c,
				source	=> unit_positions);
				
			-- CS: segment, net positions
		end merge_positions;
			
		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " get center of group (schematic)",
			level => log_threshold);

		log_indentation_up;

		query_units;
		-- CS: query net segments, texts
		
		-- Merge unit positions, net segment positions,
		-- text positons, ...
		merge_positions;
		

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
		use et_modes.schematic;



		procedure delete_nets is
			use et_schematic_ops_nets;
		begin
			log (text => "nets", level => log_threshold + 1);
			log_indentation_up;
			delete_segments_in_group (module_cursor, log_threshold + 2);
			log_indentation_down;
		end delete_nets;


		procedure delete_devices is
			use et_schematic_ops_units;
		begin
			log (text => "electrical devices and units", level => log_threshold + 1);
			log_indentation_up;
			delete_units_in_group (module_cursor, log_threshold + 2);
			log_indentation_down;
		end delete_devices;


		procedure delete_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;
			delete_netchangers_in_group (module_cursor, log_threshold + 2);
			log_indentation_down;
		end delete_netchangers;



	begin
		log (text => "module " & to_string (module_cursor)
			 & " delete group (schematic)",
			level => log_threshold);

		log_indentation_up;

		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold);
		end if;


		delete_nets;
		delete_devices;
		delete_netchangers;

		-- CS delete texts, ... ?

		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold);
		end if;

		log_indentation_down;
	end delete_group;











	procedure drag_group (
		module_cursor	: in pac_generic_modules.cursor;
		offset			: in type_vector_model; -- x/y
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is
		use et_commit;
		use et_undo_redo;
		use et_modes.schematic;


		procedure drag_units is
			use et_schematic_ops_units;
		begin
			log (text => "units", level => log_threshold + 1);
			log_indentation_up;

			drag_selected_units (module_cursor,
				offset, log_threshold + 2);

			log_indentation_down;
		end drag_units;


		procedure drag_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;

			drag_selected_netchangers (module_cursor,
				offset, log_threshold + 2);

			log_indentation_down;
		end drag_netchangers;


		procedure drag_net_segments is
			use et_schematic_ops_nets;
		begin
			log (text => "net segments", level => log_threshold + 1);
			log_indentation_up;

			drag_selected_net_segments (module_cursor,
				offset, log_threshold + 2);

			log_indentation_down;
		end drag_net_segments;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " drag group (schematic)",
			level => log_threshold);

		log_indentation_up;

		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold);
		end if;


		-- Drag first the selected units and
		-- indirectly the connected net segments:
		drag_units;

		-- Drag the selected netchangers and
		-- indirectly the connected net segments:
		drag_netchangers;

		-- Drag now the remaining net segments
		-- which are not connected with units or netchangers:
		drag_net_segments;

		-- CS texts


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
	end drag_group;










	procedure set_group_as_moving (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure set_units is
			use et_schematic_ops_units;
		begin
			log (text => "units", level => log_threshold + 1);
			log_indentation_up;

			set_selected_units_as_moving (module_cursor,
				log_threshold + 2);

			log_indentation_down;
		end set_units;


		procedure set_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;

			set_selected_netchangers_as_moving (module_cursor,
				log_threshold + 2);

			log_indentation_down;
		end set_netchangers;


		procedure set_net_segments is
			use et_schematic_ops_nets;
		begin
			log (text => "net segments", level => log_threshold + 1);
			log_indentation_up;

			set_selected_net_segments_as_moving (module_cursor,
				log_threshold + 2);

			log_indentation_down;
		end set_net_segments;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " set group as moving",
			 level => log_threshold);

		log_indentation_up;

		set_units;
		set_netchangers;
		set_net_segments;

		log_indentation_down;
	end set_group_as_moving;











	procedure set_group_as_not_moving (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure set_units is
			use et_schematic_ops_units;
		begin
			log (text => "units", level => log_threshold + 1);
			log_indentation_up;

			set_all_units_as_not_moving (module_cursor,
				log_threshold + 2);

			log_indentation_down;
		end set_units;


		procedure set_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;

			set_all_netchangers_as_not_moving (module_cursor,
				log_threshold + 2);

			log_indentation_down;
		end set_netchangers;


		procedure set_net_segments is
			use et_schematic_ops_nets;
		begin
			log (text => "net segments", level => log_threshold + 1);
			log_indentation_up;

			set_all_net_segments_as_not_moving (module_cursor,
				log_threshold + 2);

			log_indentation_down;
		end set_net_segments;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " set group as NOT moving",
			 level => log_threshold);

		log_indentation_up;

		set_units;
		set_netchangers;
		set_net_segments;

		log_indentation_down;
	end set_group_as_not_moving;









	procedure copy_group_simple (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet_relative;
		offset			: in type_vector_model; -- x/y
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is
		use et_commit;
		use et_undo_redo;
		use et_modes.schematic;


		procedure copy_units is
			use et_schematic_ops_units;
		begin
			log (text => "units", level => log_threshold + 1);
			log_indentation_up;

			copy_selected_units (module_cursor,
				sheet, offset, log_threshold + 2);

			log_indentation_down;
		end copy_units;


		procedure copy_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;

			copy_selected_netchangers (module_cursor,
				sheet, offset, log_threshold + 2);

			log_indentation_down;
		end copy_netchangers;


		procedure copy_net_segments is
			use et_schematic_ops_nets;
		begin
			log (text => "net segments", level => log_threshold + 1);
			log_indentation_up;

			copy_selected_net_segments (module_cursor,
				sheet, offset, log_threshold + 2);

			log_indentation_down;
		end copy_net_segments;


	begin
		log (text => "module " & to_string (module_cursor)
				& " simple copy group by sheet(s) " & relative_to_string (sheet)
				& " offset " & to_string (offset),
			level => log_threshold);


		log_indentation_up;


		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold);
		end if;


		-- Copy first the selected units:
		copy_units;

		-- Copy the selected netchangers:
		copy_netchangers;

		-- Copy now the selected net segments:
		copy_net_segments;

		-- CS texts


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

				-- Compute the center of the group.
				-- The group is located at the sheet as
				-- specified by the group_reference_point:
				center := get_center_of_group (
					module_cursor	=> module_cursor,
					sheet			=> get_sheet (group_reference_point),
					log_threshold	=> log_threshold + 1);

				log (text => "center " & to_string (center),
					level => log_threshold + 1);
					
				-- Set x/y of group_reference_point by
				-- the center of the group:
				set_place (group_reference_point, center);

			else
				set_place (group_reference_point, reference_point);
			end if;					
		end set_group_reference_point;

		
		
		procedure copy_units_to_clipboard is
			use et_schematic_ops_units;
		begin
			log (text => "units",
				 level => log_threshold + 1);

			log_indentation_up;

			copy_selected_units_to_clipboard (
				module_cursor, log_threshold + 2);
			-- CS: to speed up the process, pass the sheet where the group is
			-- using get_sheet (group_reference_point)

			log_indentation_down;
		end copy_units_to_clipboard;


		
		procedure copy_net_segments_to_clipboard is
			use et_schematic_ops_nets;
		begin
			log (text => "net segments",
				 level => log_threshold + 1);

			log_indentation_up;

			copy_selected_net_segments_to_clipboard (
				module_cursor, log_threshold + 2);
			-- CS: to speed up the process, pass the sheet where the group is
			-- using get_sheet (group_reference_point)
				
			log_indentation_down;
		end copy_net_segments_to_clipboard;

		
		
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

		set_group_reference_point;
		

		-- Copy selected units to clipboard:
		copy_units_to_clipboard;

		-- Copy selected net segments to clipboard:
		copy_net_segments_to_clipboard;
		
		
		-- CS texts

		log_indentation_down;
	end copy_group_to_clipboard;










	procedure paste_group (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		place			: in type_vector_model; -- x/y
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is
		use et_commit;
		use et_undo_redo;
		use et_modes.schematic;

		offset : type_object_position_relative;



		procedure compute_offset is
			destination : type_object_position;
		begin
			destination := to_position (place, sheet);

			offset := get_offset (group_reference_point, destination);

			log (text => "group offset " & to_string (offset),
				 level => log_threshold + 1);
		end compute_offset;



		procedure paste_units is
			use et_schematic_ops_units;
		begin
			log (text => "units",
				 level => log_threshold + 1);

			log_indentation_up;

			paste_units_from_clipboard (
				module_cursor, offset, log_threshold + 2);

			log_indentation_down;
		end paste_units;



	begin
		log (text => "module " & to_string (module_cursor)
			& " paste group at sheet " & to_string (sheet)
			& " place " & to_string (place),
			level => log_threshold);


		log_indentation_up;


		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold);
		end if;


		compute_offset;


		-- Transfer objects from clipboard to the
		-- given module:
		paste_units;

		-- CS
		-- nets, texts


		-- Previously to commiting the design,
		-- the status of all objects must be reset:
		reset_objects (module_cursor, log_threshold + 1);


		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold);
		end if;


		update_ratsnest (module_cursor, log_threshold + 1);

		log_indentation_down;
	end paste_group;


end et_schematic_ops_groups;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
