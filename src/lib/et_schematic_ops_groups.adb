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
		end;

		
		procedure reset_devices is 
			use et_schematic_ops_units;
		begin
			log (text => "electrical devices and units", level => log_threshold + 1);
			log_indentation_up;
			reset_status_objects (module_cursor, log_threshold + 2);
			log_indentation_down;
		end;


		procedure reset_netchangers is 
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;
			reset_status_objects (module_cursor, log_threshold + 2);
			log_indentation_down;
		end;

		
		
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
		end;
		

		procedure group_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;
			
			group_netchangers_in_rectangular_area (
				module_cursor, sheet, area, log_threshold + 2);
			
			log_indentation_down;
		end;

		
		procedure group_net_segments is
			use et_schematic_ops_nets;
		begin
			log (text => "net segments", level => log_threshold + 1);
			log_indentation_up;
			
			group_segments_in_rectangular_area (
				module_cursor, sheet, area, log_threshold + 2);
			
			log_indentation_down;
		end;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " define rectangular group (schematic)",
			level => log_threshold);

		log_indentation_up;

		-- CS: this should be depended on
		-- the currently displayed layers:
		group_units;
		group_netchangers;
		group_net_segments;
		
		-- CS texts, 
		-- Do not group placeholders of units !
		
		log_indentation_down;
	end define_group_rectangular;

	
	
	
	
	

	
	
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
		end;

		
		procedure delete_devices is 
			use et_schematic_ops_units;
		begin
			log (text => "electrical devices and units", level => log_threshold + 1);
			log_indentation_up;
			delete_units_in_group (module_cursor, log_threshold + 2);
			log_indentation_down;
		end;


		procedure delete_netchangers is 
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;
			delete_netchangers_in_group (module_cursor, log_threshold + 2);
			log_indentation_down;
		end;

		
		
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
		end;
		

		procedure drag_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;
			
			drag_selected_netchangers (module_cursor, 
				offset, log_threshold + 2);
			
			log_indentation_down;
		end;

		
		procedure drag_net_segments is
			use et_schematic_ops_nets;
		begin
			log (text => "net segments", level => log_threshold + 1);
			log_indentation_up;
			
			drag_selected_net_segments (module_cursor, 
				offset, log_threshold + 2);

			log_indentation_down;
		end;

		
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
		end;
		

		procedure set_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;
			
			set_selected_netchangers_as_moving (module_cursor, 
				log_threshold + 2);
			
			log_indentation_down;
		end;

		
		procedure set_net_segments is
			use et_schematic_ops_nets;
		begin
			log (text => "net segments", level => log_threshold + 1);
			log_indentation_up;
			
			set_selected_net_segments_as_moving (module_cursor, 
				log_threshold + 2);

			log_indentation_down;
		end;
		

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
		end;
		

		procedure set_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;
			
			set_all_netchangers_as_not_moving (module_cursor, 
				log_threshold + 2);

			log_indentation_down;
		end;

		
		procedure set_net_segments is
			use et_schematic_ops_nets;
		begin
			log (text => "net segments", level => log_threshold + 1);
			log_indentation_up;
			
			set_all_net_segments_as_not_moving (module_cursor, 
				log_threshold + 2);

			log_indentation_down;
		end;
		

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
	








	procedure copy_group (
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
		end;
		

		procedure copy_netchangers is
			use et_schematic_ops_netchangers;
		begin
			log (text => "netchangers", level => log_threshold + 1);
			log_indentation_up;
			
			copy_selected_netchangers (module_cursor, 
				sheet, offset, log_threshold + 2);
			
			log_indentation_down;
		end;

		
		procedure copy_net_segments is
			use et_schematic_ops_nets;
		begin
			log (text => "net segments", level => log_threshold + 1);
			log_indentation_up;
			
			copy_selected_net_segments (module_cursor, 
				sheet, offset, log_threshold + 2);

			log_indentation_down;
		end;


		
	begin
		log (text => "module " & to_string (module_cursor)
				& " copy group by sheet(s) " & relative_to_string (sheet) 
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
	end copy_group;
	


	
end et_schematic_ops_groups;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
