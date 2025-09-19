------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             GUI GENERAL                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with et_modes;						use et_modes;

with et_canvas_schematic_2;
with et_schematic_geometry;
with et_schematic_coordinates;

with et_canvas_board_2;
with et_board_geometry;

with et_schematic_ops.grid;
with et_board_ops.grid;

with et_module_names;				use et_module_names;



package body et_gui_2 is


	
	procedure init_schematic (
		project			: in pac_project_name.bounded_string;	-- blood_sample_analyzer
		module			: in pac_generic_modules.cursor; -- cursor of generic module to be edited
		sheet			: in et_sheets.type_sheet := et_sheets.type_sheet'first; -- the sheet to be opened
		log_threshold_in: in type_log_level) 
	is
		use et_canvas_schematic_2;
		use et_canvas_schematic_2.pac_canvas;
		use et_schematic_geometry.pac_geometry_2;
		use pac_generic_modules;
		use et_sheets;
		use et_schematic_coordinates;
	begin		
		-- Set the log threshold. Everything that happens in the gui may be logged
		-- using the gui wide variable log_threshold:
		log_threshold := log_threshold_in;

		log (text => "init_schematic", level => log_threshold);


		
		-- Set the current active project, module and sheet:
		log (text => "set active project " & enclose_in_quotes (to_string (project)), 
			 level => log_threshold);

		active_project := project;


		
		-- Set the current active module:
		log (text => "set active module " & enclose_in_quotes (to_string (key (module))),
			 level => log_threshold);

		active_module := module;


		
		-- Set the current active sheet:
		log (text => "set active sheet " & type_sheet'image (sheet),
			 level => log_threshold);

		active_sheet := sheet;

		
		
		-- CS set_grid_to_scale;
		compute_canvas_size;
		compute_bounding_box;
		set_base_offset;

		
		-- Set up general things of the main window:
		pac_canvas.set_up_main_window;

		-- Set up special things of the main window:
		et_canvas_schematic_2.set_up_main_window;

		-- Set the title bar of the main window:
		set_title_bar (
			module	=> active_module);
  

		-- Set up the primary tool display:
		log (text => "build primary tool display", level => log_threshold + 1);
		build_primary_tool_display;

		-- Set up the sheet number display:
		log (text => "build sheet number display", level => log_threshold + 1);
		build_sheet_number_display;
		
		-- Set up the coordinates display:
		log (text => "build coordinates display", level => log_threshold + 1);
		set_up_coordinates_display;

		log (text => "build mode display", level => log_threshold + 1);
		build_mode_display;

		-- Set up the console:
		log (text => "build console", level => log_threshold + 1);
		build_console;
		connect_console;


		-- Set up the scrolled window and the scrollbars:
		set_up_swin_and_scrollbars;

		-- Set up general things of the canvas:
		pac_canvas.set_up_canvas;

		-- Set up special things of the canvas:
		et_canvas_schematic_2.set_up_canvas;

		
		-- Activate the main window:
		log (text => "show schematic window", level => log_threshold + 1);
		main_window.show_all;

		-- Initialize the scrollbars:
		set_initial_scrollbar_settings;
		canvas.grab_focus;

		-- Set zoom so that all objects fit into the scrolled window:
		zoom_to_fit (bounding_box);		

		-- Backup the currently visible area.
		-- This is relevant for canvas mode MODE_3_ZOOM_FIT only:
		backup_visible_area (bounding_box);

	
		update_sheet_number_display;
		update_zoom_display;
		update_scale_display;

		-- Set the grid as specified in the database:
		pac_canvas.grid := et_schematic_ops.grid.get_grid (module, log_threshold + 1);
		update_grid_display;
		
		-- CS ? canvas.update_mode_display;

	end init_schematic;




	
	
	procedure init_board (
		project			: in pac_project_name.bounded_string;	-- blood_sample_analyzer
		module			: in pac_generic_modules.cursor; -- cursor of generic module to be edited
		log_threshold_in: in type_log_level) 
	is
		use et_canvas_board_2;
		use et_canvas_board_2.pac_canvas;
		use et_board_geometry.pac_geometry_2;
	begin
		-- Set the log threshold. Everything that happens in the gui may be logged
		-- using the gui wide variable log_threshold:
		log_threshold := log_threshold_in;

		log (text => "init_board", level => log_threshold);

		
		
		-- CS set_grid_to_scale;
		compute_canvas_size;
		compute_bounding_box;
		set_base_offset;

		
		-- Set up general things of the main window:
		pac_canvas.set_up_main_window;

		-- Set up special things of the main window:
		et_canvas_board_2.set_up_main_window;

		-- Set the title bar of the main window:
		set_title_bar (
			module	=> active_module);

		
		-- Set up the primary tool display:
		log (text => "build primary tool display", level => log_threshold + 1);
		build_primary_tool_display;

		-- Set up the coordinates display:
		log (text => "build coordinates display", level => log_threshold + 1);
		set_up_coordinates_display;

		log (text => "build mode display", level => log_threshold + 1);
		build_mode_display;

		-- Set up the console:
		log (text => "build console", level => log_threshold + 1);
		build_console;
		connect_console;
		
		-- Set up the scrolled window and the scrollbars:
		set_up_swin_and_scrollbars;

		-- Set up general things of the canvas:
		pac_canvas.set_up_canvas;

		-- Set up special things of the canvas:
		et_canvas_board_2.set_up_canvas;

		
		-- Activate the main window:
		log (text => "show board window", level => log_threshold + 1);		
		main_window.show_all;

		-- Initialize the scrollbars:
		set_initial_scrollbar_settings;

		-- Set zoom so that all objects fit into the scrolled window:
		zoom_to_fit (bounding_box);

		-- Backup the currently visible area.
		-- This is relevant for canvas mode MODE_3_ZOOM_FIT only:
		backup_visible_area (bounding_box);


		update_zoom_display;
		update_scale_display;

		-- Set the grid as specified in the database:
		pac_canvas.grid := et_board_ops.grid.get_grid (module, log_threshold + 1);
		update_grid_display;

		-- CS ? canvas.update_mode_display;

	end init_board;




	
	
	procedure single_module (
		project			: in pac_project_name.bounded_string;	-- blood_sample_analyzer
		module			: in pac_generic_modules.cursor;				-- cursor of generic module
		sheet			: in et_sheets.type_sheet := et_sheets.type_sheet'first; -- the sheet to be opened
		script			: in pac_script_name.bounded_string; -- rename_nets.scr
		log_threshold	: in type_log_level) 
	is
		use et_sheets;
		use et_schematic_coordinates;
	begin
		log (text => row_separator_single, level => log_threshold);
		log (text => "starting GUI ...", level => log_threshold);
		log (text => "project " & enclose_in_quotes (to_string (project)), level => log_threshold);
		log (text => "runmode " & to_string (MODE_MODULE), level => log_threshold);
		log (text => "module " & enclose_in_quotes (to_string (pac_generic_modules.key (module))), level => log_threshold);
		log (text => "sheet" & to_string (sheet), level => log_threshold);

		if pac_script_name.length (script) > 0 then
			log (text => "script " & enclose_in_quotes (to_string (script)), level => log_threshold);
		end if;

		log (text => "init gtk main (GUI) ... ", level => log_threshold);
		log_indentation_up;
		
		gtk.main.init;

		-- Set up the schematic window.
 		init_schematic (project, module, sheet, log_threshold + 1);

		-- CS test if board available (see et_schematic.type_module)
		
		-- Set up the board window.
		init_board (project, module, log_threshold + 1);

		-- Activate the schematic window:
		et_canvas_schematic_2.pac_canvas.main_window.present;

		-- CS
		--et_canvas_schematic.pac_canvas.console.grab_focus;

		
		-- If a script was given on startup as argument, execute it now:
		-- NOTE 1: The script execution must start AFTER BOTH schematic and board 
		--         have been completely displayed.
		-- NOTE 2: The procedure execute_script_console is available 
		--         in et_canvas_schematic_2 and et_canvas_board_2.
		--         Both launch the script in the same way. But in case there is no board
		--         available, it is more reasonable to launch the script from the schematic.
		if pac_script_name.length (script) > 0 then
  
			--et_gui.schematic_callbacks.execute_script (script);
			et_canvas_schematic_2.execute_script_console (script);
			-- 1. Composes a command that executes the script
			--    like "schematic motor_driver execute script my_script.scr"
			--    as if it was entered by the operator as an ordinary command.
			-- 2. Launches the script via procedure et_scripting.execute_schematic_command.
			-- 3. Procedure et_scripting.execute_schematic_command in turn calls 
			--    et_scripting.execute_nested_script.
			-- 4. et_scripting.execute_nested_script reads the script line per line
			--    and calls for each line et_scripting.execute_command.
			-- 5. et_scripting.execute_script_command parses the command and dispatches
			--    to procedure execute_schematic_command, execute_board_command 
			--    or execute_project_command.
		end if;
  

		et_canvas_schematic_2.pac_canvas.update_grid_display;
		et_canvas_board_2.pac_canvas.update_grid_display;

		et_canvas_schematic_2.update_mode_display;
		et_canvas_board_2.update_mode_display;
		
		-- CS Init defaults of property bars in schematic.

		-- Init defaults of property bars in board:
		-- et_canvas_board.init_property_bars;
		

		
		-- Start the main gtk loop. This is a loop that permanently draws the widgets and
		-- samples them for possible signals sent.
		gtk.main.main;
		
		log_indentation_down;
		
	end single_module;	


	
end et_gui_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
