------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             GUI GENERAL                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with et_canvas_schematic;

with et_gui.board_callbacks;
with et_canvas_board;

package body et_gui is

	procedure init_schematic (
		project			: in pac_project_name.bounded_string;	-- blood_sample_analyzer
		module			: in pac_generic_modules.cursor; -- cursor of generic module to be edited
		sheet			: in et_coordinates.type_sheet := et_coordinates.type_sheet'first; -- the sheet to be opened
		log_threshold_in: in type_log_level) is

		use et_canvas_schematic;
		use et_canvas_schematic.pac_canvas;
		
	begin
		-- Set the log threshold. Everything that happens in the gui may be logged
		-- using the gui wide variable log_threshold:
		log_threshold := log_threshold_in;

		-- Set the current project name:
		current_active_project := project;
		
		gtk_new (window); -- create the main window (where pointer "window" is pointing at)

		-- Show the module name and sheet number in the title bar:
		set_title_bar (pac_generic_modules.key (module), sheet);
		
		window.set_default_size (1024, 768);

		-- If the operator wishes to terminate the program (by clicking X)
		-- the procedure terminate_main (in gui_cb) is to be called.
		window.on_destroy (terminate_main'access);

		-- If the operator minimizes, maximizes or changes the size in some way:
		--window.on_configure_event (window_resized'access);

		-- For reaction to keys pressed on the keyboard:
		window.on_key_press_event (on_key_event'access);
		
		build_background_boxes;

		build_coordinates_display;

		build_mode_display;

		-- Connect to the on_activate signal (on hitting enter key):
		gtk_entry (cursor_position_x.get_child).on_activate (set_cursor_position_x'access);
		gtk_entry (cursor_position_y.get_child).on_activate (set_cursor_position_y'access);

		gtk_entry (grid_x.get_child).on_activate (set_grid_x'access);
		gtk_entry (grid_y.get_child).on_activate (set_grid_y'access);
		
		build_toolbars;
		button_zoom_to_fit.on_clicked (zoom_to_fit'access, toolbar_left);


		
-- 		-- Create a button and place it in the toolbar:
-- 		gtk.tool_button.gtk_new (button_zoom_in, label => "IN");
-- 		insert (toolbar, button_zoom_in);
-- 
-- 		-- If the operator clicks the button
-- 		-- call the procedure zoom_in in package callbacks_4:
-- 		button_zoom_in.on_clicked (zoom_in'access, toolbar);



		
		build_console;
		
		-- Connect to the on_activate signal of the entry (which is a child of console):
		gtk_entry (console.get_child).on_activate (et_canvas_schematic.execute_command'access); -- on hitting enter
		

		build_canvas;
		gtk_new (canvas);
		
		-- set the module to be opened and optionally the sheet to be displayed:
		init_drawing (module, sheet);

		add (scrolled, canvas); -- place the canvas in the scrolled window
		
		scale_to_fit (canvas);

		-- display the schematic:
		window.show_all;

		canvas.update_mode_display;
		
	end init_schematic;

	
	procedure init_board (
		project			: in pac_project_name.bounded_string;	-- blood_sample_analyzer
		module			: in pac_generic_modules.cursor; -- cursor of generic module to be edited
		log_threshold_in: in type_log_level) is

		use et_gui.board_callbacks;
		use et_canvas_board;
		use et_canvas_board.pac_canvas;
		
	begin
		-- Set the log threshold. Everything that happens in the gui may be logged
		-- using the gui wide variable log_threshold:
		pac_canvas.log_threshold := log_threshold_in;

		gtk_new (window); -- create the main window (where pointer "window" is pointing at)

		-- Show the module name in the title bar:
		set_title_bar (pac_generic_modules.key (module));
		
		window.set_default_size (1024, 768);

		-- If the operator wishes to terminate the program (by clicking X)
		-- the procedure terminate_main (in gui_cb) is to be called.
		window.on_destroy (terminate_main'access);

		-- If the operator minimizes, maximizes or changes the size in some way:
		--window.on_configure_event (window_resized'access);

		-- For reaction to keys pressed on the keyboard:
		window.on_key_press_event (on_key_event'access);
		

		build_background_boxes;
		
		build_coordinates_display;

		build_mode_display;
		
		-- Connect to the on_activate signal (on hitting enter key):
		gtk_entry (cursor_position_x.get_child).on_activate (set_cursor_position_x'access);
		gtk_entry (cursor_position_y.get_child).on_activate (set_cursor_position_y'access);

		gtk_entry (grid_x.get_child).on_activate (set_grid_x'access);
		gtk_entry (grid_y.get_child).on_activate (set_grid_y'access);


		
		build_toolbars;
		button_zoom_to_fit.on_clicked (zoom_to_fit'access, toolbar_left);

-- 		gtk.tool_button.gtk_new (button_zoom_in, label => "IN");
-- 		insert (toolbar, button_zoom_in);
-- 
-- 		-- If the operator clicks the button
-- 		-- call the procedure zoom_in in package callbacks_4:
-- 		button_zoom_in.on_clicked (zoom_in'access, toolbar);


		build_console;

		-- Connect to the on_activate signal of the entry (which is a child of console):
		gtk_entry (console.get_child).on_activate (execute_command'access); -- on hitting enter

		build_canvas;
		gtk_new (canvas);

		--init_drawing;
		
		add (scrolled, canvas); -- place the canvas in the scrolled window
		
		scale_to_fit (canvas);
		
		-- display the board:
		window.show_all;

		canvas.update_mode_display;
		
	end init_board;


	
	procedure single_module (
		project			: in pac_project_name.bounded_string;	-- blood_sample_analyzer
		module			: in pac_generic_modules.cursor;				-- cursor of generic module
		sheet			: in et_coordinates.type_sheet := et_coordinates.type_sheet'first; -- the sheet to be opened
		script			: in pac_script_name.bounded_string; -- rename_nets.scr
		log_threshold	: in type_log_level) is

		use et_coordinates;
	begin
		log (text => row_separator_single, level => log_threshold);
		log (text => "starting GUI ...", level => log_threshold);
		log (text => "project " & enclose_in_quotes (to_string (project)), level => log_threshold);
		log (text => "runmode " & to_string (MODE_MODULE), level => log_threshold);
		log (text => "module " & enclose_in_quotes (to_string (pac_generic_modules.key (module))), level => log_threshold);
		log (text => "sheet" & to_sheet (sheet), level => log_threshold);

		if pac_script_name.length (script) > 0 then
			log (text => "script " & enclose_in_quotes (to_string (script)), level => log_threshold);
		end if;

		gtk.main.init; -- initialize the main gtk stuff

		-- Set up the schematic window.
		init_schematic (project, module, sheet, log_threshold + 1);

		-- CS test if board available (see et_schematic.type_module)
		
		-- Set up the board window.
		init_board (project, module, log_threshold + 1);

		-- If a script was given on starup as argument, execute it now:
		-- NOTE 1: The script execution must start AFTER BOTH schematic and board 
		--         have been completely displayed.
		-- NOTE 2: The procedure execute_script is available in gui_board and gui_schematic.
		--         Both launch the script in the same way. But in case there is no board
		--         available, it is more reasonable to launch the script from the schematic.
		if pac_script_name.length (script) > 0 then

			--et_gui.schematic_callbacks.execute_script (script);
			et_canvas_schematic.execute_script (script);
			-- 1. Composes a command that executes the script
			--    like "schematic motor_driver execute script my_script.scr"
			--    as if it was entered by the operator as an ordinary command.
			-- 2. Launches the script via procedure et_scripting.schematic_cmd.
			-- 3. Procedure et_scripting.schematic_cmd in turn calls 
			--    et_scripting.execute_nested_script.
			-- 4. et_scripting.execute_nested_script reads the script line per line
			--    and calls for each line et_scripting.execute_command.
			-- 5. et_scripting.execute_command parses the command and dispatches
			--    to procedure schematic_cmd, board_cmd or project_cmd.
		end if;
		
		-- Start the main gtk loop. This is a loop that permanently draws the widgets and
		-- samples them for possible signals sent.
		gtk.main.main;
		
	end single_module;	
	
end et_gui;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
