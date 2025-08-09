------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS FOR SCHEMATIC                              --
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

with ada.text_io;					use ada.text_io;
with ada.characters.handling;		use ada.characters.handling;
with ada.strings;					use ada.strings;
with ada.strings.fixed;				use ada.strings.fixed;
with ada.directories;
with ada.exceptions;				use ada.exceptions;

with ada.calendar;					use ada.calendar;
with ada.calendar.formatting;		use ada.calendar.formatting;

with ada.containers;

-- with et_pcb_coordinates;
-- with et_terminals;
-- with et_devices;					use et_devices;
-- 
with et_domains;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_primitive_objects;			use et_primitive_objects;
with et_axes;						use et_axes;
with et_command_processor;
with et_modes;						--use et_modes;
with et_modes.board;
with et_modes.schematic;	

with et_canvas_board_2;
with et_display.schematic;			--use et_display.schematic;
with et_colors.schematic;

with et_canvas_tool;					use et_canvas_tool;

with et_canvas_schematic_nets;
with et_canvas_schematic_units;

with et_undo_redo;

with et_schematic_ops.grid;
with et_schematic_ops.units;
with et_schematic_ops.nets;
with et_board_ops.grid;
with et_system_info;
with et_project_name;
with et_module_ops;
with et_canvas_schematic_preliminary_object;
with et_cmd_sts;						use et_cmd_sts;
with et_script_processor;


package body et_canvas_schematic_2 is


	procedure set_title_bar (
		-- CS project name
		module		: in pac_generic_modules.cursor)
	is 
		use et_system_info;
	begin		
		main_window.set_title (
			system_name 
			& " - SCHEMATIC - " 
			-- CS project name					  
			& to_string (key (module)));
	end set_title_bar;


	
	procedure update_mode_display is 
		use et_modes.schematic;
		
		-- Get the current drawing mode
		v : constant string := to_string (verb);
		n : constant string := to_string (noun);
	begin
		-- show the drawing mode
		gtk_entry (mode_display.cbox_mode_verb.get_child).set_text (v);
		gtk_entry (mode_display.cbox_mode_noun.get_child).set_text (n);
	end update_mode_display;
	
	

	
	procedure compute_bounding_box (
		abort_on_first_error	: in boolean := false;
		ignore_errors			: in boolean := false;
		test_only				: in boolean := false)		
	is separate;


	

	procedure zoom_to_fit_all is
		-- debug : boolean := true;
		debug : boolean := false;
	begin
		-- put_line ("zoom_to_fit");

		-- Reset the translate-offset:
		T := (0.0, 0.0);
		
		-- Compute the new bounding-box. Update global
		-- variable bounding_box:
		compute_bounding_box;

		-- In order to simulate a violation of the maximal
		-- size of the bounding-box try this:
		-- compute_bounding_box (ignore_errors => true);
		-- compute_bounding_box (test_only => true, ignore_errors => true);

		-- Compute the new base-offset. Update global variable F:
		set_base_offset;

		-- Since the bounding_box has changed, the scrollbars
		-- must be reinitialized:
		set_initial_scrollbar_settings;

		-- Calculate the zoom factor that is required to
		-- fit all objects into the scrolled window:
		S := get_ratio (bounding_box);

		
		
		if debug then
			put_line (" S: " & type_zoom_factor'image (S));
		end if;

		update_zoom_display;


		-- Calculate the translate-offset that is required to
		-- "move" all objects to the center of the visible area:
		center_to_visible_area (bounding_box);

		backup_visible_area (bounding_box);
		
		-- Schedule a redraw of the canvas:
		refresh;
	end zoom_to_fit_all;




	procedure cb_zoom_to_fit (
		button : access gtk_button_record'class)
	is
		-- debug : boolean := true;
		debug : boolean := false;
	begin
		put_line ("cb_zoom_to_fit");

		zoom_to_fit_all;
	end cb_zoom_to_fit;


	
	
	procedure cb_zoom_area (
		button : access gtk_button_record'class)
	is
		use et_modes.schematic;
		
		-- debug : boolean := true;
		debug : boolean := false;
	begin
		put_line ("cb_zoom_area (schematic)");

		reset_verb_and_noun;
		update_mode_display;
		
		zoom_area.active := true;
	end cb_zoom_area;


	

	procedure set_up_command_buttons is
	begin
		put_line ("set_up_command_buttons (schematic)");

		-- Connect button signals with subprograms:
		
		button_zoom_fit.on_clicked (cb_zoom_to_fit'access);
		-- button_zoom_fit.on_clicked (access_cb_zoom_to_fit);		

		button_zoom_area.on_clicked (cb_zoom_area'access);
		
	end set_up_command_buttons;

	
	

	function cb_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean
	is
		event_handled : boolean := false;

		-- If event_handled is true then
		-- this event is not passed further
		-- to widgets down the chain.
		-- Prosssing the event stops here.
		
		use gdk.types;		
		use gdk.types.keysyms;
		
		key_ctrl	: gdk_modifier_type := event.state and control_mask;
		key_shift	: gdk_modifier_type := event.state and shift_mask;
		key			: gdk_key_type := event.keyval;



		-- This procedure contains all the actions required
		-- to set the focus on the canvas:
		procedure focus_canvas is begin
			backup_scrollbar_settings;
			canvas.grab_focus;
			restore_scrollbar_settings;
			
			status_clear;
			
			event_handled := true;
		end focus_canvas;
	

		-- This procedure contains all the actions required
		-- to set the focus on the console:
		procedure focus_console is begin
			console.grab_focus;
			set_status ("enter command");

			event_handled := true;
		end focus_console;
		
			
	begin
		-- Output the the gdk_key_type (which is
		-- just a number (see gdk.types und gdk.types.keysyms)):
		
		-- put_line ("cb_window_key_pressed (schematic)"
		-- 	& " key " & gdk_key_type'image (event.keyval));

		if key_ctrl = control_mask then 
			case key is
					
				when others => null;
			end case;

		else
			case key is

				-- If the operator presses F2 then change the primary tool:
				when GDK_F2 =>
					change_primary_tool;
					
					event_handled := true; -- event handled


				-- If the operator presses F3 then set the focus to the console:
				when GDK_F3 =>
					focus_console;

					
				-- If the operator presses F4 then set the focus to the canvas:
				when GDK_F4 =>
					focus_canvas;
					
					
				when GDK_F5 =>
					zoom_to_fit_all;

					-- Do not pass this event further
					-- to widgets down the chain.
					-- Prosssing the event stops here.
					event_handled := true;



				-- Switch between modules:	
				when GDK_F11 =>
					switch_module (PREVIOUS);
					event_handled := true;
					
				when GDK_F12 =>
					switch_module (NEXT);
					event_handled := true;
					

				-- Other keys are propagated to the canvas:
				when others =>
					event_handled := false;
					
			end case;
		end if;
		
		return event_handled;
	end cb_window_key_pressed;



	

	procedure set_up_main_window is begin
		log (text => "set_up_main_window (schematic)", level => log_threshold);

		main_window.on_key_press_event (cb_window_key_pressed'access);

		set_up_command_buttons;
	end set_up_main_window;






	procedure draw_drawing_frame is separate;

	procedure draw_units is separate;

	procedure draw_nets is separate;
	
	procedure draw_texts is separate;

	procedure draw_submodules is separate;
	

	
	
	function cb_draw (
		canvas		: access gtk_widget_record'class;
		context_in	: in cairo.cairo_context)
		return boolean
	is
		use cairo;
		use et_display.schematic;
		
		event_handled : boolean := true;
	
	begin
		-- new_line;
		-- put_line ("cb_draw (schematic) " & image (clock));

		-- Update the global context:
		context := context_in;

		
		-- Update the global visible_area:
		visible_area := get_visible_area (canvas);
		-- put_line (" visible " & to_string (visible_area));

		-- Set the background color:
		set_source_rgb (context, 0.0, 0.0, 0.0); -- black
		-- set_source_rgb (context, 1.0, 1.0, 1.0); -- white
		paint (context);

		-- The ends of all kinds of lines are round:
		set_line_cap (context, cairo_line_cap_round);

		draw_grid;		
		draw_drawing_origin;

		
		-- Compute the displacement in case objects are being moved or dragged.
		-- Other objects could be dragged along if they are attached to the
		-- primary object.
		-- The displacement is requred for secondary objects which are
		-- dragged or moved along with the primary object:
		object_displacement := get_object_tool_position - object_original_position;
		-- put_line ("object_displacement " & to_string (object_displacement));

		
		draw_drawing_frame;	
		draw_cursor;
		draw_zoom_area;

		draw_units;
		
		if nets_enabled then
			draw_nets;
		end if;


		if texts_enabled then
			draw_texts;
		end if;


		-- CS if submodules_enables ?
		draw_submodules;
	
		
		return event_handled;
	end cb_draw;



	


-- UNDO / REDO:
	
	procedure undo is 
		use et_undo_redo;
		use pac_undo_message;
		message : pac_undo_message.bounded_string;
	begin
		-- put_line ("schematic undo");
		undo (message, log_threshold + 1);

		-- Show the undo-message in the status bar:
		set_status (to_string (message));
		
		redraw;
	end undo;



	
	procedure redo is 
		use et_undo_redo;
		use pac_redo_message;
		message : pac_redo_message.bounded_string;
	begin
		-- put_line ("schematic redo");
		redo (message, log_threshold + 1);

		-- Show the redo-message in the status bar:
		set_status (to_string (message));

		redraw;
	end redo;





-- RESET:
	
	procedure reset_selections is 
		use et_canvas_schematic_nets;
		use et_canvas_schematic_units;
	begin
		-- Verb and noun remain as they are
		-- so that the mode is unchanged.
		
		reset_request_clarification;
		
		reset_preliminary_segment; -- after move/drag/draw of a net segment
		reset_segments_being_dragged; -- after dragging a unit
		-- reset_unit_move; -- after moving/dragging a unit
		reset_unit_add; -- after adding a device
		
		reset_label; -- after placing a label
		
		reset_placeholder; -- after moving a placeholder

		-- reset_activate_counter;

		reset_zoom_area; -- abort zoom-to-area operation
	end reset_selections;



	
	procedure reset is
		use et_modes;
		use et_modes.schematic;
		use et_canvas_schematic_nets;


		-- Do a level 1 reset. This is a partly reset:
		procedure level_1 is begin
			log (text => "level 1", level => log_threshold + 1);
			
			reset_edit_process_running;
			reset_request_clarification;
			reset_finalizing_granted;

			status_clear;

			et_schematic_ops.nets.reset_proposed_objects (active_module, log_threshold + 1);
			
			et_schematic_ops.units.reset_proposed_objects (active_module, log_threshold + 1);
			
			reset_selections; -- CS

			rename_window_open := false;
		end level_1;
	

		-- Do a level 2 reset. This is a full reset:
		procedure level_2 is begin
			level_1;
			
			log (text => "level 2", level => log_threshold + 1);
						
			reset_verb_and_noun;
			update_mode_display;
			
			status_enter_verb;
			-- clear_out_properties_box;
			reset_editing_process;
		end level_2;


		
	begin
		log (text => "RESET (schematic)", level => log_threshold + 1);
		log_indentation_up;
		
		escape_key_pressed;

		expect_entry := expect_entry_default; -- expect a verb
		
		-- Verb and noun remain as they are
		-- so that the mode is unchanged.

		case get_escape_counter is
			when 0 => null;
			
			when 1 =>
				case verb is
					-- CS
					
					when others => level_1;
				end case;
				
			when 2 =>
				level_2;

		end case;

		log_indentation_down;
		
		redraw_schematic;
	end reset;

	


	

	procedure save_module is
		use ada.directories;
		use et_project_name;
		use et_project;
		use et_module_ops;

		-- Backup the current directory (like /home/user/et/blood_sample_analyzer):
		cur_dir_bak : constant string := current_directory;
	begin
		-- NOTE: We are not in the project directory yet but
		-- in its parent directory.
		
		-- Change into the directory of the current project:
		set_directory (to_string (active_project));
		
		-- Save the module with its own name:
		save_module (
			module_cursor	=> active_module,
			log_threshold	=> log_threshold + 1);

		-- Return to previous directory:
		set_directory (cur_dir_bak);
		
		-- Show a brief message in the schematic status bar:
		set_status (status_text_module_saved);

		-- Show a brief message in the board status bar:
		et_canvas_board_2.pac_canvas.set_status (status_text_module_saved);
	end save_module;


	


	

	

	
	procedure key_pressed (
		key			: in gdk_key_type;
		key_shift	: in gdk_modifier_type)
	is separate;

	

	function cb_canvas_key_pressed (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean
	is
		event_handled : boolean := true;

		use gdk.types;		
		use gdk.types.keysyms;
		
		key_ctrl	: gdk_modifier_type := event.state and control_mask;
		key_shift	: gdk_modifier_type := event.state and shift_mask;
		key			: gdk_key_type := event.keyval;


		-- Advances to next grid density up or down:
		procedure set_grid is
			use et_schematic_coordinates.pac_grid;
		begin
			case key is
				when GDK_Shift_L =>
					next_grid_density (grid, GRID_UP);

				when GDK_Shift_R =>
					next_grid_density (grid, GRID_DOWN);

				when others => null;
			end case;
			
			update_grid_display;
			refresh;
		end set_grid;

		
	begin
		-- Output the the gdk_key_type (which is
		-- just a number (see gdk.types und gdk.types.keysyms)):

		-- put_line ("cb_canvas_key_pressed (schematic)"
		-- 	& " key " & gdk_key_type'image (event.keyval));

		if key_ctrl = control_mask then 
			case key is
				-- Zoom in/out on ctrl and +/- key:
				when GDK_KP_ADD | GDK_PLUS =>
					zoom_on_cursor (ZOOM_IN);

					
				-- Zoom on cursor:
				when GDK_KP_SUBTRACT | GDK_MINUS =>
					zoom_on_cursor (ZOOM_OUT);

					
				-- Advance to next grid density on ctrl and shift
				when GDK_Shift_L | GDK_Shift_R => -- CS: ALT key ?
					set_grid;
					
					
				-- Undo the last operation on ctrl-z
				when GDK_LC_z =>
					undo;

				-- Redo the last operation on ctrl-y
				when GDK_LC_y => -- CS shift + ctrl-z
					redo;


				-- Save the module on ctrl-s or ctrl-S:
				when GDK_LC_s | GDK_s =>
					save_module;
					
					
				when others => null;
			end case;

			
		else
			case key is
				when GDK_ESCAPE =>
					reset;
					
				when GDK_Right =>
					move_cursor (DIR_RIGHT);

				when GDK_Left =>
					move_cursor (DIR_LEFT);

				when GDK_Up =>
					move_cursor (DIR_UP);

				when GDK_Down =>
					move_cursor (DIR_DOWN);

				when GDK_HOME | GDK_KP_HOME =>
					-- Move the cursor to the grid point that
					-- is nearest to the center of the visible area:
					put_line ("move cursor to center");
					move_cursor (snap_to_grid (get_center (visible_area)));
					refresh;

				-- Advance to next sheet:
				when GDK_KP_Add =>
					active_sheet := active_sheet + 1;
					update_sheet_number_display;
					refresh;

				-- Advance to previous sheet:
				when GDK_KP_Subtract =>
					if active_sheet > sheet_default then
						active_sheet := active_sheet - 1;
						update_sheet_number_display;
						refresh;
					end if;

					
				when others => 
					key_pressed (key, key_shift);
					
			end case;
		end if;
		
		return event_handled;
	end cb_canvas_key_pressed;



-- MOUSE BUTTON PRESSED

	procedure button_pressed (
		event	: in type_mouse_event)
	is separate;

	

	function cb_canvas_button_pressed (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean
	is
		event_handled : boolean := true;

		mouse_event : type_mouse_event;
	begin
		-- put_line ("cb_canvas_button_pressed (schematic)");

		mouse_event := get_mouse_button_pressed_event (event);

		button_pressed (mouse_event);
		
		return event_handled;
	end cb_canvas_button_pressed;
	


-- MOUSE BUTTON RELEASED
	
	-- CS procedure button_released (
	-- 	event	: in type_mouse_event)
	-- is separate;

	

	function cb_canvas_button_released (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_button)
		return boolean
	is
		event_handled : boolean := true;

		mouse_event : type_mouse_event;
		
		debug : boolean := false;
	begin
		-- put_line ("cb_canvas_button_released (schematic)");
		
		mouse_event := get_mouse_button_released_event (event);

		 -- CS button_released (mouse_event);
		
		return event_handled;
	end cb_canvas_button_released;
	


	
-- MOUSE MOVED

	procedure mouse_moved (
		point	: in type_vector_model) 
	is separate;
	

	function cb_canvas_mouse_moved (
		canvas	: access gtk_widget_record'class;
		event	: gdk_event_motion)
		return boolean
	is
		event_handled : boolean := true;

		mp : type_vector_model;
	begin
		-- put_line ("cb_canvas_mouse_moved (schematic)");

		-- Get from the mouse event the model point:
		mp := get_mouse_moved_event (event);

		mouse_moved (mp);
		
		return event_handled;
	end cb_canvas_mouse_moved;


	
	procedure set_up_canvas is begin
		put_line ("set_up_canvas (schematic)");

		-- Connect signals:

		canvas.on_draw (cb_draw'access);
		-- NOTE: No context is declared here, because the canvas widget
		-- passes its own context to the callback procedure cb_draw.

		
		canvas.on_key_press_event (cb_canvas_key_pressed'access);

		canvas.on_button_press_event (cb_canvas_button_pressed'access);
		canvas.on_button_release_event (cb_canvas_button_released'access);

		canvas.on_motion_notify_event (cb_canvas_mouse_moved'access);
	end set_up_canvas;

	

	
	procedure update_sheet_number_display is begin
		gtk_entry (cbox_sheet.get_child).set_text (to_string (active_sheet));
	end update_sheet_number_display;

	
	procedure build_sheet_number_display is
		spacing : gint;
	begin
		spacing := 10;

		gtk_new_vbox (box_sheet);
		set_spacing (box_sheet, spacing);
		pack_start (box_v1, box_sheet, expand => false);
		
		gtk_new (label_sheet, "SHEET (KEYPAD +/-)");
		pack_start (box_sheet, label_sheet, expand => false);
		gtk_new_with_entry (cbox_sheet);
		pack_start (box_sheet, cbox_sheet);
	end build_sheet_number_display;

	
	

	

-- REDRAW / REFRESH:
	
	procedure redraw_schematic is begin
		refresh;
	end redraw_schematic;

	
	procedure redraw_board is begin
		et_canvas_board_2.pac_canvas.refresh;
	end redraw_board;

	
	procedure redraw is begin
		redraw_schematic;
		redraw_board;
	end redraw;



	

	
-- MODULE SELECT:

	
	procedure update_schematic_editor is 
		use et_schematic_ops.grid;
	begin

		-- Show the module name in the title bar of 
		-- the schematic editor:
		set_title_bar (active_module);

		grid := get_grid (active_module, log_threshold + 1);
		update_grid_display;

		update_sheet_number_display;
		
		-- CS Init defaults of property bars in schematic.

		-- CS
		-- zoom-fit ?
		-- move cursor home ?
		-- set sheet ?
		-- displayed objects, layers, ... ?
		-- These things could be stored in the database in the future
		-- so that they can be applied as soon as a module is selected.
	end update_schematic_editor;



	
	procedure switch_module (
		sel : in type_module_select)
	is			
		-- use ada.containers;
		-- module_ct : count_type;
		
	begin
		--put_line ("switch_module");

		-- module_ct := pac_generic_modules.length (generic_modules);
		-- put_line ("module count" & count_type'image (module_ct));

		case sel is
			when NEXT =>
				-- Advance to next module:
				active_module := pac_generic_modules.next (active_module);

				-- If there is no next module, select first module:
				if active_module = pac_generic_modules.no_element then
					-- put_line ("no next");
					active_module := generic_modules.first;
				end if;

			when PREVIOUS =>
				-- Advance to previous module:
				active_module := pac_generic_modules.previous (active_module);

				-- If there is no previous module, select last module:
				if active_module = pac_generic_modules.no_element then
					active_module := generic_modules.last;
				end if;
		end case;

				
		--put_line (to_string (key (active_module)));
		
		-- Switch module in schematic and board editor:
		update_schematic_editor;
		et_canvas_board_2.update_board_editor;
		
		redraw; -- schematic and board
	end switch_module;




	procedure set_module (
		module	: in pac_module_name.bounded_string)  -- motor_driver
	is
		use et_module_ops;
		cursor : pac_generic_modules.cursor := find (generic_modules, module);
	begin
		-- If module already loaded in collection of generic modules, set the active_module:
		if cursor /= pac_generic_modules.no_element then 
			active_module := cursor;
		else
			-- If module not loaded yet, read it and store it in collection of generic modules:
			read_module (
				file_name		=> append_extension (to_string (module)),
				log_threshold	=> log_threshold + 1);

		end if;

		-- CS exception handler could catch semantic_error_1 and show
		-- a list of available modules (which are inside the project directory)
		-- in the status bar.
	end set_module;


	
	
	procedure connect_console is begin
		-- Connect to the on_activate signal of the 
		-- entry (which is a child of console):
		gtk_entry (console.get_child).on_activate 
			(execute_command'access); -- on hitting enter

	end connect_console;



	
	
	procedure execute_script_console (
		script : in pac_script_name.bounded_string) 
	is
		use ada.directories;
		use et_project_name;
		use et_command_processor;
		use et_modes;
		use et_project;
		use et_domains;
		
		-- We compose a command that executes a script
		-- like "schematic motor_driver execute script my_script.scr:
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_SCHEMATIC)) & space &
			get_active_module & space &
			"execute" & space & "script" & space &
			to_string (script); -- "my_script.scr"
		
		fields : et_string_processing.type_fields_of_line;

		-- The single command to be executed:
		single_cmd : type_single_cmd;
		
		-- The command launches a script. Change into the project directory. 
		-- The current directory is the parent directory of the active project. 
		-- Example: The current directory is /home/user/my_projects . The directory
		--  of the current project is /home/user/my_projects/blood_sample_analyzer.
		--  Executing a script requires changing into the project directory blood_sample_analyzer.

		-- Backup the current directory (like /home/user/my_projects):
		cur_dir_bak : constant string := current_directory;
		
	begin

		log (text => "executing script (in schematic domain) " 
			 & enclose_in_quotes (line_as_typed_by_operator), 
			 level => log_threshold);
		
		log_indentation_up;
		
		-- Store the command in the command history:
	-- CS console.prepend_text (line_as_typed_by_operator);

		fields := read_line (
			line 			=> line_as_typed_by_operator,
			number			=> 1,  -- this is the one and only line
			comment_mark 	=> et_script_processor.comment_mark,
			delimiter_wrap	=> true, -- strings are enclosed in quotations
			ifs 			=> space); -- fields are separated by space

		--log (text => "full command " & enclose_in_quotes (to_string (cmd)), level => log_threshold + 1);

		set_directory (to_string (active_project));
		
		-- Execute the schematic command.
		-- Since the command is launched from inside the schematic
		-- editor, its origin must be set accordingly:
		set_fields (single_cmd, fields);
		set_origin (single_cmd, ORIGIN_CONSOLE);
		execute_schematic_command (active_module, single_cmd, log_threshold);

		-- Return to previous directory (like  /home/user/my_projects):
		set_directory (cur_dir_bak);
		
		-- The majority of commands requires refreshing the schematic and board drawing.
		
		-- refresh schematic and board
	-- CS redraw;
		--redraw (canvas);
		--et_canvas_board.pac_canvas.redraw (et_canvas_board.pac_canvas.canvas);
		
		-- CS output error message in gui

		-- Once the script has been executed, reset
		-- verb and noun in all domains:
		log (text => "reset verb and noun in all domains", level => log_threshold);
		et_modes.board.reset_verb_and_noun;
		et_modes.schematic.reset_verb_and_noun;
		-- CS other domains ?
		
		log_indentation_down;

		
	exception when event: others =>
		
		-- Return to previous directory (like  /home/user/my_projects):
		set_directory (cur_dir_bak);

		log_indentation_down;
	end execute_script_console;



	
	
	procedure execute_command (self : access gtk_entry_record'class) is 
		use ada.directories;	
		use et_project_name;
		use et_command_processor;
		use et_modes;
		use et_project;
		use et_domains;
		
		-- The operator enters a command like "rename device R1 R2".
		-- The operator is not required to type domain and module name.
		-- Since we are editing a schematic, the domain and module name itelf
		-- are known. By prepending domain and module name here the full 
		-- command after this declaration will be "schematic led_driver rename device R1 R2".
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_SCHEMATIC)) & space &
			get_active_module & space &
			get_text (self);
		
		fields : et_string_processing.type_fields_of_line;

		-- The command to be executed:
		single_cmd : type_single_cmd;
		
		-- The command might launch a script. To prepare for this case we must change
		-- into the project directory. The current directory is the parent directory
		-- of the active project. 
		-- Example: The current directory is /home/user/my_projects . The directory
		--  of the current project is /home/user/my_projects/blood_sample_analyzer.
		--  Executing scripts requires changing into the project directory blood_sample_analyzer.

		-- Backup the current directory (like /home/user/my_projects):
		cur_dir_bak : constant string := current_directory;
	begin
				
		log (text => "executing command " & enclose_in_quotes (get_text (self)), level => log_threshold);
		log_indentation_up;
		
		-- Store the latest command in the command history:
		console.prepend_text (get_text (self));

		fields := read_line (
			line 			=> line_as_typed_by_operator,
			number			=> 1,  -- this is the one and only line
			comment_mark 	=> et_script_processor.comment_mark,
			delimiter_wrap	=> true, -- strings are enclosed in quotations
			ifs 			=> space); -- fields are separated by space

		--log (text => "full command " & enclose_in_quotes (to_string (cmd)), level => log_threshold + 1);

		log (text => "changing to directory " &
				enclose_in_quotes (to_string (active_project)) & " ...",
			level => log_threshold + 1);
		
		set_directory (to_string (active_project));
		
		-- Compose and execute the schematic command.
		-- Since it is launched from inside the board editor
		-- its origin is set accordingly:
		set_fields (single_cmd, fields);
		set_origin (single_cmd, ORIGIN_CONSOLE);
		execute_schematic_command (active_module, single_cmd, log_threshold);

		-- Return to previous directory (like  /home/user/my_projects):
		log (text => "returning to directory " & enclose_in_quotes (cur_dir_bak) & " ...",
			level => log_threshold + 1);
		
		set_directory (cur_dir_bak);
		
		-- The majority of commands requires refreshing both 
		-- the schematic and board:
		redraw;

		
		-- CS output error message in gui

		log_indentation_down;

	exception when event: others =>
		
		-- Return to previous directory (like  /home/user/my_projects):
		log (text => "returning to directory " & enclose_in_quotes (cur_dir_bak) & " ...",
			level => log_threshold + 1);

		set_directory (cur_dir_bak);

		log_indentation_down;
	end execute_command;





	
	procedure reset_grid_and_cursor	is 
	begin
		-- CS et_schematic_coordinates.pac_grid.reset_grid_density (grid);
		-- CS update_grid_display;
		
		move_cursor (snap_to_grid (get_cursor_position));
		update_cursor_coordinates;
	end reset_grid_and_cursor;

	



	
	procedure clear_proposed_objects is 
		use et_canvas_schematic_nets;
		use et_canvas_schematic_units;
	begin
		clear_proposed_units;
		-- clear_proposed_segments;
	end clear_proposed_objects;

	
-- 	procedure reset_properties_selection (
-- 		self : not null access type_view)
-- 	is 
-- 	begin
-- 		--put_line ("reset selection");
-- 		clear_proposed_units;
-- 		clear_proposed_segments;
-- 		
-- 		-- CS reset other stuff ?
-- 	end reset_properties_selection;
-- 
-- 	



-- PROPERTIES WINDOW:


	function window_properties_is_open return boolean is begin
		return window_properties.open;
	end window_properties_is_open;


	procedure build_window_properties is begin
		properties_confirmed := false;
			
		gtk_new (window_properties.window);

		-- If the operator closes the properties window:
	-- CS window_properties.window.on_destroy (access_on_window_properties_closed);

		-- If the operator presses a key in the properties window:
	-- CS window_properties.window.on_key_press_event (access_on_window_properties_key_event);
		
		-- Mark window as open. This prevents the window
		-- from opening multiple times:
		window_properties.open := true;
		
		window_properties.window.set_title ("Properties");
	end build_window_properties;

	
	procedure set_status_properties (text : in string) is begin
		label_properties_status.set_text (text);
	end set_status_properties;

	
	procedure set_property_before (text : in string) is begin
		entry_property_old.set_text (text);
	end set_property_before;


	
	
end et_canvas_schematic_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
