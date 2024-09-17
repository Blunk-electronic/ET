------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS FOR SCHEMATIC                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

-- with et_pcb_coordinates;
-- with et_terminals;
-- with et_devices;					use et_devices;
-- 
with et_scripting;
with et_modes;						--use et_modes;
with et_modes.schematic;		
-- with et_project;

with et_frames;
with et_canvas_board_2;
with et_display.schematic;			--use et_display.schematic;
with et_colors.schematic;

with et_canvas_tool;					use et_canvas_tool;

with et_canvas_schematic_nets;
with et_canvas_schematic_units;

with et_undo_redo;


package body et_canvas_schematic_2 is


	procedure set_title_bar (
		-- CS project name
		module		: in pac_module_name.bounded_string)
	is begin
		main_window.set_title (title & to_string (module));
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
	is
		-- debug : boolean := false;
		debug : boolean := true;

		-- In order to detect whether the bounding-box has
		-- changed we take a copy of the current bounding-box:
		bbox_old : type_area := bounding_box;

		-- This is the temporary bounding-box we are going to build
		-- in the course of this procedure:
		bbox_new : type_area;
		
		-- The first primitive object encountered will be the
		-- seed for bbox_new. All other objects cause 
		-- this bbox_new to expand. After the first object,
		-- this flag is cleared:
		first_object : boolean := true;


		-- This procedure uses the size of the drawing frame
		-- and adds it to the temporary bounding-box bbox_new.
		-- The size expresses the outer border of the frame.
		-- So, tt is sufficient to look at the size only because
		-- all other objects of the frame are definitely inside
		-- the outer border:
		procedure parse_drawing_frame is
			use et_frames;
			
			b : type_area; -- the bounding-box of the frame

			-- Get the size of the frame:
			size : constant type_frame_size := 
				element (current_active_module).frames.frame.size;

		begin
			b.width := type_distance_positive (size.x);
			b.height := type_distance_positive (size.y);

			-- CS: orientation (portrait/landscape) ?
			
			-- CS: set b.position with frame position
			-- currently it is default (0;0);
			-- In schematic probably useless because the lower
			-- left corner of the frame is always at (0;0) ?
			
			-- If this is the first primitive object,
			-- then use its bounding-box as seed to start from:
			if first_object then
				bbox_new := b;
				first_object := false;
			else
			-- Otherwise, merge the box b with the box being built:
				merge_areas (bbox_new, b);
			end if;
				
		end parse_drawing_frame;


		
-- 		-- This procedure is called each time an object of the database
-- 		-- is processed:
-- 		procedure query_object (oc : in pac_objects.cursor) is
-- 			-- This is the complex candidate object being handled:
-- 			object : type_complex_object renames element (oc);
-- 
-- 			
-- 			-- This procedure computes the bounding-box of a line:
-- 			procedure query_line (lc : in pac_lines.cursor) is
-- 				-- The candidate line being handled:
-- 				line : type_line renames element (lc);
-- 
-- 				-- Compute the preliminary bounding-box of the line:
-- 				b : type_area := get_bounding_box (line);
-- 			begin
-- 				-- Move the box by the position of the
-- 				-- complex object to get the final bounding-box
-- 				-- of the line candidate:
-- 				move_by (b.position, object.position);
-- 
-- 				-- If this is the first primitive object,
-- 				-- then use its bounding-box as seed to start from:
-- 				if first_object then
-- 					bbox_new := b;
-- 					first_object := false;
-- 				else
-- 				-- Otherwise, merge the box b with the box being built:
-- 					merge_areas (bbox_new, b);
-- 				end if;
-- 			end query_line;
-- 
-- 
-- 			-- This procedure computes the bounding-box of a circle:
-- 			procedure query_circle (cc : in pac_circles.cursor) is
-- 				-- The candidate circle being handled:
-- 				circle : type_circle renames element (cc);
-- 
-- 				-- Compute the preliminary bounding-box of the circle:
-- 				b : type_area := get_bounding_box (circle);
-- 			begin				
-- 				-- Move the box by the position of the
-- 				-- complex object to get the final bounding-box
-- 				-- of the circle candidate:
-- 				move_by (b.position, object.position);
-- 
-- 				-- If this is the first primitive object,
-- 				-- then use its bounding-box as seed to start from:
-- 				if first_object then
-- 					bbox_new := b;
-- 					first_object := false;
-- 				else
-- 				-- Otherwise, merge the box b with the box being built:
-- 					merge_areas (bbox_new, b);
-- 				end if;
-- 			end query_circle;
-- 
-- 			
-- 		begin
-- 			-- Iterate the lines, circles and other primitive
-- 			-- components of the current object:
-- 			object.lines.iterate (query_line'access);
-- 			object.circles.iterate (query_circle'access);
-- 			-- CS arcs
-- 		end query_object;


		-- This procedure updates the bounding-box and
		-- sets the bounding_box_changed flag
		-- in NON-TEST-MODE (which is default by argument test_only).
		-- In TEST-mode the bounding_box_changed flag is cleared:
		procedure update_global_bounding_box is begin
			if test_only then
				put_line ("TEST ONLY mode. Bounding-box not changed.");
				bounding_box_changed := false;
			else
				-- Update the global bounding-box:
				bounding_box := bbox_new;

				-- The new bounding-box differs from the old one.
				-- Set the global flag bounding_box_changed:
				bounding_box_changed := true;
			end if;
		end update_global_bounding_box;
		


		procedure add_margin is
			use et_frames;

			-- Get the margin between outer border of the frame
			-- and the edge of the paper:
			margin : constant type_border_width := 
				element (current_active_module).frames.frame.border_width;
			
			-- The offset due to the margin:
			margin_offset : type_vector_model;
		begin
			bbox_new.width  := bbox_new.width  + 2.0 * type_distance_positive (margin);
			bbox_new.height := bbox_new.height + 2.0 * type_distance_positive (margin);
			
			-- Since we regard the margin as inside the bounding-box,
			-- we must move the bounding-box position towards bottom-left
			-- by the inverted margin_offset:
			margin_offset := (
				x => type_distance_positive (margin),
				y => type_distance_positive (margin));
			
			move_by (bbox_new.position, invert (margin_offset));
		end add_margin;

		
	begin
		put_line ("compute_bounding_box (schematic)");

		-- The drawing frame is regarded as part of the model:
		parse_drawing_frame;
		
		-- The database that contains all objects of the model
		-- must be parsed. This is the call of an iteration through
		-- all objects of the database:
	-- objects_database_model.iterate (query_object'access);

		-- The temporary bounding-box bbox_new in its current
		-- state is the so called "inner bounding-box" (IB).

		-- Now, we expand the temporary bounding-box by the margin.
		-- The area around the drawing frame frame is regarded
		-- as part of the model and thus inside the bounding-box:
		add_margin;
		-- Now, bbox_new has become the "outer bounding-box" (OB).
		
		-- Compare the new bounding-box with the old 
		-- bounding-box to detect a change:
		if bbox_new /= bbox_old then

			-- Do the size check of the new bounding-box. If it is
			-- too large, then restore the old bounding-box:
			if bbox_new.width  >= bounding_box_width_max or
				bbox_new.height >= bounding_box_height_max then

				-- output limits and computed box dimensions:
				put_line ("WARNING: Bounding-box size limit exceeded !");
				put_line (" max. width : " 
					& to_string (bounding_box_width_max));
				
				put_line (" max. height: " 
					& to_string (bounding_box_height_max));
				
				put_line (" detected   : " 
					& to_string (bbox_new));

				-- Set the error flag:
				bounding_box_error := (
					size_exceeded => true,
					width  => bbox_new.width,
					height => bbox_new.height);

				
				if ignore_errors then
					put_line (" Errors ignored !");
					
					-- Override old global bounding-box with
					-- the faulty box bbox_new:
					update_global_bounding_box;
					
				else -- By default errors are NOT ignored.
					put_line (" Discarded. Global bounding-box NOT changed.");
					
					-- Clear the global flag bounding_box_changed
					-- because we discard the new bounding-box (due to 
					-- a size error) and
					-- leave the current global bounding-box untouched:
					bounding_box_changed := false;

				end if;

				
			else -- size ok, no errors
				-- Reset error flag:
				bounding_box_error := (others => <>);

				update_global_bounding_box;
			end if;
			
			
		else -- No change. 
			-- Clear the global flag bounding_box_changed:
			bounding_box_changed := false;

			-- Reset error flag:
			bounding_box_error := (others => <>);
		end if;

		
		if debug then
			put_line ("bounding-box: " & to_string (bounding_box));

			if bounding_box_changed then
				put_line (" has changed");
			end if;
		end if;
	end compute_bounding_box;


	

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
		put_line ("cb_window_key_pressed (schematic)"
			& " key " & gdk_key_type'image (event.keyval));

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


				-- Advance to next sheet:
				when GDK_KP_Add =>
					current_active_sheet := current_active_sheet + 1;
					update_sheet_number_display;

					
				-- Advance to previous sheet:
				when GDK_KP_Subtract =>
					if current_active_sheet > sheet_default then
						current_active_sheet := current_active_sheet - 1;
						update_sheet_number_display;
					end if;


				when GDK_F11 =>
					previous_module;

					
				when GDK_F12 =>
					next_module;
					

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
		put_line ("cb_draw (schematic) " & image (clock));

		-- Update the global context:
		context := context_in;

		
		-- Update the global visible_area:
		visible_area := get_visible_area (canvas);
		-- put_line (" visible " & to_string (visible_area));

		-- Set the background color:
		-- set_source_rgb (context, 0.0, 0.0, 0.0); -- black
		set_source_rgb (context, 1.0, 1.0, 1.0); -- white
		paint (context);

		-- The ends of all kinds of lines are round:
		set_line_cap (context, cairo_line_cap_round);

		if grid_enabled then
			draw_grid;		
		end if;
		
		draw_drawing_origin;
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





	
	
	-- This procedure resets a lot of stuff and should
	-- be called when the operator pressed the ESCAPE key.
	-- Here the commands to abort any pending 
	-- operations related to the canvas should be placed:
	procedure reset is
		use et_modes;
		use et_modes.schematic;
		use et_canvas_schematic_nets;
	begin
		-- Here the commands to abort any pending 
		-- operations related to the canvas should be placed:

		expect_entry := expect_entry_default;
		reset_selections;
		status_enter_verb;			

	end reset;
	

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
			use et_coordinates_2.pac_grid;
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
		put_line ("cb_canvas_key_pressed (schematic)"
			& " key " & gdk_key_type'image (event.keyval));

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
					current_active_sheet := current_active_sheet + 1;
					update_sheet_number_display;

				-- Advance to previous sheet:
				when GDK_KP_Subtract =>
					if current_active_sheet > sheet_default then
						current_active_sheet := current_active_sheet - 1;
						update_sheet_number_display;
					end if;

				when GDK_F11 =>
					previous_module;

				when GDK_F12 =>
					next_module;

					
					
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
		put_line ("cb_canvas_button_pressed (schematic)");

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
		put_line ("cb_canvas_button_released (schematic)");
		
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
		put_line ("cb_canvas_mouse_moved (schematic)");

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

	
	
-- 	procedure set_title_bar (
-- 		-- CS project name
-- 		module		: in pac_module_name.bounded_string)
-- 	is begin
-- 		window.set_title (title & to_string (module));
-- 	end set_title_bar;


	
	procedure update_sheet_number_display is begin
		gtk_entry (cbox_sheet.get_child).set_text (to_sheet (current_active_sheet));
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

	
-- 	procedure set_label_console is
-- 		text_before : constant string := label_console.get_text;
-- 	begin
-- 		label_console.set_text (text_before & label_console_text);
-- 	end set_label_console;

	

	

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
	
	
	procedure next_module is
		use pac_generic_modules;
		use et_canvas_board_2;
	begin
		-- Advance to next module:
		current_active_module := pac_generic_modules.next (current_active_module);

		-- If there is no next module, select first module:
		if current_active_module = pac_generic_modules.no_element then
			current_active_module := generic_modules.first;
		end if;

		-- CS: save sheet number, cursor, zoom, displayed objects ...
		
		-- Show the module name in the title bars of 
		-- both schematic and layout editor:
		set_title_bar (active_module);
		et_canvas_board_2.set_title_bar (active_module);
		
		-- CS Init defaults of property bars in schematic.
		
		-- Init defaults of property bars in board:
	-- CS et_canvas_board.init_property_bars;
		
		-- Redraw both schematic and board:
		-- CS redraw;
	end next_module;

	
	procedure previous_module is
		use pac_generic_modules;
		use et_canvas_board_2;
	begin
		-- Advance to previous module:
		current_active_module := pac_generic_modules.previous (current_active_module);

		-- If there is no previous module, select last module:
		if current_active_module = pac_generic_modules.no_element then
			current_active_module := generic_modules.last;
		end if;

		-- CS: save sheet number, cursor, zoom, displayed objects ...
		
		-- Show the module name in the title bars of 
		-- both schematic and layout editor:
		set_title_bar (active_module);
		et_canvas_board_2.set_title_bar (active_module);

		-- CS Init defaults of property bars in schematic.
		
		-- Init defaults of property bars in board:
		-- CS et_canvas_board_2.init_property_bars;

		
		-- Redraw both schematic and board:
		-- CS redraw;
	end previous_module;


	
	procedure connect_console is begin
		-- Connect to the on_activate signal of the 
		-- entry (which is a child of console):
		gtk_entry (console.get_child).on_activate 
			(execute_command'access); -- on hitting enter

	end connect_console;


	
	procedure execute_script (script : in pac_script_name.bounded_string) is
		use ada.directories;
		use et_scripting;
		use et_modes;
		use et_project;
		
		-- We compose a command that executes a script
		-- like "schematic motor_driver execute script my_script.scr:
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_SCHEMATIC)) & space &
			to_string (et_canvas_schematic_2.active_module) & space &
			"execute" & space & "script" & space &
			to_string (script); -- "my_script.scr"
		
		cmd : et_string_processing.type_fields_of_line;

		-- The command launches a script. Change into the project directory. 
		-- The current directory is the parent directory of the active project. 
		-- Example: The current directory is /home/user/my_projects . The directory
		--  of the current project is /home/user/my_projects/blood_sample_analyzer.
		--  Executing a script requires changing into the project directory blood_sample_analyzer.

		-- Backup the current directory (like /home/user/my_projects):
		cur_dir_bak : constant string := current_directory;
	begin
		cmd_entry_mode := VIA_SCRIPT;
		
		log (text => "executing command " & enclose_in_quotes (line_as_typed_by_operator), level => log_threshold);
		log_indentation_up;
		
		-- Store the command in the command history:
	-- CS console.prepend_text (line_as_typed_by_operator);

		cmd := read_line (
			line 			=> line_as_typed_by_operator,
			number			=> 1,  -- this is the one and only line
			comment_mark 	=> et_scripting.comment_mark,
			delimiter_wrap	=> true, -- strings are enclosed in quotations
			ifs 			=> space); -- fields are separated by space

		--log (text => "full command " & enclose_in_quotes (to_string (cmd)), level => log_threshold + 1);

		set_directory (to_string (current_active_project));
		
		-- execute the schematic command
		schematic_cmd (current_active_module, cmd, log_threshold);

		-- Return to previous directory (like  /home/user/my_projects):
		set_directory (cur_dir_bak);
		
		-- The majority of commands requires refreshing the schematic and board drawing.
		
		-- refresh schematic and board
	-- CS redraw;
		--redraw (canvas);
		--et_canvas_board.pac_canvas.redraw (et_canvas_board.pac_canvas.canvas);
		
		-- CS output error message in gui

		log_indentation_down;

	exception when event: others =>
		
		-- Return to previous directory (like  /home/user/my_projects):
		set_directory (cur_dir_bak);

		log_indentation_down;
	end execute_script;



	
	procedure execute_command (self : access gtk_entry_record'class) is 
		use ada.directories;	
		use et_scripting;
		use et_modes;
		use et_project;
		
		-- The operator enters a command like "rename device R1 R2".
		-- The operator is not required to type domain and module name.
		-- Since we are editing a schematic, the domain and module name itelf
		-- are known. By prepending domain and module name here the full 
		-- command after this declaration will be "schematic led_driver rename device R1 R2".
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_SCHEMATIC)) & space &
			to_string (active_module) & space &
			get_text (self);
		
		cmd : et_string_processing.type_fields_of_line;

		-- The command might launch a script. To prepare for this case we must change
		-- into the project directory. The current directory is the parent directory
		-- of the active project. 
		-- Example: The current directory is /home/user/my_projects . The directory
		--  of the current project is /home/user/my_projects/blood_sample_analyzer.
		--  Executing scripts requires changing into the project directory blood_sample_analyzer.

		-- Backup the current directory (like /home/user/my_projects):
		cur_dir_bak : constant string := current_directory;
	begin
		cmd_entry_mode := SINGLE_CMD;
				
		log (text => "executing command " & enclose_in_quotes (get_text (self)), level => log_threshold);
		log_indentation_up;
		
		-- Store the latest command in the command history:
		console.prepend_text (get_text (self));

		cmd := read_line (
			line 			=> line_as_typed_by_operator,
			number			=> 1,  -- this is the one and only line
			comment_mark 	=> et_scripting.comment_mark,
			delimiter_wrap	=> true, -- strings are enclosed in quotations
			ifs 			=> space); -- fields are separated by space

		--log (text => "full command " & enclose_in_quotes (to_string (cmd)), level => log_threshold + 1);

		log (text => "changing to directory " &
				enclose_in_quotes (to_string (current_active_project)) & " ...",
			level => log_threshold + 1);
		
		set_directory (to_string (current_active_project));
		
		-- execute the schematic command
		schematic_cmd (current_active_module, cmd, log_threshold);

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

	



	
	function active_module return pac_module_name.bounded_string is
		use pac_module_name;
		use et_project.modules.pac_generic_modules;
	begin
		return key (current_active_module); -- motor_driver (without extension)
	end active_module;


	

	
	procedure set_module (
		module	: in pac_module_name.bounded_string)  -- motor_driver
	is
		use et_project.modules;
		use et_project.modules.pac_generic_modules;
		cursor : et_project.modules.pac_generic_modules.cursor := find (generic_modules, module);
	begin
		-- If module already loaded in collection of generic modules, set the current_active_module:
		if cursor /= pac_generic_modules.no_element then 
			current_active_module := cursor;
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

	
	procedure init_drawing (
		module	: in et_project.modules.pac_generic_modules.cursor; -- the module to be drawn
		sheet	: in et_coordinates_2.type_sheet := et_coordinates_2.type_sheet'first) -- the sheet to be drawn
	is begin
		-- set the active module:
		current_active_module := module;
		
		-- set active sheet:
		current_active_sheet := sheet;
	end init_drawing;


	
-- 	procedure set_grid_x (self : access gtk_entry_record'class) is
-- 		use et_schematic_ops;
-- 
-- 		-- get the current grid		
-- 		grid : type_grid := element (current_active_module).grid;
-- 	begin
-- 		-- Assign grid to x AND y axis so that the operator is not requested
-- 		-- to manually assign y.
-- 		grid.x := to_distance (get_text (self));
-- 		grid.y := to_distance (get_text (self));
-- 
-- 		-- Show the grid:
-- 		gtk_entry (grid_x.get_child).set_text (trim (to_string (grid.x), left));
-- 		gtk_entry (grid_y.get_child).set_text (trim (to_string (grid.y), left));
-- 		
-- 		-- Finally set the grid in the module database:
-- 		set_grid (current_active_module, grid, log_threshold + 1);
-- 		redraw (canvas);
-- 	end set_grid_x;
-- 
-- 	
-- 	procedure set_grid_y (self : access gtk_entry_record'class) is
-- 		use et_schematic_ops;
-- 
-- 		-- get the current grid	
-- 		grid : type_grid := element (current_active_module).grid;
-- 	begin
-- 		-- Assign grid to y axis:
-- 		grid.y := to_distance (get_text (self));
-- 
-- 		-- Show the grid:
-- 		gtk_entry (grid_y.get_child).set_text (trim (to_string (grid.y), left));
-- 		
-- 		-- Finally set the grid in the module database:
-- 		set_grid (current_active_module, grid, log_threshold + 1);
-- 		redraw (canvas);
-- 	end set_grid_y;

	
	procedure reset_grid_and_cursor	is 
	begin
		-- CS et_coordinates_2.pac_grid.reset_grid_density (grid);
		-- CS update_grid_display;
		
		move_cursor (snap_to_grid (get_cursor_position));
		update_cursor_coordinates;
	end reset_grid_and_cursor;

	
-- 	procedure set_grid (
-- 		self	: not null access type_view;
-- 		density	: in type_grid_density)
-- 	is begin
-- 		grid_density := density;
-- 		cursor_main.position := snap_to_grid (cursor_main.position);
-- 		self.update_coordinates_display;
-- 	end set_grid;

	-- 	
-- 	function get_grid (
-- 		self : not null access type_view)
-- 		return type_grid
-- 	is
-- 		-- Get the default grid as defined in the module database:
-- 		g : type_grid := element (current_active_module).grid;
-- 	begin
-- 		-- Scale the grid according to current grid level:
-- 		case grid_density is
-- 			when COARSE => scale_grid (g, grid_density_multiplier_coarse);
-- 			when NORMAL => scale_grid (g, grid_density_multiplier_normal);
-- 			when FINE	=> scale_grid (g, grid_density_multiplier_fine);
-- 		end case;
-- 				
-- 		return g;
-- 	end get_grid;
-- 
-- 	
-- 	function get_frame (
-- 		self : not null access type_view)
-- 		return et_frames.type_frame 
-- 	is
-- 		use et_project.modules.pac_generic_modules;
-- 	begin
-- 		return element (current_active_module).frames.frame;
-- 	end get_frame;
-- 
-- 	
-- 	function get_frame_height (
-- 		self : not null access type_view)
-- 		return type_float_positive 
-- 	is 
-- 		use et_project.modules.pac_generic_modules;
-- 	begin
-- 		return type_float_positive (
-- 			element (current_active_module).frames.frame.size.y);
-- 	end get_frame_height;
-- 
-- 	
-- 	function frame_width (
-- 		self : not null access type_view)
-- 		return type_float_positive 
-- 	is 
-- 		use et_project.modules.pac_generic_modules;
-- 	begin
-- 		return type_float_positive (
-- 			element (current_active_module).frames.frame.size.x);
-- 	end frame_width;
-- 
-- 	
-- 	function title_block_position (
-- 		self : not null access type_view)
-- 		return et_frames.type_position 
-- 	is begin
-- 		return self.get_frame.title_block_schematic.position;
-- 	end title_block_position;
-- 	
-- 	
-- 	function get_verb (
-- 		self	: not null access type_view)
-- 		return string 
-- 	is begin
-- 		return to_string (verb);
-- 	end get_verb;
-- 
-- 	
-- 	function get_noun (
-- 		self	: not null access type_view)
-- 		return string is
-- 	begin
-- 		return to_string (noun);
-- 	end get_noun;
-- 
-- 
-- 	
	procedure reset_selections is 
		use et_canvas_schematic_nets;
		use et_canvas_schematic_units;
	begin
		-- Verb and noun remain as they are
		-- so that the mode is unchanged.
		
		reset_request_clarification;
		
		reset_preliminary_segment; -- after move/drag/draw of a net segment
		reset_segments_being_dragged; -- after dragging a unit
		reset_unit_move; -- after moving/dragging a unit
		reset_unit_add; -- after adding a device
		
		reset_label; -- after placing a label
		
		reset_placeholder; -- after moving a placeholder

		reset_single_cmd_status;
		
		reset_activate_counter;

		reset_zoom_area; -- abort zoom-to-area operation
	end reset_selections;

	
	procedure clear_proposed_objects is 
		use et_canvas_schematic_nets;
		use et_canvas_schematic_units;
	begin
		clear_proposed_units;
		clear_proposed_segments;
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
-- 	procedure save_module is
-- 		use ada.directories;
-- 		use et_project;
-- 
-- 		-- Backup the current directory (like /home/user/et/blood_sample_analyzer):
-- 		cur_dir_bak : constant string := current_directory;
-- 	begin
-- 		-- NOTE: We are not in the project directory yet but
-- 		-- in its parent directory.
-- 		
-- 		-- Change into the directory of the current project:
-- 		set_directory (to_string (current_active_project));
-- 		
-- 		-- Save the module with its own name:
-- 		save_module (
-- 			module_cursor	=> current_active_module,
-- 			log_threshold	=> log_threshold + 1);
-- 
-- 		-- Return to previous directory:
-- 		set_directory (cur_dir_bak);
-- 		
-- 		-- Show a brief message in the schematic status bar:
-- 		set_status (status_text_module_saved);
-- 
-- 		-- Show a brief message in the board status bar:
-- 		et_canvas_board.pac_canvas.set_status (status_text_module_saved);
-- 	end save_module;
-- 
-- 	
-- 	procedure save_drawing (
-- 		self : not null access type_view)
-- 	is begin
-- 		save_module;
-- 	end save_drawing;

	
end et_canvas_schematic_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
