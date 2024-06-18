------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS FOR SCHEMATIC                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
-- with et_scripting;
-- with et_modes;
-- with et_project;

-- with et_canvas_board;
-- with et_display.schematic;			use et_display.schematic;
-- with et_colors;						use et_colors;
-- with et_colors.schematic;			use et_colors.schematic;
-- with et_modes.schematic;			use et_modes.schematic;
-- 
-- with et_net_names;					use et_net_names;
-- with et_net_labels;					use et_net_labels;
-- 
-- with et_canvas_schematic_nets;			use et_canvas_schematic_nets;
-- with et_canvas_schematic_units;			use et_canvas_schematic_units;
-- 
-- with et_device_placeholders;			use et_device_placeholders;
-- with et_device_placeholders.symbols;	use et_device_placeholders.symbols;
-- 
-- with et_undo_redo;


package body et_canvas_schematic_2 is

	procedure dummy is begin null; end;

	procedure compute_bounding_box (
		abort_on_first_error	: in boolean := false;
		ignore_errors			: in boolean := false;
		test_only				: in boolean := false)		
	is
		-- use pac_lines;
		-- use pac_circles;
		-- use pac_objects;

		-- debug : boolean := false;
		debug : boolean := true;
		

		-- In order to detect whether the bounding-box has
		-- changed we take a copy of the current bounding-box:
		bbox_old : type_area := bounding_box;

		-- This is the temporary bounding-box we are going to build
		-- in the course of this procedure:
		bbox_new : type_area;
		
		-- The first primitie object encountered will be the
		-- seed for bbox_new. All other objects cause 
		-- this bbox_new to expand. After the first object,
		-- this flag is cleared:
		first_object : boolean := true;


		-- This procedure iterates through all primitive objects
		-- of the drawing frame and adds them to the temporary
		-- bounding-box bbox_new:
-- 		procedure parse_drawing_frame is
-- 			use demo_frame;
-- 
-- 			procedure query_line (l : in pac_lines.cursor) is
-- 				-- The candidate line being handled:
-- 				line : type_line renames element (l);
-- 
-- 				-- Compute the preliminary bounding-box of the line:
-- 				b : type_area := get_bounding_box (line);
-- 			begin
-- 				-- Move the box by the position of the
-- 				-- drawing frame to get the final bounding-box
-- 				-- of the line candidate:
-- 				move_by (b.position, drawing_frame.position);
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
-- 		begin
-- 			-- CS: This simple solution iterates through all lines
-- 			-- incl. the title block. But since the title block
-- 			-- is always inside the drawing frame, the elements
-- 			-- of the title block can be omitted.
-- 			drawing_frame.lines.iterate (query_line'access);
-- 			
-- 			-- CS texts
-- 		end parse_drawing_frame;


		
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
-- 
-- 
-- 		-- This procedure updates the bounding-box and
-- 		-- sets the bounding_box_changed flag
-- 		-- in NON-TEST-MODE (which is default by argument test_only).
-- 		-- In TEST-mode the bounding_box_changed flag is cleared:
-- 		procedure update_global_bounding_box is begin
-- 			if test_only then
-- 				put_line ("TEST ONLY mode. Bounding-box not changed.");
-- 				bounding_box_changed := false;
-- 			else
-- 				-- Update the global bounding-box:
-- 				bounding_box := bbox_new;
-- 
-- 				-- The new bounding-box differs from the old one.
-- 				-- Set the global flag bounding_box_changed:
-- 				bounding_box_changed := true;
-- 			end if;
-- 		end update_global_bounding_box;
-- 		
-- 
-- 
-- 		procedure add_margin is
-- 			use demo_frame;
-- 			
-- 			-- The offset due to the margin:
-- 			margin_offset : type_vector_model;
-- 		begin
-- 			bbox_new.width  := bbox_new.width  + 2.0 * margin;
-- 			bbox_new.height := bbox_new.height + 2.0 * margin;
-- 			
-- 			-- Since we regard the margin as inside the bounding-box,
-- 			-- we must move the bounding-box position towards bottom-left
-- 			-- by the inverted margin_offset:
-- 			margin_offset := (x	=> margin, y => margin);
-- 			move_by (bbox_new.position, invert (margin_offset));
-- 		end add_margin;
-- 
		
	begin
		put_line ("compute_bounding_box (schematic)");

		-- The drawing frame is regarded as part of the model.
		-- Iterate through all primitive objects of the 
		-- drawing frame:
	-- parse_drawing_frame;
		
		-- The database that contains all objects of the model
		-- must be parsed. This is the call of an iteration through
		-- all objects of the database:
	-- objects_database_model.iterate (query_object'access);

		-- The temporary bounding-box bbox_new in its current
		-- state is the so called "inner bounding-box" (IB).

		-- Now, we expand the temporary bounding-box by the margin.
		-- The area around the drawing frame frame is regarded
		-- as part of the model and thus inside the bounding-box:
	-- add_margin;
		-- Now, bbox_new has become the "outer bounding-box" (OB).
		
		-- Compare the new bounding-box with the old 
		-- bounding-box to detect a change:
		if bbox_new /= bbox_old then
			null;
-- 
-- 			-- Do the size check of the new bounding-box. If it is
-- 			-- too large, then restore the old bounding-box:
-- 			if bbox_new.width  >= bounding_box_width_max or
-- 				bbox_new.height >= bounding_box_height_max then
-- 
-- 				-- output limits and computed box dimensions:
-- 				put_line ("WARNING: Bounding-box size limit exceeded !");
-- 				put_line (" max. width : " 
-- 					& to_string (bounding_box_width_max));
-- 				
-- 				put_line (" max. height: " 
-- 					& to_string (bounding_box_height_max));
-- 				
-- 				put_line (" detected   : " 
-- 					& to_string (bbox_new));
-- 
-- 				-- Set the error flag:
-- 				bounding_box_error := (
-- 					size_exceeded => true,
-- 					width  => bbox_new.width,
-- 					height => bbox_new.height);
-- 
-- 				
-- 				if ignore_errors then
-- 					put_line (" Errors ignored !");
-- 					
-- 					-- Override old global bounding-box with
-- 					-- the faulty box bbox_new:
-- 					update_global_bounding_box;
-- 					
-- 				else -- By default errors are NOT ignored.
-- 					put_line (" Discarded. Global bounding-box NOT changed.");
-- 					
-- 					-- Clear the global flag bounding_box_changed
-- 					-- because we discard the new bounding-box (due to 
-- 					-- a size error) and
-- 					-- leave the current global bounding-box untouched:
-- 					bounding_box_changed := false;
-- 
-- 				end if;
-- 
-- 				
-- 			else -- size ok, no errors
-- 				-- Reset error flag:
-- 				bounding_box_error := (others => <>);
-- 
-- 				update_global_bounding_box;
-- 			end if;
-- 			
-- 			
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
		refresh (canvas);
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



	procedure set_up_command_buttons is
	begin
		put_line ("set_up_command_buttons (schematic)");

		-- Connect button signals with subprograms:
		
		button_zoom_fit.on_clicked (cb_zoom_to_fit'access);
		-- button_zoom_fit.on_clicked (access_cb_zoom_to_fit);		
		
	end set_up_command_buttons;

	
	

	function cb_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean
	is
		event_handled : boolean := false;

		use gdk.types;		
		use gdk.types.keysyms;
		
		key_ctrl	: gdk_modifier_type := event.state and control_mask;
		key_shift	: gdk_modifier_type := event.state and shift_mask;
		key			: gdk_key_type := event.keyval;

	begin
		-- Output the the gdk_key_type (which is
		-- just a number (see gdk.types und gdk.types.keysyms)):
		put_line ("cb_window_key_pressed "
			& " key " & gdk_key_type'image (event.keyval));

		if key_ctrl = control_mask then 
			case key is

				when others => null;
			end case;

		else
			case key is
				when GDK_ESCAPE =>
					-- Here the commands to abort any pending 
					-- operations should be placed:
					
					-- Abort the zoom-to-area operation:
					reset_zoom_area;

					-- Do not pass this event further
					-- to widgets down the chain.
					-- Prosssing the event stops here.
					event_handled := true;
					

				when GDK_F5 =>
					zoom_to_fit_all;

					-- Do not pass this event further
					-- to widgets down the chain.
					-- Prosssing the event stops here.
					event_handled := true;

					
				when others => null;
			end case;
		end if;
		
		return event_handled;
	end cb_window_key_pressed;




	procedure set_up_main_window is begin
		log (text => "set_up_main_window (schematic)", level => log_threshold);

		main_window.on_key_press_event (cb_window_key_pressed'access);

		set_up_command_buttons;
	end set_up_main_window;

	

	function cb_draw (
		canvas		: access gtk_widget_record'class;
		context_in	: in cairo.cairo_context)
		return boolean
	is
		use cairo;
		event_handled : boolean := true;
	
	begin
		-- new_line;
		-- put_line ("cb_draw " & image (clock));

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

		--draw_grid;		
		--draw_origin;
		draw_cursor;
		draw_zoom_area;
		--draw_drawing_frame;		
		--draw_objects;		
		
		return event_handled;
	end cb_draw;

	
	procedure set_up_canvas is begin
		put_line ("set_up_canvas (schematic)");

		-- Connect signals:

		canvas.on_draw (cb_draw'access);
		-- NOTE: No context is declared here, because the canvas widget
		-- passes its own context to the callback procedure cb_draw.

	end set_up_canvas;


	
-- 	procedure set_title_bar (
-- 		-- CS project name
-- 		module		: in pac_module_name.bounded_string)
-- 	is begin
-- 		window.set_title (title & to_string (module));
-- 	end set_title_bar;
-- 
-- 
-- 	
-- 	procedure update_sheet_number_display is begin
-- 		gtk_entry (cbox_sheet.get_child).set_text (to_sheet (current_active_sheet));
-- 	end update_sheet_number_display;
-- 
-- 	
-- 	procedure build_sheet_number_display is
-- 		spacing : gint;
-- 	begin
-- 		spacing := 10;
-- 
-- 		gtk_new_vbox (box_sheet);
-- 		set_spacing (box_sheet, spacing);
-- 		pack_start (box_left, box_sheet, expand => false);
-- 		
-- 		gtk_new (label_sheet, "SHEET (KEYPAD +/-)");
-- 		pack_start (box_sheet, label_sheet, expand => false);
-- 		gtk_new_with_entry (cbox_sheet);
-- 		pack_start (box_sheet, cbox_sheet);
-- 	end build_sheet_number_display;
-- 
-- 	
-- 	procedure set_label_console is
-- 		text_before : constant string := label_console.get_text;
-- 	begin
-- 		label_console.set_text (text_before & label_console_text);
-- 	end set_label_console;
-- 
-- 	
-- 	procedure redraw_board is begin
-- 		et_canvas_board.redraw_board;
-- 	end redraw_board;
-- 
-- 	
-- 	procedure redraw_schematic is begin
-- 		redraw (canvas);
-- 	end redraw_schematic;
-- 
-- 	
-- 	procedure redraw is begin
-- 		redraw_schematic;
-- 		redraw_board;
-- 	end redraw;
-- 
-- 	
-- 	procedure next_module is
-- 		use pac_generic_modules;
-- 	begin
-- 		-- Advance to next module:
-- 		current_active_module := pac_generic_modules.next (current_active_module);
-- 
-- 		-- If there is no next module, select first module:
-- 		if current_active_module = pac_generic_modules.no_element then
-- 			current_active_module := generic_modules.first;
-- 		end if;
-- 
-- 		-- CS: save sheet number, cursor, zoom, displayed objects ...
-- 		
-- 		-- Show the module name in the title bars of 
-- 		-- both schematic and layout editor:
-- 		set_title_bar (active_module);
-- 		et_canvas_board.set_title_bar (active_module);
-- 		
-- 		-- CS Init defaults of property bars in schematic.
-- 		
-- 		-- Init defaults of property bars in board:
-- 		et_canvas_board.init_property_bars;
-- 		
-- 		-- Redraw both schematic and board:
-- 		redraw;
-- 	end next_module;
-- 
-- 	
-- 	procedure previous_module is
-- 		use pac_generic_modules;
-- 	begin
-- 		-- Advance to previous module:
-- 		current_active_module := pac_generic_modules.previous (current_active_module);
-- 
-- 		-- If there is no previous module, select last module:
-- 		if current_active_module = pac_generic_modules.no_element then
-- 			current_active_module := generic_modules.last;
-- 		end if;
-- 
-- 		-- CS: save sheet number, cursor, zoom, displayed objects ...
-- 		
-- 		-- Show the module name in the title bars of 
-- 		-- both schematic and layout editor:
-- 		set_title_bar (active_module);
-- 		et_canvas_board.set_title_bar (active_module);
-- 
-- 		-- CS Init defaults of property bars in schematic.
-- 		
-- 		-- Init defaults of property bars in board:
-- 		et_canvas_board.init_property_bars;
-- 
-- 		
-- 		-- Redraw both schematic and board:
-- 		redraw;
-- 	end previous_module;
-- 
-- 
-- 
-- 	
-- 	function model_to_drawing (
-- 		self		: not null access type_view;
-- 		model_point : in type_model_point)	
-- 		return type_vector_model 
-- 	is 
-- 		p : type_vector_model; -- to be returned
-- 	begin
-- 		set (point	=> p,
-- 			 axis	=> X, 
-- 			 value	=> clip_distance (type_distance (model_point.x - self.frame_bounding_box.x)));
-- 		
-- 		set (point	=> p,
-- 			 axis	=> Y,
-- 			 value	=> clip_distance (type_distance (
-- 						self.get_frame_height
-- 						- model_point.y
-- 						+ self.frame_bounding_box.y)));
-- 		 
-- 		return p;
-- 
-- 	end model_to_drawing;
-- 		
-- 
-- 	function drawing_to_model (
-- 		self			: not null access type_view;
-- 		drawing_point	: in type_vector_model)	
-- 		return type_model_point 
-- 	is 
-- 		p : type_model_point; -- to be returned
-- 	begin
-- 		p.x := type_float (get_x (drawing_point)) 
-- 			+ self.frame_bounding_box.x;
-- 		
-- 		p.y := self.get_frame_height 
-- 			- type_float (get_y (drawing_point))
-- 			+ self.frame_bounding_box.y;
-- 			
-- 		return p;
-- 	end drawing_to_model;
-- 
-- 
-- 
-- 	
-- 	procedure execute_script (script : in pac_script_name.bounded_string) is
-- 		use ada.directories;
-- 		use et_scripting;
-- 		use et_modes;
-- 		use et_project;
-- 		
-- 		-- We compose a command that executes a script
-- 		-- like "schematic motor_driver execute script my_script.scr:
-- 		line_as_typed_by_operator : constant string := 
-- 			to_lower (to_string (DOM_SCHEMATIC)) & space &
-- 			to_string (et_canvas_schematic.active_module) & space &
-- 			"execute" & space & "script" & space &
-- 			to_string (script); -- "my_script.scr"
-- 		
-- 		cmd : et_string_processing.type_fields_of_line;
-- 
-- 		-- The command launches a script. Change into the project directory. 
-- 		-- The current directory is the parent directory of the active project. 
-- 		-- Example: The current directory is /home/user/my_projects . The directory
-- 		--  of the current project is /home/user/my_projects/blood_sample_analyzer.
-- 		--  Executing a script requires changing into the project directory blood_sample_analyzer.
-- 
-- 		-- Backup the current directory (like /home/user/my_projects):
-- 		cur_dir_bak : constant string := current_directory;
-- 	begin
-- 		cmd_entry_mode := VIA_SCRIPT;
-- 		
-- 		log (text => "executing command " & enclose_in_quotes (line_as_typed_by_operator), level => log_threshold);
-- 		log_indentation_up;
-- 		
-- 		-- Store the command in the command history:
-- 		console.prepend_text (line_as_typed_by_operator);
-- 
-- 		cmd := read_line (
-- 			line 			=> line_as_typed_by_operator,
-- 			number			=> 1,  -- this is the one and only line
-- 			comment_mark 	=> et_scripting.comment_mark,
-- 			delimiter_wrap	=> true, -- strings are enclosed in quotations
-- 			ifs 			=> space); -- fields are separated by space
-- 
-- 		--log (text => "full command " & enclose_in_quotes (to_string (cmd)), level => log_threshold + 1);
-- 
-- 		set_directory (to_string (current_active_project));
-- 		
-- 		-- execute the schematic command
-- 		schematic_cmd (current_active_module, cmd, log_threshold);
-- 
-- 		-- Return to previous directory (like  /home/user/my_projects):
-- 		set_directory (cur_dir_bak);
-- 		
-- 		-- The majority of commands requires refreshing the schematic and board drawing.
-- 		
-- 		-- refresh schematic and board
-- 		redraw;
-- 		--redraw (canvas);
-- 		--et_canvas_board.pac_canvas.redraw (et_canvas_board.pac_canvas.canvas);
-- 		
-- 		-- CS output error message in gui
-- 
-- 		log_indentation_down;
-- 
-- 	exception when event: others =>
-- 		
-- 		-- Return to previous directory (like  /home/user/my_projects):
-- 		set_directory (cur_dir_bak);
-- 
-- 		log_indentation_down;
-- 	end execute_script;
-- 
-- 
-- 
-- 	
-- 	procedure execute_command (self : access gtk_entry_record'class) is 
-- 		use ada.directories;	
-- 		use et_scripting;
-- 		use et_modes;
-- 		use et_project;
-- 		
-- 		-- The operator enters a command like "rename device R1 R2".
-- 		-- The operator is not required to type domain and module name.
-- 		-- Since we are editing a schematic, the domain and module name itelf
-- 		-- are known. By prepending domain and module name here the full 
-- 		-- command after this declaration will be "schematic led_driver rename device R1 R2".
-- 		line_as_typed_by_operator : constant string := 
-- 			to_lower (to_string (DOM_SCHEMATIC)) & space &
-- 			to_string (active_module) & space &
-- 			get_text (self);
-- 		
-- 		cmd : et_string_processing.type_fields_of_line;
-- 
-- 		-- The command might launch a script. To prepare for this case we must change
-- 		-- into the project directory. The current directory is the parent directory
-- 		-- of the active project. 
-- 		-- Example: The current directory is /home/user/my_projects . The directory
-- 		--  of the current project is /home/user/my_projects/blood_sample_analyzer.
-- 		--  Executing scripts requires changing into the project directory blood_sample_analyzer.
-- 
-- 		-- Backup the current directory (like /home/user/my_projects):
-- 		cur_dir_bak : constant string := current_directory;
-- 	begin
-- 		cmd_entry_mode := SINGLE_CMD;
-- 				
-- 		log (text => "executing command " & enclose_in_quotes (get_text (self)), level => log_threshold);
-- 		log_indentation_up;
-- 		
-- 		-- Store the latest command in the command history:
-- 		console.prepend_text (get_text (self));
-- 
-- 		cmd := read_line (
-- 			line 			=> line_as_typed_by_operator,
-- 			number			=> 1,  -- this is the one and only line
-- 			comment_mark 	=> et_scripting.comment_mark,
-- 			delimiter_wrap	=> true, -- strings are enclosed in quotations
-- 			ifs 			=> space); -- fields are separated by space
-- 
-- 		--log (text => "full command " & enclose_in_quotes (to_string (cmd)), level => log_threshold + 1);
-- 
-- 		log (text => "changing to directory " &
-- 				enclose_in_quotes (to_string (current_active_project)) & " ...",
-- 			level => log_threshold + 1);
-- 		
-- 		set_directory (to_string (current_active_project));
-- 		
-- 		-- execute the schematic command
-- 		schematic_cmd (current_active_module, cmd, log_threshold);
-- 
-- 		-- Return to previous directory (like  /home/user/my_projects):
-- 		log (text => "returning to directory " & enclose_in_quotes (cur_dir_bak) & " ...",
-- 			level => log_threshold + 1);
-- 		
-- 		set_directory (cur_dir_bak);
-- 		
-- 		-- The majority of commands requires refreshing the schematic and board drawing.
-- 		
-- 		-- refresh schematic and board
-- 		redraw;
-- 		--redraw (canvas);
-- 		--et_canvas_board.pac_canvas.redraw (et_canvas_board.pac_canvas.canvas);
-- 		
-- 		-- CS output error message in gui
-- 
-- 		log_indentation_down;
-- 
-- 	exception when event: others =>
-- 		
-- 		-- Return to previous directory (like  /home/user/my_projects):
-- 		log (text => "returning to directory " & enclose_in_quotes (cur_dir_bak) & " ...",
-- 			level => log_threshold + 1);
-- 
-- 		set_directory (cur_dir_bak);
-- 
-- 		log_indentation_down;
-- 	end execute_command;
-- 
-- 	
-- 
-- 
-- 
-- 	
-- 	function active_module return pac_module_name.bounded_string is
-- 		use pac_module_name;
-- 		use et_project.modules.pac_generic_modules;
-- 	begin
-- 		return key (current_active_module); -- motor_driver (without extension)
-- 	end active_module;
-- 
-- 
-- 	
-- 	
-- 	function bounding_box (self : not null access type_view)
-- 		return type_bounding_box is
-- 	begin
-- 		return self.paper_bounding_box; -- CS should include all items of the current sheet.
-- 		-- means: also items outside the frame
-- 	end;
-- 
-- 
-- 	
-- 	procedure gtk_new (
-- 		self	: out type_view_ptr) is
-- 	begin
-- 		self := new type_view;
-- 		init (self);
-- 	end;
-- 
-- 
-- 
-- 	procedure draw_grid (
-- 		self    : not null access type_view;
-- 		area    : type_bounding_box) is separate;
-- 
-- 	
-- 	procedure draw_frame (
-- 		self    : not null access type_view)
-- 		is separate;
-- 
-- 	
-- 	procedure draw_tag_label (
-- 		self	: not null access type_view;
-- 		net		: in pac_net_name.bounded_string;
-- 		label	: in type_net_label) is separate;
-- 
-- 	
-- 	procedure draw_nets (
-- 		self    : not null access type_view)
-- 		is separate;
-- 
-- 	
-- 	procedure draw_texts (
-- 		self    : not null access type_view)
-- 		is separate;
-- 
-- 			
-- 	-- Draws all units:
-- 	procedure draw_units (
-- 		self	: not null access type_view) 
-- 		is separate;
-- 
-- 
-- 	
-- 	procedure draw_submodules (
-- 		self	: not null access type_view)
-- 		is separate;
-- 
-- 	
-- 	procedure draw_internal (
-- 		self	: not null access type_view;
-- 		area_in	: type_bounding_box) 
-- 	is
-- 		offset : type_offset;
-- 		
-- 		use et_display.schematic;
-- 	begin
-- 		-- put_line ("draw internal schematic " & image (clock));
-- 		
-- -- 		shift_area (self, area_shifted, cursor_main);
-- -- 		shift_area (self, offset, cursor_main);
-- 
-- 		frame_height := self.get_frame_height;
-- 
-- 		-- The given area must be shifted (left and up) by the position
-- 		-- of the drawing frame. This is required for all objects in the 
-- 		-- drawing frame.
-- 		
-- 		-- Set the global area:
-- 		area := area_in;
-- 
-- 		-- Calculate the new position of the global area:
-- 		offset := to_offset (
-- 			x => - self.frame_bounding_box.x,
-- 			y => - self.frame_bounding_box.y);
-- 
-- 		
-- 		
-- 		set_color_background (context.cr);
-- 		paint (context.cr);
-- 
-- 		if grid_enabled then
-- 			draw_grid (self, area_in);
-- 		end if;
-- 		
-- 		-- move area according to frame position:
-- 		move_by (area, offset);
-- 
-- 		
-- 		save (context.cr);
-- 			
-- 		-- Prepare the current transformation matrix (CTM) so that
-- 		-- all following drawing is relative to the upper left frame corner.
-- 		-- translate (
-- 		-- 	context.cr,
-- 		-- 	convert_x (self.frame_bounding_box.x),
-- 		-- 	convert_y (self.frame_bounding_box.y));
-- 
-- 		
-- 		draw_units (self);
-- 		
-- 		draw_frame (self);
-- 		
-- 		-- Draw nets if layer is enabled:
-- 		if nets_enabled then
-- 			draw_nets (self);
-- 		end if;
-- 
-- 		-- Draw texts if layer is enabled:
-- 		if texts_enabled then
-- 			draw_texts (self);
-- 		end if;
-- 		
-- 		draw_submodules (self);
-- 		
-- 		draw_cursor (self, cursor_main);
-- 		
-- 		restore (context.cr);
-- 		
-- 	end draw_internal;
-- 
-- 	
-- 	procedure set_module (
-- 		module	: in pac_module_name.bounded_string)  -- motor_driver
-- 	is
-- 		use et_project.modules;
-- 		use et_project.modules.pac_generic_modules;
-- 		cursor : et_project.modules.pac_generic_modules.cursor := find (generic_modules, module);
-- 	begin
-- 		-- If module already loaded in collection of generic modules, set the current_active_module:
-- 		if cursor /= pac_generic_modules.no_element then 
-- 			current_active_module := cursor;
-- 		else
-- 			-- If module not loaded yet, read it and store it in collection of generic modules:
-- 			read_module (
-- 				file_name		=> append_extension (to_string (module)),
-- 				log_threshold	=> log_threshold + 1);
-- 
-- 		end if;
-- 
-- 		-- CS exception handler could catch semantic_error_1 and show
-- 		-- a list of available modules (which are inside the project directory)
-- 		-- in the status bar.
-- 	end set_module;
-- 
-- 	
-- 	procedure init_drawing (
-- 		module	: in et_project.modules.pac_generic_modules.cursor; -- the module to be drawn
-- 		sheet	: in et_coordinates.type_sheet := et_coordinates.type_sheet'first) -- the sheet to be drawn
-- 	is begin
-- 		-- set the active module:
-- 		current_active_module := module;
-- 		
-- 		-- set active sheet:
-- 		current_active_sheet := sheet;
-- 	end init_drawing;
-- 
-- 
-- 	
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
-- 
-- 	
-- 	procedure reset_grid_and_cursor (
-- 		self : not null access type_view)
-- 	is begin
-- 		reset_grid_density;
-- 		cursor_main.position := snap_to_grid (cursor_main.position);
-- 		self.update_coordinates_display;
-- 	end reset_grid_and_cursor;
-- 
-- 	
-- 	procedure set_grid (
-- 		self	: not null access type_view;
-- 		density	: in type_grid_density)
-- 	is begin
-- 		grid_density := density;
-- 		cursor_main.position := snap_to_grid (cursor_main.position);
-- 		self.update_coordinates_display;
-- 	end set_grid;
-- 
-- 	
-- 	procedure move_cursor (
-- 		self		: not null access type_view;
-- 		coordinates	: in type_coordinates;
-- 		cursor		: in out type_cursor;
-- 		position	: in type_vector_model) 
-- 	is
-- 		use et_project.modules.pac_generic_modules;
-- 	begin
-- 		case coordinates is
-- 			when ABSOLUTE =>
-- 				--cursor.position := type_vector_model (round (position, element (current_active_module).grid));
-- 				cursor.position := type_vector_model (round (position, self.get_grid));
-- 				
-- 			when RELATIVE =>
-- 				--cursor.position := type_vector_model (round (cursor.position + position, element (current_active_module).grid));
-- 				cursor.position := type_vector_model (round (cursor.position + position, self.get_grid));
-- 		end case;
-- 
-- 		self.shift_area (cursor);		
-- 	end move_cursor;
-- 
-- 	
-- 	procedure move_cursor (
-- 		self		: not null access type_view;
-- 		direction	: in type_cursor_direction;
-- 		cursor		: in out type_cursor)
-- 	is
-- 		-- Get the currently active grid:
-- 		use et_project.modules.pac_generic_modules;
-- 		--grid : constant type_grid := element (current_active_module).grid;
-- 		grid : constant type_grid := self.get_grid;
-- 
-- 		-- Find the grid point nearest available to the current cursor position:
-- 		position_snapped : constant type_vector_model := type_vector_model (round (
-- 							point	=> cursor.position,
-- 							grid	=> grid));
-- 
-- 	begin
-- 		case direction is
-- 			when RIGHT =>
-- 				cursor.position := type_vector_model (move (position_snapped, 0.0, grid.x, clip => true));
-- 
-- 			when LEFT =>
-- 				cursor.position := type_vector_model (move (position_snapped, 180.0, grid.x, clip => true));
-- 
-- 			when UP =>
-- 				cursor.position := type_vector_model (move (position_snapped, 90.0, grid.y, clip => true));
-- 
-- 			when DOWN =>
-- 				cursor.position := type_vector_model (move (position_snapped, -90.0, grid.y, clip => true));
-- 		end case;
-- 		
-- 		self.shift_area (cursor);
-- 	end move_cursor;
-- 
-- 	
-- 	procedure draw_cursor (
-- 		self		: not null access type_view;
-- 		cursor		: in type_cursor)
-- 	is
-- 		lh : type_cursor_line; -- the horizontal line
-- 		lv : type_cursor_line; -- the vertical line
-- 
-- 		size : type_distance_positive;
-- 		width : type_view_coordinate;
-- 	begin
-- 		size := cursor_half_size / type_distance_positive (global_scale);
-- 		
-- 		-- set start and end point of horizontal line
-- 		lh.start_point := type_vector_model (set (
-- 			x	=> get_x (cursor.position) - size,
-- 			y	=> get_y (cursor.position)));
-- 
-- 		lh.end_point := type_vector_model (set (
-- 			x	=> get_x (cursor.position) + size,
-- 			y	=> get_y (cursor.position)));
-- 
-- 		-- set start and end point of vertical line
-- 		lv.start_point := type_vector_model (set (
-- 			x	=> get_x (cursor.position),
-- 			y	=> get_y (cursor.position) + size));
-- 
-- 		lv.end_point := type_vector_model (set (
-- 			x	=> get_x (cursor.position),
-- 			y	=> get_y (cursor.position) - size));
-- 
-- 
-- 		-- The line width is inversely proportional to the scale:
-- 		width := type_view_coordinate (cursor_line_width) / global_scale;
-- 		
-- 		set_line_width (context.cr, width);
-- 		
-- 		set_color_cursor (context.cr);
-- 
-- 		draw_line (
-- 			line		=> to_line_fine (lh),
-- 			width		=> type_distance_positive (width));
-- 
-- 		draw_line (
-- 			line		=> to_line_fine (lv),
-- 			width		=> type_distance_positive (width));
-- 		
-- 		cairo.stroke (context.cr);		
-- 
-- 		exception
-- 			when constraint_error => null;
-- 			--put_line ("Schematic: " & message_border_reached);
-- 			-- CS put in status bar ?
-- 				
-- 	end draw_cursor;
-- 
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
-- 	procedure reset_selections is begin
-- 		-- Verb and noun remain as they are
-- 		-- so that the mode is unchanged.
-- 		
-- 		reset_request_clarification;
-- 		
-- 		reset_preliminary_segment; -- after move/drag/draw of a net segment
-- 		reset_segments_being_dragged; -- after dragging a unit
-- 		reset_unit_move; -- after moving/dragging a unit
-- 		reset_unit_add; -- after adding a device
-- 		
-- 		reset_label; -- after placing a label
-- 		
-- 		reset_placeholder; -- after moving a placeholder
-- 
-- 		reset_single_cmd_status;
-- 		
-- 		reset_activate_counter;
-- 	end reset_selections;
-- 
-- 	
-- 	procedure clear_proposed_objects is begin
-- 		clear_proposed_units;
-- 		clear_proposed_segments;
-- 	end clear_proposed_objects;
-- 
-- 	
-- 	procedure key_pressed (
-- 		self		: not null access type_view;
-- 		key			: in gdk_key_type;
-- 		key_shift	: in gdk_modifier_type)
-- 	is separate;
-- 
-- 	
-- 	overriding procedure mouse_moved (
-- 		self	: not null access type_view;
-- 		point	: in type_vector_model) 
-- 	is separate;
-- 
-- 	
-- 	overriding procedure button_pressed (
-- 		self	: not null access type_view;
-- 		button	: in type_mouse_button;
-- 		point	: in type_vector_model) 
-- 	is separate;
-- 
-- 	
-- 	
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
-- 
-- 
-- 	procedure undo (
-- 		self : not null access type_view) 
-- 	is 
-- 		use et_undo_redo;
-- 		use pac_undo_message;
-- 		message : pac_undo_message.bounded_string;
-- 	begin
-- 		-- put_line ("schematic undo");
-- 		undo (message, log_threshold + 1);
-- 
-- 		-- Show the undo-message in the status bar:
-- 		set_status (to_string (message));
-- 		
-- 		redraw;
-- 	end undo;
-- 
-- 	
-- 	procedure redo (
-- 		self : not null access type_view) 
-- 	is 
-- 		use et_undo_redo;
-- 		use pac_redo_message;
-- 		message : pac_redo_message.bounded_string;
-- 	begin
-- 		-- put_line ("schematic redo");
-- 		redo (message, log_threshold + 1);
-- 
-- 		-- Show the redo-message in the status bar:
-- 		set_status (to_string (message));
-- 
-- 		redraw;
-- 	end redo;
-- 
	
end et_canvas_schematic_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
