------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
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
with ada.containers;

with ada.calendar;					use ada.calendar;
with ada.calendar.formatting;		use ada.calendar.formatting;

with et_system_info;
with et_domains;
with et_primitive_objects;				use et_primitive_objects;
with et_scripting;
with et_modes;
with et_modes.board;				
with et_modes.schematic;
with et_module_names;				use et_module_names;

with et_canvas_schematic_2;
with et_canvas_tool;
with et_display.board;
with et_colors.board;
with et_board_ops.grid;
with et_board_ops.assy_doc;
with et_board_ops.silkscreen;
with et_board_ops.stopmask;
with et_board_ops.stencil;
with et_board_ops.keepout;
with et_board_ops.ratsnest;
with et_board_ops.conductors;
with et_board_ops.vias;
with et_board_ops.board_contour;
with et_pcb;

with et_text;
with et_canvas_board_lines;
with et_canvas_board_outline;
with et_canvas_board_texts;
with et_canvas_board_vias;
with et_canvas_board_devices;
with et_canvas_board_assy_doc;
with et_canvas_board_silkscreen;
with et_canvas_board_stopmask;
with et_canvas_board_stencil;
with et_canvas_board_keepout;
with et_canvas_board_tracks;
with et_canvas_board_conductors;

with et_canvas_board_preliminary_object;
with et_cmd_sts;						use et_cmd_sts;
with et_script_processor;

with et_undo_redo;

with et_project_name;
with et_ripup;


package body et_canvas_board_2 is


	procedure set_title_bar (
		-- CS project name
		module		: in pac_generic_modules.cursor)
	is 
		use et_system_info;
	begin
		main_window.set_title (
			system_name 
			& " - BOARD - " 
			-- CS project name					  
			& to_string (key (module)));
	end set_title_bar;

	

	procedure update_mode_display is 
		use et_modes.board;
		
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
		use et_modes.board;
		
		-- debug : boolean := true;
		debug : boolean := false;
	begin
		put_line ("cb_zoom_area (board)");

		reset_verb_and_noun;
		update_mode_display;
		
		zoom_area.active := true;
	end cb_zoom_area;

	

	procedure set_up_command_buttons is
	begin
		put_line ("set_up_command_buttons (board)");

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
		
		-- put_line ("cb_window_key_pressed (board)"
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
					et_canvas_schematic_2.switch_module (et_canvas_schematic_2.PREVIOUS);
					event_handled := true;
					
				when GDK_F12 =>
					et_canvas_schematic_2.switch_module (et_canvas_schematic_2.NEXT);
					event_handled := true;


					
				-- Other keys are propagated to the canvas:
				when others =>
					event_handled := false;

			end case;
		end if;
		
		return event_handled;
	end cb_window_key_pressed;




	procedure set_up_main_window is begin
		log (text => "set_up_main_window (board)", level => log_threshold);

		main_window.on_key_press_event (cb_window_key_pressed'access);

		set_up_command_buttons;
	end set_up_main_window;





	procedure draw_text_being_placed (
		face		: in type_face;
		category	: in type_layer_category)
	is 
		use et_canvas_board_preliminary_object;
		use et_canvas_board_texts;
		use et_modes.board;

		point : type_vector_model;
	begin
		-- put_line ("draw_text_being_placed");
		
		if verb = VERB_PLACE and noun = NOUN_TEXT and edit_process_running then

			if object_layer_category = category and object_face = face then

				-- Set the point where the text is to be drawn
				-- while the operator is moving the tool:
				point := get_primary_tool_position;

				preliminary_text.text.position := 
					type_position (to_position (point, zero_rotation));

				-- Draw the text:
				pac_draw_text.draw_vector_text (preliminary_text.text);
			end if;
		end if;
	end draw_text_being_placed;




	procedure draw_path (
		cat : in type_layer_category) 
	is
		use et_canvas_board_lines;
		use et_colors.board;
		use pac_path_and_bend;
		use et_modes.board;
		use et_canvas_tool;
		use et_canvas_board_preliminary_object;
		
		-- PL : type_preliminary_object renames preliminary_object;	


		-- Computes the path from given start to given end point.
		-- Takes the bend style given in preliminary_object into account.
		-- Draws the path.
		procedure compute_and_draw (
			A, B : in type_vector_model) 
		is
			use et_colors;
			
			line : type_line;

			-- Do the actual path calculation.
			path : constant type_path := to_path (A, B, live_path.bend_style);

			-- Draws the line:
			procedure draw is begin
				case cat is
					when LAYER_CAT_ROUTE_RESTRICT =>
						-- Lines in route restrict have zero width:
						draw_line (line => line, width => zero, do_stroke => true);

					when others =>
						draw_line (line => line, width => object_linewidth, do_stroke => true);
				end case;
			end draw;
			
			
		begin
			-- The calculated path may require a bend point.
			-- Set/clear the "bended" flag of the line being drawn.
			live_path.bended := path.bended;

			-- set linewidth:			
			set_linewidth (object_linewidth);

			-- If we are drawing a path in a conductor layer then
			-- the color must be set according to the signal layer:
			if object_layer_category = LAYER_CAT_CONDUCTOR then
				set_color_conductor (object_signal_layer, NORMAL);
			end if;

			
			-- If the path does not require a bend point, draw a single line
			-- from start to end point:
			if path.bended = NO then
				
				set_A (line, path.A);
				set_B (line, path.B);

				draw;

			-- If the path DOES require a bend point, then draw first a line
			-- from start point to bend point. Then draw a second line from
			-- bend point end point:
			else
				live_path.bend_point := path.bend_point;

				set_A (line, path.A);
				set_B (line, path.bend_point);
				
				draw;

				set_A (line, path.bend_point);
				set_B (line, path.B);
				
				draw;
				
			end if;
		end compute_and_draw;

		
	begin -- draw_path
		-- put_line ("draw_path");		
		
		if verb = VERB_DRAW and noun = NOUN_LINE and edit_process_running
		and object_layer_category = cat then
			
			compute_and_draw (
				A	=> live_path.A, -- start of path
				B	=> get_object_tool_position);	-- end of route
					
			
		end if;
	end draw_path;
	





	procedure draw_live_zone (
		cat : in type_layer_category) 
	is
		use pac_path_and_bend;
		use et_modes.board;
		use et_canvas_tool;
		use et_canvas_board_preliminary_object;


		-- Computes the path from given start to given end point.
		-- Takes the bend style given in preliminary_zone into account.
		-- Draws the path.
		procedure compute_and_draw (
			A, B : in type_vector_model) 
		is
			-- use et_colors;
			
			line : type_line;

			-- Do the actual path calculation.
			path : constant type_path := to_path (A, B, live_path.bend_style);

			-- Draws the line:
			procedure draw is begin
				-- Lines in of contours have zero width:
				draw_line (line => line, width => zero, do_stroke => true);
			end draw;
			
			
		begin
			-- The calculated path may require a bend point.
			-- Set/clear the "bended" flag of the line being drawn.
			live_path.bended := path.bended;

			
			-- If the path does not require a bend point, draw a single line
			-- from start to end point:
			if path.bended = NO then
				
				set_A (line, path.A);
				set_B (line, path.B);

				draw;

			-- If the path DOES require a bend point, then draw first a line
			-- from start point to bend point. Then draw a second line from
			-- bend point end point:
			else
				live_path.bend_point := path.bend_point;

				set_A (line, path.A);
				set_B (line, path.bend_point);
				
				draw;

				set_A (line, path.bend_point);
				set_B (line, path.B);
				
				draw;
				
			end if;
		end compute_and_draw;

		
	begin
		-- put_line ("draw_live_zone");		
		
		if verb = VERB_DRAW and edit_process_running
		and object_layer_category = cat 
			
		-- and (noun = NOUN_ZONE or noun = NOUN_OUTLINE) 
		
		then
			compute_and_draw (
				A	=> live_path.A, -- start of path
				B	=> get_object_tool_position);	-- end of route

		end if;
	end draw_live_zone;




	
	

	procedure draw_drawing_frame is separate;
	procedure draw_packages is separate;	
	procedure draw_conductors is separate;
	procedure draw_route_restrict is separate;
	procedure draw_via_restrict is separate;
	
	procedure draw_outline is separate;

	procedure draw_silkscreen (
		face	: in type_face) is separate;

	procedure draw_assy (
		face	: in type_face) is separate;

	procedure draw_keepout (
		face	: in type_face) is separate;

	procedure draw_stop (
		face	: in type_face) is separate;

	procedure draw_stencil (
		face	: in type_face) is separate;
	
	
	procedure draw_board is 
		use et_display.board;
		

		procedure draw_conductor_layers is begin
			draw_route_restrict;
			draw_via_restrict;
			draw_conductors;
		end draw_conductor_layers;
		
		
		procedure draw_silkscreen is begin
			if silkscreen_enabled (BOTTOM) then
				draw_silkscreen (BOTTOM);
			end if;
   
			if silkscreen_enabled (TOP) then
				draw_silkscreen (TOP);
			end if;
		end draw_silkscreen;

		
		procedure draw_assy_doc is begin
			if assy_doc_enabled (BOTTOM) then
				draw_assy (BOTTOM);
			end if;
   
			if assy_doc_enabled (TOP) then
				draw_assy (TOP);
			end if;
		end draw_assy_doc;

		
		procedure draw_keepout is begin
			if keepout_enabled (BOTTOM) then
				draw_keepout (BOTTOM);
			end if;
   
			if keepout_enabled (TOP) then
				draw_keepout (TOP);
			end if;
		end draw_keepout;

		
		procedure draw_stop_mask is begin
			if stop_mask_enabled (BOTTOM) then
				draw_stop (BOTTOM);
			end if;
   
			if stop_mask_enabled (TOP) then
				draw_stop (TOP);
			end if;
		end draw_stop_mask;

		
		procedure draw_stencil is begin
			if stencil_enabled (BOTTOM) then
				draw_stencil (BOTTOM);
			end if;
   
			if stencil_enabled (TOP) then
				draw_stencil (TOP);
			end if;
		end draw_stencil;

		
		procedure draw_board_contour is begin
			if board_contour_enabled then		
				draw_outline;
			end if;
		end draw_board_contour;
		

	begin
		draw_conductor_layers;
		draw_packages;
		draw_silkscreen;
		draw_assy_doc;
		draw_keepout;
		draw_stop_mask;
		draw_stencil;
		draw_board_contour;
		
		-- CS draw_submodules			
	end draw_board;
	

	

	function cb_draw (
		canvas		: access gtk_widget_record'class;
		context_in	: in cairo.cairo_context)
		return boolean
	is
		use cairo;
		use et_display.board;
		
		event_handled : boolean := true;

		use et_text;
	begin
		-- new_line;
		-- put_line ("cb_draw (board) " & image (clock));

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
		draw_drawing_frame;		
		draw_cursor;
		draw_zoom_area;
	
		draw_board;
		
		return event_handled;
	end cb_draw;




	
-- UNDO / REDO:	

	procedure undo is 
		use et_undo_redo;
		use pac_undo_message;
		message : pac_undo_message.bounded_string;
	begin
		-- put_line ("board undo");
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
		-- put_line ("board redo");
		redo (message, log_threshold + 1);

		-- Show the redo-message in the status bar:
		set_status (to_string (message));
		
		redraw;
	end redo;




	

-- RESET:
	
	procedure reset is 
		use et_modes;
		use et_modes.board;

		-- use et_canvas_board_lines;
		-- use et_canvas_board_zone;
		use et_canvas_board_texts;
		use et_canvas_board_devices;
		use et_canvas_board_vias;
		use et_canvas_board_tracks;

		use et_ripup;


		-- Do a level 1 reset. This is a partly reset:
		procedure level_1 is begin
			log (text => "level 1", level => log_threshold + 1);
			
			reset_edit_process_running;
			reset_request_clarification;
			reset_finalizing_granted;

			status_clear;

			reset_preliminary_text; -- after placing a text
			
			et_board_ops.ratsnest.reset_proposed_airwires (active_module, log_threshold + 1);
			reset_ripup_mode;

			reset_preliminary_electrical_device; -- after moving, rotating, flipping a device
			reset_preliminary_non_electrical_device;

			et_board_ops.assy_doc.reset_proposed_objects (active_module, log_threshold + 1);
			et_board_ops.silkscreen.reset_proposed_objects (active_module, log_threshold + 1);
			et_board_ops.stopmask.reset_proposed_objects (active_module, log_threshold + 1);
			et_board_ops.stencil.reset_proposed_objects (active_module, log_threshold + 1);
			et_board_ops.keepout.reset_proposed_objects (active_module, log_threshold + 1);
			et_board_ops.board_contour.reset_proposed_objects (active_module, log_threshold + 1);
			et_board_ops.conductors.reset_proposed_objects (active_module, log_threshold + 1);
			et_board_ops.vias.reset_proposed_vias (active_module, log_threshold + 1);
		end level_1;


		-- Do a level 2 reset. This is a full reset:
		procedure level_2 is begin
			log (text => "level 2", level => log_threshold + 1);
			
			level_1;
			
			reset_verb_and_noun;
			update_mode_display;
			
			status_enter_verb;
			clear_out_properties_box;
			reset_editing_process;
		end level_2;
	
	
	begin
		log (text => "RESET (board)", level => log_threshold + 1);
		log_indentation_up;
		
		escape_key_pressed;

		expect_entry := expect_entry_default; -- expect a verb
		
		-- Verb and noun remain as they are
		-- so that the mode is unchanged.

		case get_escape_counter is
			when 0 => null;
			
			when 1 =>
				case verb is
					when VERB_PLACE =>
						level_2;

					when others => level_1;
				end case;
				
			when 2 =>
				level_2;

		end case;

		log_indentation_down;
		
		redraw_board;
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
			use et_board_coordinates.pac_grid;
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
		
		-- put_line ("cb_canvas_key_pressed (board)"
		-- 	& " key " & gdk_key_type'image (event.keyval));

		if key_ctrl = control_mask then 
			case key is
				when GDK_KP_ADD | GDK_PLUS =>
					zoom_on_cursor (ZOOM_IN);

					
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
					et_canvas_schematic_2.save_module;

					
					
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
		-- put_line ("cb_canvas_button_pressed (board)");

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
		-- put_line ("cb_canvas_button_released (board)");
		
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
		-- put_line ("cb_canvas_mouse_moved (board)");

		-- Get from the mouse event the model point:
		mp := get_mouse_moved_event (event);

		mouse_moved (mp);
		
		return event_handled;
	end cb_canvas_mouse_moved;
	


	
	procedure set_up_canvas is begin
		put_line ("set_up_canvas (board)");

		-- Connect signals:

		canvas.on_draw (cb_draw'access);
		-- NOTE: No context is declared here, because the canvas widget
		-- passes its own context to the callback procedure cb_draw.

		canvas.on_key_press_event (cb_canvas_key_pressed'access);

		canvas.on_button_press_event (cb_canvas_button_pressed'access);
		canvas.on_button_release_event (cb_canvas_button_released'access);

		canvas.on_motion_notify_event (cb_canvas_mouse_moved'access);
	end set_up_canvas;


	
	-- This procedure should be called each time after the current 
	-- active module changes. 
	-- It removes all property bars (if being displayed) and
	-- calls other procedures that initialize the values used in 
	-- property bars for vias, tracks, ...
	procedure init_property_bars is 
		use et_canvas_board_vias;
	begin
		null;
		-- CS reset_preliminary_via;

		--  CS init route
		-- CS init text
		-- ...
	end init_property_bars;


	

	

-- REDRAW / REFRESH:
	
	procedure redraw_board is begin
		refresh;
	end redraw_board;

	
	procedure redraw_schematic is begin
		et_canvas_schematic_2.pac_canvas.refresh;
	end redraw_schematic;
	

	procedure redraw is begin
		redraw_schematic;
		redraw_board;
	end redraw;




-- MODULE SELECT:

	procedure update_board_editor is 
		use et_board_ops.grid;
	begin
		-- Show the module name in the title bar of 
		-- the board editor:
		set_title_bar (active_module);

		pac_canvas.grid := 
			get_grid (active_module, log_threshold + 1);
		
		pac_canvas.update_grid_display;

		-- Init defaults of property bars in board:
		init_property_bars;

		-- CS
		-- zoom-fit ?
		-- move cursor home ?
		-- displayed objects, layers, ... ?
	end update_board_editor;

	

	

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
		use et_scripting;
		use et_modes;
		use et_domains;
		use et_project;

		-- We assemble a command that executes a script
		-- like "board motor_driver execute script my_script.scr:
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_BOARD)) & space &
			get_active_module & space &
			"execute" & space & "script" & space &
			to_string (script); -- "my_script.scr"
		
		fields : et_string_processing.type_fields_of_line;

		-- The command to be executed:
		single_cmd : type_single_cmd;
		
		-- The command launches a script. Change into the project directory. 
		-- The current directory is the parent directory of the active project. 
		-- Example: The current directory is /home/user/my_projects . The directory
		--  of the current project is /home/user/my_projects/blood_sample_analyzer.
		--  Executing a script requires changing into the project directory blood_sample_analyzer.

		-- Backup the current directory (like /home/user/my_projects):
		cur_dir_bak : constant string := current_directory;
		
	begin
		
		log (text => "executing script (in board domain) " 
			 & enclose_in_quotes (line_as_typed_by_operator), 
			 level => log_threshold);
		
		log_indentation_up;
		
		-- Store the command in the command history:
		console.prepend_text (line_as_typed_by_operator);

		fields := read_line (
			line 			=> line_as_typed_by_operator,
			number			=> 1,  -- this is the one and only line
			comment_mark 	=> et_script_processor.comment_mark,
			delimiter_wrap	=> true, -- strings are enclosed in quotations
			ifs 			=> space); -- fields are separated by space

		--log (text => "full command " & enclose_in_quotes (to_string (cmd)), level => log_threshold + 1);

		set_directory (to_string (active_project));
		
		-- Execute the board command. Since it is launched from
		-- inside the board editor, its origin must be set accordingly:
		set_fields (single_cmd, fields);
		set_origin (single_cmd, ORIGIN_CONSOLE);
		execute_board_command (active_module, single_cmd, log_threshold);

		-- Return to previous directory (like  /home/user/my_projects):
		set_directory (cur_dir_bak);
		
		-- The majority of commands requires refreshing the schematic and board drawing.
		
		-- refresh board and schematic
		-- CS redraw;
		--redraw (canvas);
		--et_canvas_schematic.pac_canvas.redraw (et_canvas_schematic.pac_canvas.canvas);
		
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
		use et_string_processing;
		use et_scripting;
		use et_modes;
		use et_domains;
		use et_project;
		
		-- The operator enters a command like "rename device R1 R2".
		-- The operator is not required to type domain and module name.
		-- Since we are editing a board, the domain and module name itelf
		-- are known. By prepending domain and module name here the full 
		-- command after this declaration will be "board led_driver rename device R1 R2".		
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_BOARD)) & space &
			get_active_module & space &
			get_text (self);
		
		fields : et_string_processing.type_fields_of_line;

		-- The command to be executed:
		single_cmd : type_single_cmd;

		-- The command might launch a script. To prepare for this case we must change
		-- into the project directory. The current directory is the parent directory
		-- of the active project. 
		-- Example: The curreent directory is /home/user/my_projects . The directory
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
			number			=> 1, -- this is the one and only line
			comment_mark 	=> et_script_processor.comment_mark,
			delimiter_wrap	=> true, -- strings are enclosed in quotations
			ifs 			=> space); -- fields are separated by space

		--log (text => "full command " & enclose_in_quotes (to_string (cmd)), level => log_threshold + 1);

		log (text => "changing to directory " &
				enclose_in_quotes (to_string (active_project)) & " ...",
			level => log_threshold + 1);
		
		set_directory (to_string (active_project));
		
		-- Compose and execute the board command.
		-- Since it is launched from inside the board editor
		-- its origin is set accordingly:
		set_fields (single_cmd, fields);
		set_origin (single_cmd, ORIGIN_CONSOLE);
		execute_board_command (active_module, single_cmd, log_threshold);
		
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


	
end et_canvas_board_2;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
