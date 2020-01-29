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

with gtk.main;
with gtk.window; 			use gtk.window;
with gtk.widget;  			use gtk.widget;
with gtk.box;				use gtk.box;
with gtk.button;     		use gtk.button;
with gtk.toolbar; 			use gtk.toolbar;
with gtk.tool_button;		use gtk.tool_button;
with gtk.enums;				use gtk.enums;
with gtk.gentry;			use gtk.gentry;
with gtk.combo_box_text;	use gtk.combo_box_text;
with gtk.frame;				use gtk.frame;
with gtk.scrolled_window;	use gtk.scrolled_window;
with glib.object;			use glib.object;

with ada.text_io;			use ada.text_io;
with ada.directories;

with et_general;				use et_general;
with et_project;				use et_project;
with et_string_processing;		use et_string_processing;

with gui_cb;					use gui_cb;	
with canvas_schematic;

with gui_schematic;

package body gui is

	window_schematic, window_board	: gtk_window; -- This is an access/pointer to the actual window.
	box_back				: gtk_box; -- This is an access/pointer to the actual box.
	box_left, box_right		: gtk_box;
	box_console				: gtk_box;
	box_drawing				: gtk_box;

	-- We will have some buttons:
	button_zoom_to_fit					: gtk_tool_button; -- This is an access/pointer to the actual button.
	button_zoom_in, button_zoom_out		: gtk_tool_button;

	-- We will have a toolbar, a console, a frame and a scrolled window:
	toolbar					: gtk_toolbar; -- This is an access/pointer to the actual toolbar.
	console					: gtk_entry;
	frame					: gtk_frame;
	scrolled				: gtk_scrolled_window;
	
	procedure init_schematic_window is begin

		gtk_new (window_schematic); -- create the main window (where pointer "window" is pointing at)
		window_schematic.set_title (system_name & " SCHEMATIC");
		window_schematic.set_default_size (1024, 768);

		-- If the operator wishes to terminate the program (by clicking X)
		-- the procedure terminate_main (in gui_cb) is to be called.
		window_schematic.on_destroy (terminate_main'access);

		-- If the operator minimizes, maximizes or changes the size in some way:
		window_schematic.on_configure_event (window_resized'access);

		-- For reaction to keys pressed on the keyboard:
		-- This is required in order to propagate the key-pressed events to sub-windows.
		window_schematic.on_key_press_event (on_key_event'access);
		

		
		-- background box
		gtk_new_hbox (box_back);
		set_spacing (box_back, 10);
		add (window_schematic, box_back);

		-- left box
		gtk_new_hbox (box_left);
		set_spacing (box_left, 10);
		pack_start (box_back, box_left, expand => false);

		-- right box
		gtk_new_vbox (box_right);
		set_spacing (box_right, 10);
		add (box_back, box_right);

		-- toolbar on the left
		gtk_new (toolbar);
		set_orientation (toolbar, orientation_vertical);
		pack_start (box_left, toolbar, expand => false);



		
		-- Create a button and place it in the toolbar:
		gtk.tool_button.gtk_new (button_zoom_to_fit, label => "FIT");
		insert (toolbar, button_zoom_to_fit);

		-- If the operator clicks the button
		-- call the procedure zoom_to_fit in package callbacks_4:
		button_zoom_to_fit.on_clicked (zoom_to_fit'access, toolbar);



		
		-- Create a button and place it in the toolbar:
		gtk.tool_button.gtk_new (button_zoom_in, label => "IN");
		insert (toolbar, button_zoom_in);

		-- If the operator clicks the button
		-- call the procedure zoom_in in package callbacks_4:
		button_zoom_in.on_clicked (zoom_in'access, toolbar);



		
		-- Create another button and place it in the toolbar:
		gtk.tool_button.gtk_new (button_zoom_out, label => "OUT");
		insert (toolbar, button_zoom_out);

		-- If the operator clicks the button
		-- call the procedure zoom_out in package callbacks_4:
		button_zoom_out.on_clicked (zoom_out'access, toolbar);



		
		-- box for console on the right top
		gtk_new_vbox (box_console);
		set_spacing (box_console, 10);
		pack_start (box_right, box_console, expand => false);

		-- a simple text entry
		gtk_new (console);
		set_text (console, "cmd: ");
		pack_start (box_console, console, expand => false);

		-- If the operator hits enter after typing text in the console,
		-- call the procedure echo_command_simple in package callbacks_4:
		console.on_activate (echo_command_simple'access); -- on hitting enter



		
		-- drawing area on the right bottom
		gtk_new_hbox (box_drawing);
		set_spacing (box_drawing, 10);
		add (box_right, box_drawing);

		-- frame inside the drawing box
		gtk_new (frame);
		pack_start (box_drawing, frame);

		-- scrolled window inside the frame
		gtk_new (scrolled);
		set_policy (scrolled, policy_automatic, policy_automatic);
		add (frame, scrolled);

	end init_schematic_window;

	
	procedure init_board_window is begin

		gtk_new (window_board); -- create the main window_board (where pointer "window_board" is pointing at)
		window_board.set_title (system_name & " BOARD");
		window_board.set_default_size (1024, 768);

		-- If the operator wishes to terminate the program (by clicking X)
		-- the procedure terminate_main (in gui_cb) is to be called.
		window_board.on_destroy (terminate_main'access);

		-- If the operator minimizes, maximizes or changes the size in some way:
		window_board.on_configure_event (window_resized'access);

		-- For reaction to keys pressed on the keyboard:
		-- This is required in order to propagate the key-pressed events to sub-windows.
		window_board.on_key_press_event (on_key_event'access);
		

		
		-- background box
		gtk_new_hbox (box_back);
		set_spacing (box_back, 10);
		add (window_board, box_back);

		-- left box
		gtk_new_hbox (box_left);
		set_spacing (box_left, 10);
		pack_start (box_back, box_left, expand => false);

		-- right box
		gtk_new_vbox (box_right);
		set_spacing (box_right, 10);
		add (box_back, box_right);

		-- toolbar on the left
		gtk_new (toolbar);
		set_orientation (toolbar, orientation_vertical);
		pack_start (box_left, toolbar, expand => false);



		
		-- Create a button and place it in the toolbar:
		gtk.tool_button.gtk_new (button_zoom_to_fit, label => "FIT");
		insert (toolbar, button_zoom_to_fit);

		-- If the operator clicks the button
		-- call the procedure zoom_to_fit in package callbacks_4:
		button_zoom_to_fit.on_clicked (zoom_to_fit'access, toolbar);



		
		-- Create a button and place it in the toolbar:
		gtk.tool_button.gtk_new (button_zoom_in, label => "IN");
		insert (toolbar, button_zoom_in);

		-- If the operator clicks the button
		-- call the procedure zoom_in in package callbacks_4:
		button_zoom_in.on_clicked (zoom_in'access, toolbar);



		
		-- Create another button and place it in the toolbar:
		gtk.tool_button.gtk_new (button_zoom_out, label => "OUT");
		insert (toolbar, button_zoom_out);

		-- If the operator clicks the button
		-- call the procedure zoom_out in package callbacks_4:
		button_zoom_out.on_clicked (zoom_out'access, toolbar);


		
		



		
		-- box for console on the right top
		gtk_new_vbox (box_console);
		set_spacing (box_console, 10);
		pack_start (box_right, box_console, expand => false);

		-- a simple text entry
		gtk_new (console);
		set_text (console, "cmd: ");
		pack_start (box_console, console, expand => false);

		-- If the operator hits enter after typing text in the console,
		-- call the procedure echo_command_simple in package callbacks_4:
		console.on_activate (echo_command_simple'access); -- on hitting enter



		
		-- drawing area on the right bottom
		gtk_new_hbox (box_drawing);
		set_spacing (box_drawing, 10);
		add (box_right, box_drawing);

		-- frame inside the drawing box
		gtk_new (frame);
		pack_start (box_drawing, frame);

		-- scrolled window inside the frame
		gtk_new (scrolled);
		set_policy (scrolled, policy_automatic, policy_automatic);
		add (frame, scrolled);

	end init_board_window;

	
	procedure single_module (
		module			: in type_modules.cursor; -- cursor of generic module to be edited
		sheet			: in et_coordinates.type_sheet := et_coordinates.type_sheet'first; -- the sheet to be opened
		log_threshold	: in type_log_level) is
		use canvas_schematic;
		use canvas_schematic.pac_canvas;
	begin
		log (text => "launching mode " & to_string (MODE_MODULE), level => log_threshold);
		log (text => "opening module " & enclose_in_quotes (to_string (type_modules.key (module))), level => log_threshold);
		log (text => "sheet" & to_sheet (sheet), level => log_threshold);

		gtk.main.init; -- initialize the main gtk stuff

	-- SCHEMATIC
		
		-- set up the schematic window
		init_schematic_window;

		gtk_new (canvas);
		add (scrolled, canvas); -- place the canvas in the scrolled window

		-- set the module to be opened and optionally the sheet to be displayed:
		init_drawing (canvas, module, sheet);
		
		scale_to_fit (canvas);
		
		-- display the schematic:
		window_schematic.show_all;


		

	-- BOARD
		
		-- set up the board window
		init_board_window;

		-- display the board
		window_board.show_all;



		
		-- Start the main gtk loop. This is a loop that permanently draws the widgets and
		-- samples them for possible signals sent.
		gtk.main.main;
		
	end single_module;
	
	
end gui;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
