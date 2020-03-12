------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GUI BOARD                                   --
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
-- with gtk.gentry;			use gtk.gentry;
-- with gtk.combo_box_text;	use gtk.combo_box_text;
-- with gtk.frame;				use gtk.frame;
-- with gtk.scrolled_window;	use gtk.scrolled_window;
with glib.object;			use glib.object;

with ada.text_io;			use ada.text_io;
with ada.directories;

with et_general;				use et_general;
with et_project;				use et_project;
with et_string_processing;		use et_string_processing;

with gui_board.callbacks;		use gui_board.callbacks;
with et_canvas_board;			use et_canvas_board;
		
use et_canvas_board.pac_canvas;


package body gui_board is

	procedure init_window (
		module			: in type_modules.cursor; -- cursor of generic module to be edited
		log_threshold_in: in type_log_level) is
	begin
		-- Set the log threshold. Everything that happens in the gui may be logged
		-- using the gui wide variable log_threshold:
		pac_canvas.log_threshold := log_threshold_in;

		gtk_new (window); -- create the main window (where pointer "window" is pointing at)

		-- Show the module name in the title bar:
		set_title_bar (type_modules.key (module));
		
		window.set_default_size (1024, 768);

		-- If the operator wishes to terminate the program (by clicking X)
		-- the procedure terminate_main (in gui_cb) is to be called.
		window.on_destroy (terminate_main'access);

		-- If the operator minimizes, maximizes or changes the size in some way:
		window.on_configure_event (window_resized'access);

		-- For reaction to keys pressed on the keyboard:
		-- This is required in order to propagate the key-pressed events to sub-windows.
		window.on_key_press_event (on_key_event'access);
		

		build_background_boxes;
		
		build_position_display;

		-- Connect to the on_activate signal of the entry (which is a child of console):
		-- 		gtk_entry (cursor_x.get_child).set_text ("test"); --gui_schematic.callbacks.execute_command'access); -- on hitting enter
		gtk_entry (cursor_position_x.get_child).on_activate (gui_board.callbacks.set_cursor_position_x'access); -- on hitting enter
		
-- 		-- toolbar on the left
-- 		gtk_new (toolbar);
-- 		set_orientation (toolbar, orientation_vertical);
-- 		pack_start (box_left, toolbar, expand => false);
-- 

-- 		
-- 		-- Create a button and place it in the toolbar:
-- 		gtk.tool_button.gtk_new (button_zoom_to_fit, label => "FIT");
-- 		insert (toolbar, button_zoom_to_fit);
-- 
-- 		-- If the operator clicks the button
-- 		-- call the procedure zoom_to_fit in package callbacks_4:
-- 		button_zoom_to_fit.on_clicked (zoom_to_fit'access, toolbar);
-- 
-- 
-- 
-- 		
-- 		-- Create a button and place it in the toolbar:
-- 		gtk.tool_button.gtk_new (button_zoom_in, label => "IN");
-- 		insert (toolbar, button_zoom_in);
-- 
-- 		-- If the operator clicks the button
-- 		-- call the procedure zoom_in in package callbacks_4:
-- 		button_zoom_in.on_clicked (zoom_in'access, toolbar);
-- 
-- 
-- 
-- 		
-- 		-- Create another button and place it in the toolbar:
-- 		gtk.tool_button.gtk_new (button_zoom_out, label => "OUT");
-- 		insert (toolbar, button_zoom_out);
-- 
-- 		-- If the operator clicks the button
-- 		-- call the procedure zoom_out in package callbacks_4:
-- 		button_zoom_out.on_clicked (zoom_out'access, toolbar);



		build_console;

		-- Connect to the on_activate signal of the entry (which is a child of console):
		gtk_entry (console.get_child).on_activate (gui_board.callbacks.execute_command'access); -- on hitting enter

		build_canvas;
		gtk_new (canvas);

		-- draw the board layout
		redraw (canvas);
		
		add (scrolled, canvas); -- place the canvas in the scrolled window
		
		scale_to_fit (canvas);
		
		-- display the board:
		window.show_all;

		
	end init_window;
	
	
end gui_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
