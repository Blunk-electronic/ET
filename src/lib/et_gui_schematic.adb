------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            GUI SCHEMATIC                                 --
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
with gtk.window; 				use gtk.window;
with gtk.widget;  				use gtk.widget;
with gtk.box;					use gtk.box;
with gtk.button;     			use gtk.button;
with gtk.toolbar; 				use gtk.toolbar;
with gtk.tool_button;			use gtk.tool_button;
with gtk.enums;					use gtk.enums;
with gtk.gentry;				use gtk.gentry;
with gtk.frame;					use gtk.frame;
with gtk.scrolled_window;		use gtk.scrolled_window;
with glib.object;				use glib.object;

with ada.text_io;				use ada.text_io;
with ada.directories;

with et_gui_schematic.callbacks;	use et_gui_schematic.callbacks;	
with et_canvas_schematic;			use et_canvas_schematic;
		
use et_canvas_schematic.pac_canvas;


package body et_gui_schematic is

	procedure init_window (
		project			: in pac_project_name.bounded_string;	-- blood_sample_analyzer
		module			: in pac_generic_modules.cursor; -- cursor of generic module to be edited
		sheet			: in et_coordinates.type_sheet := et_coordinates.type_sheet'first; -- the sheet to be opened
		log_threshold_in: in type_log_level) is
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
		window.on_configure_event (window_resized'access);

		-- For reaction to keys pressed on the keyboard:
		window.on_key_press_event (on_key_event'access);
		
		build_background_boxes;

		build_coordinates_display;

		-- Connect to the on_activate signal (on hitting enter key) of the entry (which is a child of console):
		gtk_entry (cursor_position_x.get_child).on_activate (set_cursor_position_x'access);
		gtk_entry (cursor_position_y.get_child).on_activate (set_cursor_position_y'access);

		
-- 		-- toolbar on the left
-- 		gtk_new (toolbar);
-- 		set_orientation (toolbar, orientation_vertical);
-- 		pack_start (box_left, toolbar, expand => false);


		

		
		
-- 		-- Create a button and place it in the toolbar:
-- 		gtk.tool_button.gtk_new (button_zoom_to_fit, label => "FIT");
-- 		insert (toolbar, button_zoom_to_fit);
-- 
-- 		-- If the operator clicks the button
-- 		-- call the procedure zoom_to_fit in package callbacks_4:
-- 		button_zoom_to_fit.on_clicked (zoom_to_fit'access, toolbar);



		
-- 		-- Create a button and place it in the toolbar:
-- 		gtk.tool_button.gtk_new (button_zoom_in, label => "IN");
-- 		insert (toolbar, button_zoom_in);
-- 
-- 		-- If the operator clicks the button
-- 		-- call the procedure zoom_in in package callbacks_4:
-- 		button_zoom_in.on_clicked (zoom_in'access, toolbar);



		
-- 		-- Create another button and place it in the toolbar:
-- 		gtk.tool_button.gtk_new (button_zoom_out, label => "OUT");
-- 		insert (toolbar, button_zoom_out);
-- 
-- 		-- If the operator clicks the button
-- 		-- call the procedure zoom_out in package callbacks_4:
-- 		button_zoom_out.on_clicked (zoom_out'access, toolbar);



		
		build_console;
		
		-- Connect to the on_activate signal of the entry (which is a child of console):
		gtk_entry (console.get_child).on_activate (execute_command'access); -- on hitting enter
		

		build_canvas;
		gtk_new (canvas);
		
		-- set the module to be opened and optionally the sheet to be displayed:
		init_drawing (module, sheet);

		-- draw the schematic sheet
-- 		redraw (canvas); -- CS no need
		
		add (scrolled, canvas); -- place the canvas in the scrolled window
		
		scale_to_fit (canvas);

		-- display the schematic:
		window.show_all;

		
	end init_window;
	
	
end et_gui_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
