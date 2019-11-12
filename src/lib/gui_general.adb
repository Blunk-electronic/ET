------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             GUI GENERAL                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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
with gtk.window; 				--use gtk.window;

-- with gtk.widget;  				--use gtk.widget;
-- with gtk.box;					use gtk.box;
-- with gtk.button;     			use gtk.button;
-- with gtk.label;					use gtk.label;
-- with gtk.image;					use gtk.image;
-- with gtk.file_chooser;			use gtk.file_chooser;
-- with gtk.file_chooser_button;	use gtk.file_chooser_button;
-- with gtk.file_filter;			use gtk.file_filter;
-- with gtkada.handlers; 			use gtkada.handlers;
-- with glib.object;
-- with gdk.event;


with ada.directories;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;

with gui_cb;					

package body gui_general is

	
	procedure single_module (
		log_threshold	: in type_log_level) is

		procedure create_main_window is
			use gui_cb;
			window : gtk.window.gtk_window;
		begin
			gtk.window.gtk_new (window);
			gtk.window.set_title (window => window, title  => system_name);

			--  Construct the window and connect various callbacks

			
			window.on_destroy (terminate_main'access);

			window.show_all;
		end;

	begin
		log (text => "launching mode " & to_string (MODE_MODULE), level => log_threshold);

		-- initialize gtkada
		gtk.main.init;

		create_main_window;



		

		-- All GTK applications must have a Gtk.Main.Main. Control ends here
		-- and waits for an event to occur (like a key press or a mouse event),
		-- until Gtk.Main.Main_Quit is called.
		gtk.main.main;


		
	end single_module;
	
	
end gui_general;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
