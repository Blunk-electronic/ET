------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          GUI BOARD CALLBACKS                             --
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

with gdk.event;					use gdk.event;

with gtk.main;
with gtk.window;				use gtk.window;
with ada.text_io;				use ada.text_io;
with et_general;
with et_canvas_board;			use et_canvas_board;
use et_canvas_board.pac_canvas;
with et_canvas_schematic;

with et_string_processing;		use et_string_processing;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with scripting;

package body gui_board.callbacks is

	procedure terminate_main (self : access gtk_widget_record'class) is begin
		put_line ("exiting ...");
		gtk.main.main_quit;
	end;

	function window_resized (
		self  : access gtk_widget_record'class;
		event : gdk.event.gdk_event_configure) 
		return boolean is
		-- Get the current scale:
		scale : gdouble := get_scale (canvas);
	begin
		-- Assign the scale anew:
		set_scale (canvas, scale);
		
		return true;
	end;
		
-- 	procedure zoom_to_fit (self : access glib.object.gobject_record'class) is 
-- 	begin
-- 		put_line ("zoom to fit ...");
-- 		scale_to_fit (canvas);
-- -- 		put_line (to_string (get_scale (canvas)));
-- 	end;

-- 	procedure zoom_in (self : access glib.object.gobject_record'class) is 
-- 		scale : gdouble;
-- 	begin
-- 		put_line ("zooming in ...");
-- 		scale := get_scale (canvas);
-- 		scale := scale + 0.1;
-- 		set_scale (canvas, scale);
-- -- 		put_line (to_string (get_scale (canvas)));
-- 	end;

-- 	procedure zoom_out (self : access glib.object.gobject_record'class) is 
-- 		scale : gdouble;
-- 	begin
-- 		put_line ("zooming out ...");
-- 		scale := get_scale (canvas);
-- 		if scale >= 0.0 then
-- 			scale := scale - 0.1;
-- 			set_scale (canvas, scale);
-- 		end if;
-- -- 		put_line (to_string (get_scale (canvas)));
-- 	end;

	procedure execute_command (self : access gtk.gentry.gtk_entry_record'class) is 
		use gtk.gentry;
		use et_string_processing;
		use scripting;

		-- The operator enters a command like "rename device R1 R2".
		-- The operator is not required to type domain and board name.
		-- Since we are editing a board, the domain and board name itelf
		-- are known. By prepending domain and board name here the full 
		-- command after this declaration will be "board led_driver rename device R1 R2".		
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_BOARD)) & latin_1.space &
			et_general.to_string (et_canvas_schematic.active_module) & latin_1.space &
			get_text (self);
		
		cmd : et_string_processing.type_fields_of_line;

		exit_code : type_exit_code := SUCCESSFUL;

		-- build an access to the board canvas:
		type type_local_view_ptr is access all et_canvas_board.type_view;
		canvas_board : type_local_view_ptr := type_local_view_ptr (canvas);
		
	begin
		log (text => "executing command " & enclose_in_quotes (get_text (self)), level => log_threshold);
		log_indentation_up;

		-- Store the latest command in the command history:
		console.prepend_text (get_text (self));
		
		cmd := read_line (
			line 			=> line_as_typed_by_operator,
			number			=> 1, -- this is the one and only line
			comment_mark 	=> scripting.comment_mark, -- comments start with "--"
			delimiter_wrap	=> true, -- strings are enclosed in quotations
			ifs 			=> latin_1.space); -- fields are separated by space

		-- The 3rd field of the command indicates whether it is
		-- drawing related or canvas related.
		if is_canvas_related (et_string_processing.field (cmd, 3)) then
			log (text => "command is canvas related", level => log_threshold);

			-- execute the canvas board command
			et_canvas_board.execute_command (
				self			=> canvas_board,
				cmd				=> remove (cmd, 1, 2), -- field 1..2 no longer required
				log_threshold	=> log_threshold);

		else
			log (text => "command is board related", level => log_threshold);

			-- execute the board command
			exit_code := board_cmd (cmd, log_threshold);

			-- CS evaluate exit_code
			
		end if;
		
		-- The majority of commands requires refreshing the schematic and board drawing.

		-- refresh board
		redraw (canvas);

		-- refresh schematic (because some commands also affect the schematic)
		et_canvas_schematic.redraw (et_canvas_schematic.pac_canvas.canvas);
		

		-- CS output error message in gui

		log_indentation_down;
	end execute_command;

	function on_key_event (
		self	: access gtk_widget_record'class;
		event	: in gdk_event_key) 
		return boolean is
		
		result : boolean; -- to be returned. Indicates that the event has been handled.

		-- Make a pointer to the main window:
		current_window : constant gtk_window := gtk_window (self);

	begin
		--new_line;
-- 		put_line ("top level key pressed");

		-- Set the focus to the canvas:
-- 		set_focus (current_window, canvas);

		-- Propagate the key-press event to the canvas:
		result := propagate_key_event (current_window, event);

-- 		if result = true then
-- 			put_line ("got handled");
-- 		else
-- 			put_line ("not handled");
-- 		end if;
		
		return result;
	end on_key_event;


	
end gui_board.callbacks;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
