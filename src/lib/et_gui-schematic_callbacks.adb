------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       GUI SCHEMATIC CALLBACKS                            --
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

with et_coordinates;			use et_coordinates;
with et_canvas_schematic;		use et_canvas_schematic;
with et_canvas_board;

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.directories;

package body et_gui.schematic_callbacks is

	use et_canvas_schematic.pac_canvas;
	use et_coordinates.pac_geometry_sch;
	
	procedure terminate_main (self : access gtk_widget_record'class) is begin
		put_line ("exiting ...");
		gtk.main.main_quit;
	end;

-- 	function window_resized (
-- 		self  : access gtk_widget_record'class;
-- 		event : gdk.event.gdk_event_configure) 
-- 		return boolean is
-- 		-- Get the current scale:
-- 		scale : gdouble := get_scale (canvas);
-- 	begin
-- 		-- Assign the scale anew:
-- 		set_scale (canvas, scale);
-- 
-- 		-- NOTE: Do not call scale_to_fit here !
-- 		-- It would undo zoom operations called by a script that is passed
-- 		-- on startup.
-- 		
-- 		return true;
-- 	end;

	procedure set_cursor_position_x (self : access gtk.gentry.gtk_entry_record'class) is 
		use et_general;
		use gtk.gentry;
		cp : type_point := cursor_main.position;
	begin
		set (point => cp, axis => X, value => to_distance (get_text (self)));
		move_cursor (canvas, ABSOLUTE, cursor_main, cp);
		
		redraw (canvas);
	end set_cursor_position_x;

	procedure set_cursor_position_y (self : access gtk.gentry.gtk_entry_record'class) is 
		use et_general;
		use gtk.gentry;
		cp : type_point := cursor_main.position;
	begin
		set (point => cp, axis => Y, value => to_distance (get_text (self)));
		move_cursor (canvas, ABSOLUTE, cursor_main, cp);
		
		redraw (canvas);
	end set_cursor_position_y;
	
-- 	procedure zoom_to_fit (self : access glib.object.gobject_record'class) is 
-- 	begin
-- 		put_line ("zoom to fit ...");
-- 		scale_to_fit (canvas);
-- -- 		put_line (to_string (get_scale (canvas)));
-- 	end;

-- 	procedure zoom_in (self : access glib.object.gobject_record'class) is 
-- 		scale : gdouble;
-- 		use geometry;
-- 	begin
-- 		put_line ("zooming in ...");
-- 		scale := get_scale (canvas);
-- 		scale := scale + 0.1;
-- 		set_scale (canvas, scale);
-- 		-- 		put_line (to_string (get_scale (canvas)));
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

	procedure execute_script (script : in pac_script_name.bounded_string) is
		use ada.directories;
		use et_string_processing;
		use et_scripting;
		use pac_script_name;

		-- We assemble a command that executes a script
		-- like "schematic motor_driver execute script my_script.scr:
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_SCHEMATIC)) & space &
			et_general.to_string (et_canvas_schematic.active_module) & space &
			"execute" & space & "script" & space &
			et_general.to_string (script); -- "my_script.scr"
		
		cmd : et_string_processing.type_fields_of_line;

		exit_code : type_exit_code := SUCCESSFUL;

		-- The command launches a script. Change into the project directory. 
		-- The current directory is the parent directory of the active project. 
		-- Example: The current directory is /home/user/my_projects . The directory
		--  of the current project is /home/user/my_projects/blood_sample_analyzer.
		--  Executing a script requires changing into the project directory blood_sample_analyzer.

		-- Backup the current directory (like /home/user/my_projects):
		cur_dir_bak : constant string := current_directory;
	begin
		log (text => "executing command " & enclose_in_quotes (line_as_typed_by_operator), level => log_threshold);
		log_indentation_up;
		
		-- Store the command in the command history:
		console.prepend_text (line_as_typed_by_operator);

		cmd := read_line (
			line 			=> line_as_typed_by_operator,
			number			=> 1,  -- this is the one and only line
			comment_mark 	=> et_scripting.comment_mark,
			delimiter_wrap	=> true, -- strings are enclosed in quotations
			ifs 			=> latin_1.space); -- fields are separated by space

		--log (text => "full command " & enclose_in_quotes (to_string (cmd)), level => log_threshold + 1);

		set_directory (to_string (et_canvas_schematic.current_active_project));
		
		-- execute the schematic command
		exit_code := schematic_cmd (cmd, log_threshold);

		-- Return to previous directory (like  /home/user/my_projects):
		set_directory (cur_dir_bak);
		
		-- CS evaluate exit_code

		-- The majority of commands requires refreshing the schematic and board drawing.
		
		-- refresh schematic and board
		redraw (canvas);
		et_canvas_board.redraw (et_canvas_board.pac_canvas.canvas);
		
		-- CS output error message in gui


		log_indentation_down;

	end execute_script;
	
	procedure execute_command (self : access gtk.gentry.gtk_entry_record'class) is 
		use ada.directories;	
		use gtk.gentry;
		use et_string_processing;
		use et_scripting;

		-- The operator enters a command like "rename device R1 R2".
		-- The operator is not required to type domain and module name.
		-- Since we are editing a schematic, the domain and module name itelf
		-- are known. By prepending domain and module name here the full 
		-- command after this declaration will be "schematic led_driver rename device R1 R2".
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_SCHEMATIC)) & latin_1.space &
			et_general.to_string (active_module) & latin_1.space &
			get_text (self);
		
		cmd : et_string_processing.type_fields_of_line;

		exit_code : type_exit_code := SUCCESSFUL;

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

		cmd := read_line (
			line 			=> line_as_typed_by_operator,
			number			=> 1,  -- this is the one and only line
			comment_mark 	=> et_scripting.comment_mark,
			delimiter_wrap	=> true, -- strings are enclosed in quotations
			ifs 			=> latin_1.space); -- fields are separated by space

		--log (text => "full command " & enclose_in_quotes (to_string (cmd)), level => log_threshold + 1);

		log (text => "change to directory " &
				enclose_in_quotes (to_string (current_active_project)) & " ...",
			level => log_threshold + 1);
		
		set_directory (to_string (current_active_project));
		
		-- execute the schematic command
		exit_code := schematic_cmd (cmd, log_threshold);

		-- Return to previous directory (like  /home/user/my_projects):
		log (text => "returning to directory " & enclose_in_quotes (cur_dir_bak) & " ...",
			level => log_threshold + 1);
		
		set_directory (cur_dir_bak);
		
		-- CS evaluate exit_code

		-- The majority of commands requires refreshing the schematic and board drawing.
		
		-- refresh schematic and board
		redraw (canvas);
		et_canvas_board.redraw (et_canvas_board.pac_canvas.canvas);
		
		-- CS output error message in gui

		log_indentation_down;
	end execute_command;


	function on_key_event (
		self	: access gtk_widget_record'class;
		event	: in gdk_event_key) 
		return boolean is

		-- This is required in order to propagate the key-pressed events to sub-windows.		
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

-- 	function on_button_event (
-- 		self  : access gtk_widget_record'class;
-- 		event : gdk_event_button)
-- 		return boolean
-- 	is
-- -- 		self    : constant type_view_ptr := type_view_ptr (view);
-- 	begin
-- 		put_line ("top mouse button pressed");
-- 
-- 		return false; -- indicates that event has been handled
-- 	end on_button_event;

	
end et_gui.schematic_callbacks;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16