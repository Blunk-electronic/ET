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

with et_pcb_coordinates;
with et_terminals;
with et_devices;					use et_devices;

with et_scripting;
with et_modes;
with et_project;

with et_canvas_board;
with et_display.schematic;			use et_display.schematic;
with et_colors;						use et_colors;
with et_colors.schematic;			use et_colors.schematic;
with et_modes.schematic;			use et_modes.schematic;

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;


package body et_canvas_schematic is

	procedure set_title_bar (
		-- CS project name
		module		: in pac_module_name.bounded_string)
	is begin
		window.set_title (title & to_string (module));
	end set_title_bar;


	
	procedure update_sheet_number_display is begin
		gtk_entry (cbox_sheet.get_child).set_text (to_sheet (current_active_sheet));
	end update_sheet_number_display;

	
	procedure build_sheet_number_display is
		spacing : gint;
	begin
		spacing := 10;

		gtk_new_vbox (box_sheet);
		set_spacing (box_sheet, spacing);
		pack_start (box_left, box_sheet, expand => false);
		
		gtk_new (label_sheet, "SHEET (KEYPAD +/-)");
		pack_start (box_sheet, label_sheet, expand => false);
		gtk_new_with_entry (cbox_sheet);
		pack_start (box_sheet, cbox_sheet);
	end build_sheet_number_display;

	
	procedure set_label_console is
		text_before : constant string := label_console.get_text;
	begin
		label_console.set_text (text_before & label_console_text);
	end set_label_console;

	
	procedure redraw_board is begin
		et_canvas_board.redraw_board;
	end redraw_board;

	
	procedure redraw_schematic is begin
		redraw (canvas);
	end redraw_schematic;

	
	procedure redraw is begin
		redraw_schematic;
		redraw_board;
	end redraw;

	
	procedure next_module is
		use pac_generic_modules;
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
		et_canvas_board.set_title_bar (active_module);
		
		-- CS Init defaults of property bars in schematic.
		
		-- Init defaults of property bars in board:
		et_canvas_board.init_property_bars;
		
		-- Redraw both schematic and board:
		redraw;
	end next_module;

	
	procedure previous_module is
		use pac_generic_modules;
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
		et_canvas_board.set_title_bar (active_module);

		-- CS Init defaults of property bars in schematic.
		
		-- Init defaults of property bars in board:
		et_canvas_board.init_property_bars;

		
		-- Redraw both schematic and board:
		redraw;
	end previous_module;



	
	function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_model_point)	
		return type_point 
	is 
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> clip_distance (type_distance (model_point.x - self.frame_bounding_box.x)));
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> clip_distance (type_distance (
						self.get_frame_height
						- model_point.y
						+ self.frame_bounding_box.y)));
		 
		return p;

	end model_to_drawing;
		

	function drawing_to_model (
		self			: not null access type_view;
		drawing_point	: in type_point)	
		return type_model_point 
	is 
		p : type_model_point; -- to be returned
	begin
		p.x := type_float (get_x (drawing_point)) 
			+ self.frame_bounding_box.x;
		
		p.y := self.get_frame_height 
			- type_float (get_y (drawing_point))
			+ self.frame_bounding_box.y;
			
		return p;
	end drawing_to_model;



	
	procedure execute_script (script : in pac_script_name.bounded_string) is
		use ada.directories;
		use et_scripting;
		use et_modes;
		use et_project;
		
		-- We compose a command that executes a script
		-- like "schematic motor_driver execute script my_script.scr:
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_SCHEMATIC)) & space &
			to_string (et_canvas_schematic.active_module) & space &
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
		console.prepend_text (line_as_typed_by_operator);

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
		redraw;
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
		
		-- The majority of commands requires refreshing the schematic and board drawing.
		
		-- refresh schematic and board
		redraw;
		--redraw (canvas);
		--et_canvas_board.pac_canvas.redraw (et_canvas_board.pac_canvas.canvas);
		
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


	
	
	function bounding_box (self : not null access type_view)
		return type_bounding_box is
	begin
		return self.paper_bounding_box; -- CS should include all items of the current sheet.
		-- means: also items outside the frame
	end;


	
	procedure gtk_new (
		self	: out type_view_ptr) is
	begin
		self := new type_view;
		init (self);
	end;



	procedure draw_grid (
		self    : not null access type_view;
		area    : type_bounding_box) is separate;

	
	procedure draw_frame (
		self    : not null access type_view)
		is separate;

	
	procedure draw_tag_label (
		self	: not null access type_view;
		net		: in pac_net_name.bounded_string;
		label	: in type_net_label) is separate;

	
	procedure draw_nets (
		self    : not null access type_view)
		is separate;

	
	procedure draw_texts (
		self    : not null access type_view)
		is separate;

	
	-- Draws a single symbol of the given device:
	procedure draw_symbol (
		self			: not null access type_view;
		symbol			: in et_symbols.type_symbol;
		device_name		: in et_devices.type_device_name := (others => <>);
		device_value	: in pac_device_value.bounded_string := to_value (""); -- like 100R or TL084
		device_purpose	: in pac_device_purpose.bounded_string := to_purpose (""); -- like "brightness control"
		unit_name		: in et_devices.pac_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...
		unit_count		: in et_devices.type_unit_count;
		unit_position	: in type_point; -- x/y on the schematic sheet
		unit_rotation	: in type_rotation := zero_rotation;
		sch_placeholder_name	: in type_text_placeholder;
		sch_placeholder_value	: in type_text_placeholder;
		sch_placeholder_purpose : in type_text_placeholder;
		brightness		: in type_brightness := NORMAL;

		-- If preview is true, then the unit will be drawn less detailled.
		preview			: in boolean := false)
		is separate;

		
	-- Draws all units:
	procedure draw_units (
		self	: not null access type_view) 
		is separate;


	procedure draw_net_route_being_drawn (
		self	: not null access type_view)
	is
		use et_nets;
		line : pac_geometry_2.type_line;

		procedure compute_route (s, e : in type_point) is 

			-- Do the actual route calculation.
			r : type_path := to_path (s, e, route.path.bend_style);

			procedure draw is begin
				-- draw the net segment:
				draw_line (
					line		=> to_line_fine (line),
					width		=> net_line_width);
			end draw;
			
		begin -- compute_route

			-- The calculated route may required a bend point.
			-- Set/clear the "bended" flag of the net_segment being drawn.
			route.path.bended := r.bended;

			-- set color and line width for net segments:
			set_color_nets (context.cr);
			set_line_width (context.cr, type_view_coordinate (net_line_width));

			-- If the route does not require a bend point, draw a single line
			-- from start to end point:
			if r.bended = NO then
				
				line.start_point := r.start_point;
				line.end_point := r.end_point;

				draw;

			-- If the route DOES require a bend point, then draw first a line
			-- from start point to bend point. Then draw a second line from
			-- bend point end point:
			else
				route.path.bend_point := r.bend_point;

				line.start_point := r.start_point;
				line.end_point := r.bend_point;
				
				draw;

				line.start_point := r.bend_point;
				line.end_point := r.end_point;
				
				draw;
				
			end if;
		end compute_route;

		
	begin -- draw_net_route_being_drawn
		if verb = VERB_DRAW and noun = NOUN_NET and route.path.being_drawn = true then

			-- The route start point has been set eariler by procedures
			-- key_pressed or button_pressed.
			-- For drawing here, the route end point is to be taken from
			-- either the mouse pointer or the cursor position:
			case route.path.tool is
				
				when MOUSE => 
					
					compute_route (
						s	=> route.path.start_point,				-- start of route
						e	=> snap_to_grid (self, mouse_position (self)));	-- end of route

				when KEYBOARD =>

					compute_route (
						s	=> route.path.start_point,	-- start of route
						e	=> cursor_main.position);	-- end of route

			end case;
			
		end if;
	end draw_net_route_being_drawn;
	
	
	procedure draw_submodules (
		self	: not null access type_view)
		is separate;

	
	procedure draw_internal (
		self	: not null access type_view;
		area_in	: type_bounding_box) 
	is
		offset : type_offset;
		
		use et_display.schematic;
	begin
-- 		put_line ("draw internal ...");
-- 		shift_area (self, area_shifted, cursor_main);
-- 		shift_area (self, offset, cursor_main);

		frame_height := self.get_frame_height;

		-- The given area must be shifted (left and up) by the position
		-- of the drawing frame. This is required for all objects in the 
		-- drawing frame.
		
		-- Set the global area:
		area := area_in;

		-- Calculate the new position of the global area:
		offset := to_offset (
			x => - self.frame_bounding_box.x,
			y => - self.frame_bounding_box.y);

		
		
		set_color_background (context.cr);
		paint (context.cr);

		if grid_enabled then
			draw_grid (self, area_in);
		end if;
		
		-- move area according to frame position:
		move_by (area, offset);

		
		save (context.cr);
			
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			convert_x (self.frame_bounding_box.x),
			convert_y (self.frame_bounding_box.y));

		
		draw_units (self);
		
		draw_frame (self);
		
		-- Draw nets if layer is enabled:
		if nets_enabled then
			draw_nets (self);
		end if;

		-- Draw texts if layer is enabled:
		if texts_enabled then
			draw_texts (self);
		end if;
		
		draw_submodules (self);

		draw_net_route_being_drawn (self);
		
		draw_cursor (self, cursor_main);
		
		restore (context.cr);
		
	end draw_internal;

	
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
		sheet	: in et_coordinates.type_sheet := et_coordinates.type_sheet'first) -- the sheet to be drawn
	is begin
		-- set the active module:
		current_active_module := module;
		
		-- set active sheet:
		current_active_sheet := sheet;
	end init_drawing;


	
	procedure set_grid_x (self : access gtk_entry_record'class) is
		use et_schematic_ops;

		-- get the current grid		
		grid : type_grid := element (current_active_module).grid;
	begin
		-- Assign grid to x AND y axis so that the operator is not requested
		-- to manually assign y.
		grid.x := to_distance (get_text (self));
		grid.y := to_distance (get_text (self));

		-- Show the grid:
		gtk_entry (grid_x.get_child).set_text (trim (to_string (grid.x), left));
		gtk_entry (grid_y.get_child).set_text (trim (to_string (grid.y), left));
		
		-- Finally set the grid in the module database:
		set_grid (current_active_module, grid, log_threshold + 1);
		redraw (canvas);
	end set_grid_x;

	
	procedure set_grid_y (self : access gtk_entry_record'class) is
		use et_schematic_ops;

		-- get the current grid	
		grid : type_grid := element (current_active_module).grid;
	begin
		-- Assign grid to y axis:
		grid.y := to_distance (get_text (self));

		-- Show the grid:
		gtk_entry (grid_y.get_child).set_text (trim (to_string (grid.y), left));
		
		-- Finally set the grid in the module database:
		set_grid (current_active_module, grid, log_threshold + 1);
		redraw (canvas);
	end set_grid_y;

	
	procedure reset_grid_and_cursor (
		self : not null access type_view)
	is begin
		reset_grid_density;
		cursor_main.position := self.snap_to_grid (cursor_main.position);
		self.update_coordinates_display;
	end reset_grid_and_cursor;

	
	procedure set_grid (
		self	: not null access type_view;
		density	: in type_grid_density)
	is begin
		grid_density := density;
		cursor_main.position := self.snap_to_grid (cursor_main.position);
		self.update_coordinates_display;
	end set_grid;

	
	procedure move_cursor (
		self		: not null access type_view;
		coordinates	: in type_coordinates;
		cursor		: in out type_cursor;
		position	: in type_point) 
	is
		use et_project.modules.pac_generic_modules;
	begin
		case coordinates is
			when ABSOLUTE =>
				--cursor.position := type_point (round (position, element (current_active_module).grid));
				cursor.position := type_point (round (position, self.get_grid));
				
			when RELATIVE =>
				--cursor.position := type_point (round (cursor.position + position, element (current_active_module).grid));
				cursor.position := type_point (round (cursor.position + position, self.get_grid));
		end case;

		self.shift_area (cursor);		
	end move_cursor;

	
	procedure move_cursor (
		self		: not null access type_view;
		direction	: in type_cursor_direction;
		cursor		: in out type_cursor)
	is
		-- Get the currently active grid:
		use et_project.modules.pac_generic_modules;
		--grid : constant type_grid := element (current_active_module).grid;
		grid : constant type_grid := self.get_grid;

		-- Find the grid point nearest available to the current cursor position:
		position_snapped : constant type_point := type_point (round (
							point	=> cursor.position,
							grid	=> grid));

	begin
		case direction is
			when RIGHT =>
				cursor.position := type_point (move (position_snapped, 0.0, grid.x, clip => true));

			when LEFT =>
				cursor.position := type_point (move (position_snapped, 180.0, grid.x, clip => true));

			when UP =>
				cursor.position := type_point (move (position_snapped, 90.0, grid.y, clip => true));

			when DOWN =>
				cursor.position := type_point (move (position_snapped, -90.0, grid.y, clip => true));
		end case;
		
		self.shift_area (cursor);
	end move_cursor;

	
	procedure draw_cursor (
		self		: not null access type_view;
		cursor		: in type_cursor)
	is
		lh : type_cursor_line; -- the horizontal line
		lv : type_cursor_line; -- the vertical line

		size : type_distance_positive;
		width : type_view_coordinate;
	begin
		size := cursor_half_size / type_distance_positive (self.scale);
		
		-- set start and end point of horizontal line
		lh.start_point := type_point (set (
			x	=> get_x (cursor.position) - size,
			y	=> get_y (cursor.position)));

		lh.end_point := type_point (set (
			x	=> get_x (cursor.position) + size,
			y	=> get_y (cursor.position)));

		-- set start and end point of vertical line
		lv.start_point := type_point (set (
			x	=> get_x (cursor.position),
			y	=> get_y (cursor.position) + size));

		lv.end_point := type_point (set (
			x	=> get_x (cursor.position),
			y	=> get_y (cursor.position) - size));


		-- The line width is inversely proportional to the scale:
		width := type_view_coordinate (cursor_line_width) / self.scale;
		
		set_line_width (context.cr, width);
		
		set_color_cursor (context.cr);

		draw_line (
			line		=> to_line_fine (lh),
			width		=> type_distance_positive (width));

		draw_line (
			line		=> to_line_fine (lv),
			width		=> type_distance_positive (width));
		
		cairo.stroke (context.cr);		

		exception
			when constraint_error => null;
			--put_line ("Schematic: " & message_border_reached);
			-- CS put in status bar ?
				
	end draw_cursor;

	
	function get_grid (
		self : not null access type_view)
		return type_grid
	is
		-- Get the default grid as defined in the module database:
		g : type_grid := element (current_active_module).grid;
	begin
		-- Scale the grid according to current grid level:
		case grid_density is
			when COARSE => scale_grid (g, grid_density_multiplier_coarse);
			when NORMAL => scale_grid (g, grid_density_multiplier_normal);
			when FINE	=> scale_grid (g, grid_density_multiplier_fine);
		end case;
				
		return g;
	end get_grid;

	
	function get_frame (
		self : not null access type_view)
		return et_frames.type_frame 
	is
		use et_project.modules.pac_generic_modules;
	begin
		return element (current_active_module).frames.frame;
	end get_frame;

	
	function get_frame_height (
		self : not null access type_view)
		return type_float_positive 
	is 
		use et_project.modules.pac_generic_modules;
	begin
		return type_float_positive (
			element (current_active_module).frames.frame.size.y);
	end get_frame_height;

	
	function frame_width (
		self : not null access type_view)
		return type_float_positive 
	is 
		use et_project.modules.pac_generic_modules;
	begin
		return type_float_positive (
			element (current_active_module).frames.frame.size.x);
	end frame_width;

	
	function title_block_position (
		self : not null access type_view)
		return et_frames.type_position 
	is begin
		return self.get_frame.title_block_schematic.position;
	end title_block_position;
	
	
	function get_verb (
		self	: not null access type_view)
		return string 
	is begin
		return to_string (verb);
	end get_verb;

	
	function get_noun (
		self	: not null access type_view)
		return string is
	begin
		return to_string (noun);
	end get_noun;
	

	-- Builds a live net route. This procedure requires to be called twice:
	-- first time for the start and the second time for the end point of the route.
	-- The current bend style in global variable "net_route" is taken into account.
	-- The route may be started and finished with different tools. For example start
	-- with MOUSE and finish with KEYBOARD or vice versa.
	procedure make_net_route (
		self	: not null access type_view;
		tool	: in type_tool;
		point	: in type_point)
	is begin
		-- Set the tool being used for this net so that procedure
		-- draw_net_segment_being_drawn knows where to get the end point from.
		route.path.tool := tool;

		if not route.path.being_drawn then

			route.path.start_point := point;
					
			-- Before processing the start point further, it must be validated:
			if valid_for_net_segment (route.path.start_point, log_threshold + 3) then

				route.path.being_drawn := true;
				
				set_status (status_start_point & to_string (route.path.start_point) & ". " &
					status_press_space & status_set_end_point & status_hint_for_abort);
			end if;

		else
			-- set end point
			if route.path.bended = NO then
				
				route.path.end_point := point;

				-- Before processing the end point further, it must be validated:
				if valid_for_net_segment (route.path.end_point, log_threshold + 3) then

					insert_net_segment (
						module			=> current_active_module,
						sheet			=> current_active_sheet,
						net_name_given	=> route.name, -- RESET_N, or empty
						segment			=> (
								start_point	=> route.path.start_point,
								end_point	=> route.path.end_point,
								others		=> <>), -- no labels and no ports, just a bare segment
						log_threshold	=>	log_threshold + 1);

					reset_net_route;
				end if;

			else
				-- Before processing the BEND point further, it must be validated:
				if valid_for_net_segment (route.path.bend_point, log_threshold + 3) then

					insert_net_segment (
						module			=> current_active_module,
						sheet			=> current_active_sheet,
						net_name_given	=> route.name, -- RESET_N, or empty
						segment			=> (
								start_point	=> route.path.start_point,
								end_point	=> route.path.bend_point,
								others		=> <>), -- no labels and no ports, just a bare segment
						log_threshold	=>	log_threshold + 1);

					-- END POINT:
					route.path.end_point := point;

					-- Before processing the END point further, it must be validated:
					if valid_for_net_segment (route.path.end_point, log_threshold + 3) then
					
						insert_net_segment (
							module			=> current_active_module,
							sheet			=> current_active_sheet,
							net_name_given	=> route.name, -- RESET_N, or empty
							segment			=> (
									start_point	=> route.path.bend_point,
									end_point	=> route.path.end_point,
									others		=> <>), -- no labels and no ports, just a bare segment
							log_threshold	=>	log_threshold + 1);
					
						reset_net_route;
					end if;
				end if;

			end if;
		end if;
	end make_net_route;

	
	procedure reset_selections is begin

		-- Verb and noun remain as they are
		-- so that the mode is unchanged.
		
		reset_request_clarification;
		
		reset_net_route; -- after drawing a net route
		reset_segment; -- after move/drag
		reset_segments_being_dragged; -- after dragging a unit
		reset_unit_move; -- after moving/dragging a unit
		reset_unit_add; -- after adding a device
		
		reset_label; -- after placing a label
		
		reset_placeholder; -- after moving a placeholder

		reset_single_cmd_status;
		
		reset_activate_counter;
	end reset_selections;

	
	procedure clear_proposed_objects is begin
		clear_proposed_units;
		clear_proposed_segments;
	end clear_proposed_objects;

	
	procedure key_pressed (
		self	: not null access type_view;
		key		: in gdk_key_type) 
	is separate;

	
	overriding procedure mouse_moved (
		self	: not null access type_view;
		point	: in type_point) 
	is separate;

	
	overriding procedure button_pressed (
		self	: not null access type_view;
		button	: in type_mouse_button;
		point	: in type_point) 
	is separate;

	
	
	procedure reset_properties_selection (
		self : not null access type_view)
	is 
	begin
		--put_line ("reset selection");
		clear_proposed_units;
		clear_proposed_segments;
		
		-- CS reset other stuff ?
	end reset_properties_selection;

	
	procedure save_module is
		use ada.directories;
		use et_project;

		-- Backup the current directory (like /home/user/et/blood_sample_analyzer):
		cur_dir_bak : constant string := current_directory;
	begin
		-- NOTE: We are not in the project directory yet but
		-- in its parent directory.
		
		-- Change into the directory of the current project:
		set_directory (to_string (current_active_project));
		
		-- Save the module with its own name:
		save_module (
			module_cursor	=> current_active_module,
			log_threshold	=> log_threshold + 1);

		-- Return to previous directory:
		set_directory (cur_dir_bak);
		
		-- Show a brief message in the schematic status bar:
		set_status (status_text_module_saved);

		-- Show a brief message in the board status bar:
		et_canvas_board.pac_canvas.set_status (status_text_module_saved);
	end save_module;

	
	procedure save_drawing (
		self : not null access type_view)
	is begin
		save_module;
	end save_drawing;

	
end et_canvas_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
