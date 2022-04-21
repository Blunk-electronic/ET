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
						self.frame_height
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
		p.x := type_float_internal (get_x (drawing_point)) 
			+ self.frame_bounding_box.x;
		
		p.y := self.frame_height 
			- type_float_internal (get_y (drawing_point))
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
		return type_rectangle is
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
		context : type_draw_context;
		area    : type_rectangle)	is separate;

	
	procedure draw_frame (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	
	procedure draw_tag_label (
		self	: not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context;
		net		: in pac_net_name.bounded_string;
		label	: in type_net_label) is separate;

	
	procedure draw_nets (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	
	procedure draw_texts (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	
	-- Draws a single symbol of the given device:
	procedure draw_symbol (
		self			: not null access type_view;
		in_area			: in type_rectangle := no_rectangle;
		context 		: in type_draw_context;
		symbol			: in et_symbols.type_symbol;
		device_name		: in et_devices.type_device_name := (others => <>);
		device_value	: in pac_device_value.bounded_string := to_value (""); -- like 100R or TL084
		device_purpose	: in pac_device_purpose.bounded_string := to_purpose (""); -- like "brightness control"
		unit_name		: in et_devices.pac_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...
		unit_count		: in et_devices.type_unit_count;
		unit_position	: in type_point; -- x/y on the schematic sheet
		unit_rotation	: in type_rotation := zero_rotation;
		sch_placeholder_name	: in et_symbols.type_text_placeholder;
		sch_placeholder_value	: in et_symbols.type_text_placeholder;
		sch_placeholder_purpose : in et_symbols.type_text_placeholder;
		brightness		: in type_brightness := NORMAL;

		-- If preview is true, then the unit will be drawn less detailled.
		preview			: in boolean := false)
		is separate;

		
	-- Draws all units:
	procedure draw_units (
		self	: not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;


	procedure draw_net_route_being_drawn (
		self	: not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context)
	is
		use et_nets;
		line : type_line;

		procedure compute_route (s, e : in type_point) is 

			-- Do the actual route calculation.
			r : type_route := to_route (s, e, route.path.bend_style);

			procedure draw is begin
				-- draw the net segment:
				draw_line (
					area		=> in_area,
					context		=> context,
					line		=> line,
					width		=> net_line_width,
					height		=> type_float_internal_positive (self.frame_height));
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
			-- evaluate_key or button_pressed.
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
		self	: not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	
	procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_rectangle) 
	is
		-- The given area must be shifted (left and up) by the position
		-- of the drawing frame. This is required for all objects in the 
		-- drawing frame.
		-- Take a copy of the given area:
		area_shifted : type_rectangle := area;

		-- Calculate the new position of area_shifted:
		area_shifted_new_position : constant type_offset := to_offset (
			x => - self.frame_bounding_box.x,
			y => - self.frame_bounding_box.y);
		
		use et_display.schematic;
	begin
-- 		put_line ("draw internal ...");
-- 		shift_area (self, area_shifted, cursor_main);
-- 		shift_area (self, area_shifted_new_position, cursor_main);
		
		set_color_background (context.cr);
		paint (context.cr);

		if grid_enabled then
			draw_grid (self, context, area);
		end if;
		
		-- move area_shifted
		move_by (area_shifted, area_shifted_new_position);

		save (context.cr);
			
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			convert_x (self.frame_bounding_box.x),
			convert_y (self.frame_bounding_box.y));

		
		draw_units (self, area_shifted, context);
		
		draw_frame (self, area_shifted, context);
		
		-- Draw nets if layer is enabled:
		if nets_enabled then
			draw_nets (self, area_shifted, context);
		end if;

		-- Draw texts if layer is enabled:
		if texts_enabled then
			draw_texts (self, area_shifted, context);
		end if;
		
		draw_submodules (self, area_shifted, context);

		draw_net_route_being_drawn (self, area_shifted, context);
		
		draw_cursor (self, area_shifted, context, cursor_main);
		
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

		update_coordinates_display (self);
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
		
		update_coordinates_display (self);
		self.shift_area (cursor);
	end move_cursor;

	
	procedure draw_cursor (
		self		: not null access type_view;
		in_area		: in type_rectangle := no_rectangle;
		context 	: in type_draw_context;
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
			area		=> in_area,
			context		=> context,
			line		=> lh,
			width		=> type_distance_positive (width),
			height		=> type_float_internal_positive (self.frame_height));

		draw_line (
			area		=> in_area,
			context		=> context,
			line		=> lv,
			width		=> type_distance_positive (width),
			height		=> type_float_internal_positive (self.frame_height));
		
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

	
	function frame_height (
		self : not null access type_view)
		return type_float_internal_positive 
	is 
		use et_project.modules.pac_generic_modules;
	begin
		return type_float_internal_positive (
			element (current_active_module).frames.frame.size.y);
	end frame_height;

	
	function frame_width (
		self : not null access type_view)
		return type_distance_positive is 

		use et_project.modules.pac_generic_modules;
	begin
		return type_distance_positive (element (current_active_module).frames.frame.size.x);
	end frame_width;
	
	function title_block_position (
		self : not null access type_view)
		return et_frames.type_position is
	begin
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
	
	procedure evaluate_key (
		self	: not null access type_view;
		key		: in gdk_key_type) 
	is
		use gdk.types;
		use gdk.types.keysyms;

		use et_modes;

		procedure delete is begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_l =>
					noun := NOUN_LABEL;
					set_status (et_canvas_schematic_nets.status_delete_label);
				
				when GDK_LC_u =>
					noun := NOUN_UNIT;					
					set_status (et_canvas_schematic_units.status_delete);
					
				when GDK_LC_n =>
					noun := NOUN_NET;					
					set_status (et_canvas_schematic_nets.status_delete);

				-- If space pressed, then the operator wishes to operate via keyboard:	
				when GDK_Space =>
					case noun is
						when NOUN_LABEL =>
							if not clarification_pending then
								delete_label (cursor_main.position);
							else
								delete_selected_label;
							end if;
						
						when NOUN_NET => 
							if not clarification_pending then
								delete_net_segment (cursor_main.position);
							else
								delete_selected_net_segment;
							end if;

						when NOUN_UNIT =>
							if not clarification_pending then
								delete_unit (cursor_main.position);
							else
								delete_selected_unit;
							end if;
							
						when others => null;							
					end case;

				-- If page down pressed, then the operator is clarifying:
				when GDK_page_down =>
					case noun is
						when NOUN_LABEL => 
							if clarification_pending then
								clarify_label;
							end if;

						when NOUN_NET => 
							if clarification_pending then
								clarify_net_segment;
							end if;

						when NOUN_UNIT =>
							if clarification_pending then
								clarify_unit;
							end if;

						when others =>
							null;
							
					end case;
					
				when others => status_noun_invalid;
			end case;
		end delete;

		procedure drag is begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_n =>
					noun := NOUN_NET;
					set_status (et_canvas_schematic_nets.status_drag);

					-- When dragging net segments, we enforce the default grid
					-- and snap the cursor position to the default grid:
					self.reset_grid_and_cursor;
					
				when GDK_LC_u =>
					noun := NOUN_UNIT;
					set_status (et_canvas_schematic_units.status_drag);

					-- When dragging units, we enforce the default grid
					-- and snap the cursor position to the default grid:
					self.reset_grid_and_cursor;

				-- If space pressed then the operator wishes to operate
				-- by keyboard:
				when GDK_Space =>
		
					case noun is
						when NOUN_NET =>
							if not segment.being_moved then

								-- When dragging net segments, we enforce the default grid
								-- and snap the cursor position to the default grid:
								self.reset_grid_and_cursor;
								
								-- Set the tool being used for moving the segment:
								segment.tool := KEYBOARD;
								
								if not clarification_pending then
									find_segments (cursor_main.position);
									segment.point_of_attack := cursor_main.position;
								else
									segment.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the cursor position to the
								-- currently selected segment:
								et_canvas_schematic_nets.finalize_drag (
									destination		=> cursor_main.position,
									log_threshold	=> log_threshold + 1);

							end if;

						when NOUN_UNIT =>
							if not unit_move.being_moved then

								-- When dragging units, we enforce the default grid
								-- and snap the cursor position to the default grid:
								self.reset_grid_and_cursor;
								
								-- Set the tool being used for moving the unit:
								unit_move.tool := KEYBOARD;
								
								if not clarification_pending then
									find_units_for_move (cursor_main.position);
								else
									find_attached_segments;
									unit_move.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the cursor position to the
								-- currently selected unit:
								et_canvas_schematic_units.finalize_drag (
									destination		=> cursor_main.position,
									log_threshold	=> log_threshold + 1);

							end if;

						when others => null;
							
					end case;

				-- If page down pressed, then the operator is clarifying:
				when GDK_page_down =>
					case noun is
						when NOUN_UNIT =>
							if clarification_pending then
								clarify_unit;
							end if;

						when NOUN_NET => 
							if clarification_pending then
								clarify_net_segment;
							end if;

						when others => null;
							
					end case;
					
				when others => status_noun_invalid;
			end case;

		end drag;
		
		procedure draw is begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_n =>
					noun := NOUN_NET;
					
					set_status (status_draw_net);

					-- we start a new route:
					reset_net_route;

					-- When drawing net segments, we enforce the default grid
					-- and snap the cursor position to the default grid:
					self.reset_grid_and_cursor;
				
				-- If space pressed, then the operator wishes to operate via keyboard:
				when GDK_Space =>
					case noun is
						when NOUN_NET =>
							-- When drawing net segments, we enforce the default grid
							-- and snap the cursor position to the default grid:
							self.reset_grid_and_cursor;

							self.make_net_route (KEYBOARD, cursor_main.position);	
							
						when others => null;
					end case;

				-- If B pressed, then a bend style is being selected.
				-- this affects only certain modes and is ignored otherwise:
				when GDK_LC_b =>
					case noun is
						when NOUN_NET =>
							next_bend_style (route.path);
							
						when others => null;
							
					end case;
					
				when others => status_noun_invalid;
			end case;
		end draw;

		procedure move is begin
			case key is
				-- EVALUATE KEY FOR NOUN:
-- 				when GDK_LC_n =>
-- 					noun := NOUN_NET;

					-- CS
					--set_status (et_canvas_schematic_nets.status_move);

				when GDK_LC_l =>
					noun := NOUN_LABEL;
					set_status (et_canvas_schematic_nets.status_move_label);

				when GDK_LC_n =>
					noun := NOUN_NAME;					
					set_status (et_canvas_schematic_units.status_move_placeholder);

				when GDK_LC_p =>
					noun := NOUN_PURPOSE;					
					set_status (et_canvas_schematic_units.status_move_placeholder);
					
				when GDK_LC_u =>
					noun := NOUN_UNIT;
					set_status (et_canvas_schematic_units.status_move);

					-- When moving units, we enforce the default grid
					-- and snap the cursor position to the default grid:
					self.reset_grid_and_cursor;
					
				when GDK_LC_v =>
					noun := NOUN_VALUE;					
					set_status (et_canvas_schematic_units.status_move_placeholder);

					
				-- If space pressed then the operator wishes to operate
				-- by keyboard:
				when GDK_Space =>
		
					case noun is
-- CS
-- 						when NOUN_NET =>
-- 							if not segment.being_moved then
-- 								
-- 								-- Set the tool being used for moving the segment:
-- 								segment.tool := KEYBOARD;
-- 								
-- 								if not clarification_pending then
-- 									find_segments (cursor_main.position);
-- 								else
-- 									segment.being_moved := true;
-- 									reset_request_clarification;
-- 								end if;
-- 								
-- 							else
-- 								-- Finally assign the cursor position to the
-- 								-- currently selected segment:
-- 								et_canvas_schematic_nets.finalize_move (
-- 									destination		=> cursor_main.position,
-- 									log_threshold	=> log_threshold + 1);
-- 
-- 							end if;

						when NOUN_LABEL =>
							if not label.being_moved then

								-- Set the tool being used for moving the label:
								label.tool := KEYBOARD;

								if not clarification_pending then
									-- NOTE: Only simple labels can be moved.
									-- Tag labels are always attached to a stub
									-- and are moved along when the stub is moved.
									find_labels (cursor_main.position, SIMPLE);
								else
									label.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the cursor position to the
								-- currently selected net label:
								finalize_move_label (
									destination		=> cursor_main.position,
									log_threshold	=> log_threshold + 1);

							end if;
						
						when NOUN_NAME =>
							if not placeholder_move.being_moved then

								-- Set the tool being used for moving the placeholder:
								placeholder_move.tool := KEYBOARD;
								placeholder_move.category := et_symbols.NAME;

								if not clarification_pending then
									find_placeholders (
										point		=> cursor_main.position,
										category	=> et_symbols.NAME);
								else
									placeholder_move.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the cursor position to the
								-- currently selected placeholder:
								et_canvas_schematic_units.finalize_move_placeholder (
									destination		=> cursor_main.position,
									category		=> et_symbols.NAME,
									log_threshold	=> log_threshold + 1);

							end if;

							
						when NOUN_PURPOSE =>
							if not placeholder_move.being_moved then

								-- Set the tool being used for moving the placeholder:
								placeholder_move.tool := KEYBOARD;
								placeholder_move.category := et_symbols.PURPOSE;

								if not clarification_pending then
									find_placeholders (
										point		=> cursor_main.position,
										category	=> et_symbols.PURPOSE);
								else
									placeholder_move.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the cursor position to the
								-- currently selected placeholder:
								et_canvas_schematic_units.finalize_move_placeholder (
									destination		=> cursor_main.position,
									category		=> et_symbols.PURPOSE,
									log_threshold	=> log_threshold + 1);

							end if;

								
						when NOUN_UNIT =>
							if not unit_move.being_moved then

								-- When moving units, we enforce the default grid
								-- and snap the cursor position to the default grid:
								self.reset_grid_and_cursor;
								
								-- Set the tool being used for moving the unit:
								unit_move.tool := KEYBOARD;
								
								if not clarification_pending then
									find_units_for_move (cursor_main.position);
								else
									unit_move.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the cursor position to the
								-- currently selected unit:
								et_canvas_schematic_units.finalize_move (
									destination		=> cursor_main.position,
									log_threshold	=> log_threshold + 1);

							end if;


						when NOUN_VALUE =>
							if not placeholder_move.being_moved then

								-- Set the tool being used for moving the placeholder:
								placeholder_move.tool := KEYBOARD;
								placeholder_move.category := et_symbols.VALUE;

								if not clarification_pending then
									find_placeholders (
										point		=> cursor_main.position,
										category	=> et_symbols.VALUE);
								else
									placeholder_move.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the cursor position to the
								-- currently selected placeholder:
								et_canvas_schematic_units.finalize_move_placeholder (
									destination		=> cursor_main.position,
									category		=> et_symbols.VALUE,
									log_threshold	=> log_threshold + 1);

							end if;

							
						when others => null;
							
					end case;

				-- If page down pressed, then the operator is clarifying:
				when GDK_page_down =>

					case noun is

						when NOUN_LABEL => 
							if clarification_pending then
								clarify_label;
							end if;
						
						when NOUN_NAME => 
							if clarification_pending then
								clarify_placeholder;
							end if;

						when NOUN_PURPOSE => 
							if clarification_pending then
								clarify_placeholder;
							end if;

						when NOUN_UNIT =>
							if clarification_pending then
								clarify_unit;
							end if;
							
						when NOUN_VALUE => 
							if clarification_pending then
								clarify_placeholder;
							end if;

							
						when others => null;
							
					end case;
					
				when others => status_noun_invalid;
			end case;
		end move;

		procedure place is 
			use et_schematic;
		begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_l =>
					noun := NOUN_LABEL;
					label.appearance := SIMPLE;
					set_status (et_canvas_schematic_nets.status_place_label_simple);

					-- For placing simple net labels, the fine grid is required:
					self.set_grid (FINE);
					
				when GDK_L =>
					noun := NOUN_LABEL;
					label.appearance := TAG;
					set_status (et_canvas_schematic_nets.status_place_label_tag);

				-- If space pressed, then the operator wishes to operate via keyboard:	
				when GDK_Space =>
					case noun is

						when NOUN_LABEL =>
							if not label.being_moved then
								
								-- Set the tool being used for placing the label:
								label.tool := KEYBOARD;
								
								if not clarification_pending then
									find_segments (cursor_main.position);
								else
									label.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally place the label at the current
								-- cursor position:
								et_canvas_schematic_nets.finalize_place_label (
									destination		=> cursor_main.position,
									log_threshold	=> log_threshold + 1);
							end if;
							
						when others => null;
							
					end case;

				-- If page down pressed, then the operator is clarifying:
				when GDK_page_down =>
					case noun is

						when NOUN_LABEL => 
							if clarification_pending then
								clarify_net_segment;
							end if;

						when others => null;
							
					end case;

				when GDK_LC_r =>
					case noun is

						when NOUN_LABEL =>
							-- Rotate simple label:
							if label.being_moved then
								toggle_rotation (label.rotation_simple);
							end if;

						when others => null;
							
					end case;
					
				when others => status_noun_invalid;
			end case;
		end place;
		
		procedure rotate is begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_n =>
					noun := NOUN_NAME;					
					set_status (et_canvas_schematic_units.status_rotate_placeholder);

				when GDK_LC_p =>
					noun := NOUN_PURPOSE;					
					set_status (et_canvas_schematic_units.status_rotate_placeholder);

					
				when GDK_LC_u =>
					noun := NOUN_UNIT;					
					set_status (et_canvas_schematic_units.status_rotate);

				when GDK_LC_v =>
					noun := NOUN_VALUE;					
					set_status (et_canvas_schematic_units.status_rotate_placeholder);


				-- If space pressed, then the operator wishes to operate via keyboard:	
				when GDK_Space =>
					case noun is
						when NOUN_NAME =>
							if not clarification_pending then
								rotate_placeholder (
									point		=> cursor_main.position,
									category	=> et_symbols.NAME);
							else
								rotate_selected_placeholder (et_symbols.NAME);
							end if;
							
						when NOUN_PURPOSE =>
							if not clarification_pending then
								rotate_placeholder (
									point		=> cursor_main.position,
									category	=> et_symbols.PURPOSE);
							else
								rotate_selected_placeholder (et_symbols.PURPOSE);
							end if;

						when NOUN_UNIT =>
							if not clarification_pending then
								rotate_unit (cursor_main.position);
							else
								rotate_selected_unit;
							end if;
							
						when NOUN_VALUE =>
							if not clarification_pending then
								rotate_placeholder (
									point		=> cursor_main.position,
									category	=> et_symbols.VALUE);
							else
								rotate_selected_placeholder (et_symbols.VALUE);
							end if;
							
						when others => null;
					end case;

				-- If page down pressed, then the operator is clarifying:
				when GDK_page_down =>
					case noun is
						when NOUN_NAME | NOUN_VALUE | NOUN_PURPOSE => 
							if clarification_pending then
								clarify_placeholder;
							end if;

						when NOUN_UNIT =>
							if clarification_pending then
								clarify_unit;
							end if;

						when others => null;
							
					end case;
					
				when others => status_noun_invalid;
			end case;
		end rotate;

		procedure add is 
			use pac_devices_lib;
		begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_d =>
					noun := NOUN_DEVICE;					
					set_status (et_canvas_schematic_units.status_add);

					-- When adding units, we enforce the default grid
					-- and snap the cursor position to the default grid:
					self.reset_grid_and_cursor;
					
					-- open device model selection
					add_device; 
					
				-- If space pressed, then the operator wishes to operate via keyboard:	
				when GDK_Space =>
					case noun is

						when NOUN_DEVICE =>
							-- When adding units, we enforce the default grid
							-- and snap the cursor position to the default grid:
							self.reset_grid_and_cursor;

							-- If a unit has been selected already, then
							-- the number of "activate" actions must be counted.
							-- The "activate" action in this case is pressing the
							-- "space" key. After the first "activate" the tool
							-- for placing the unit is set. After the second "activate"
							-- the unit is placed at the current cursor position.
							-- If no unit has been selected yet, then the device
							-- model selection dialog opens.
							if unit_add.device /= pac_devices_lib.no_element then -- unit selected

								-- Set the tool being used for placing the unit:
								increment_activate_counter;
								
								case activate_counter is
									when 1 =>
										unit_add.tool := KEYBOARD;

									when 2 =>
										-- Finally place the unit at the current 
										-- cursor position:
										finalize_add_device (cursor_main.position);

									when others => null;
								end case;

							else -- no unit selected yet
								add_device; -- open device model selection
							end if;
							
						when others => null;
							
					end case;

					
				when others => null;
			end case;
		end add;

		procedure invoke is 
			use pac_devices_lib;
		begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_u =>
					noun := NOUN_UNIT;					
					set_status (et_canvas_schematic_units.status_invoke);

				-- If space pressed, then the operator wishes to operate via keyboard:	
				when GDK_Space =>
					case noun is

						when NOUN_UNIT =>
							-- If no device has been selected already, then
							-- set the tool used for invoking.
							if unit_add.device = pac_devices_lib.no_element then

								unit_add.tool := KEYBOARD;

								if not clarification_pending then
									invoke_unit (cursor_main.position);
								else
									show_units;
								end if;

							else
								finalize_invoke (cursor_main.position, log_threshold + 1);
							end if;
							
						when others => null;
							
					end case;

				-- If page down pressed, then the operator is clarifying:
				when GDK_page_down =>
					case noun is

						when NOUN_UNIT => 
							if clarification_pending then
								clarify_unit;
							end if;
							
						when others => null;
							
					end case;
					
				when others => null;
			end case;
		end invoke;

		procedure set is begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_p =>
					noun := NOUN_PARTCODE;
					set_status (et_canvas_schematic_units.status_set_partcode);

				when GDK_LC_u =>
					noun := NOUN_PURPOSE;
					set_status (et_canvas_schematic_units.status_set_purpose);
				
				when GDK_LC_v =>
					noun := NOUN_VALUE;					
					set_status (et_canvas_schematic_units.status_set_value);

				when GDK_LC_a =>
					noun := NOUN_VARIANT;
					set_status (et_canvas_schematic_units.status_set_variant);
					
				-- If space pressed, then the operator wishes to operate via keyboard:	
				when GDK_Space =>
					case noun is
						
						when NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VALUE | NOUN_VARIANT =>
							if not clarification_pending then
								set_property (cursor_main.position);
							else
								set_property_selected_unit;
							end if;
							
						when others => null;
					end case;

				-- If page down pressed, then the operator is clarifying:
				when GDK_page_down =>
					case noun is
						when NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VALUE | NOUN_VARIANT =>
							if clarification_pending then
								clarify_unit;
							end if;

						when others => null;							
					end case;
					
				when others => status_noun_invalid;
			end case;
			
		end set;

		procedure show is begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_d =>
					noun := NOUN_DEVICE;
					set_status (et_canvas_schematic_units.status_show_device);

				when GDK_LC_n =>
					noun := NOUN_NET;
					set_status (et_canvas_schematic_nets.status_show_net);
					
				-- If space pressed, then the operator wishes to operate via keyboard:	
				when GDK_Space =>
					case noun is
						when NOUN_DEVICE =>
							if not clarification_pending then
								find_units_for_show (cursor_main.position);
							else
								show_properties_of_selected_device;
							end if;
							
						when NOUN_NET =>
							if not clarification_pending then
								find_segments (cursor_main.position);
							else
								show_properties_of_selected_net;
							end if;
							
						when others => null;
					end case;

				-- If page down pressed, then the operator is clarifying:
				when GDK_page_down =>
					case noun is
						when NOUN_DEVICE => 
							if clarification_pending then
								clarify_unit;
							end if;

						when NOUN_NET =>
							if clarification_pending then
								clarify_net_segment;
							end if;

						when others => null;
							
					end case;
					
				when others => status_noun_invalid;
			end case;
		end show;
		
		procedure rename is 
			use et_schematic_ops.nets;
		begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_d =>
					noun := NOUN_DEVICE;
					set_status (et_canvas_schematic_units.status_rename);

				when GDK_LC_s => -- rename strand
					noun := NOUN_NET;
					net_rename.scope := STRAND;
					set_status (et_canvas_schematic_nets.status_rename_net_strand);
				
				when GDK_LC_n => -- rename all strands on current sheet
					noun := NOUN_NET;
					net_rename.scope := SHEET;
					set_status (et_canvas_schematic_nets.status_rename_net_sheet);

				when GDK_N => -- rename everywhere: all strands on all sheets
					noun := NOUN_NET;
					net_rename.scope := EVERYWHERE;
					set_status (et_canvas_schematic_nets.status_rename_net_everywhere);
					
				-- If space pressed, then the operator wishes to operate via keyboard:	
				when GDK_Space =>
					case noun is
						when NOUN_DEVICE =>
							if not clarification_pending then
								set_property (cursor_main.position);
							else
								set_property_selected_unit;
							end if;

						when NOUN_NET =>
							if not clarification_pending then
								find_segments (cursor_main.position);
							else
								et_canvas_schematic_nets.window_set_property;
							end if;
							
						when others => null;
					end case;

				-- If page down pressed, then the operator is clarifying:
				when GDK_page_down =>
					case noun is
						when NOUN_DEVICE =>
							if clarification_pending then
								clarify_unit;
							end if;

						when NOUN_NET =>
							if clarification_pending then
								clarify_net_segment;
							end if;
							
						when others => null;							
					end case;
					
				when others => status_noun_invalid;
			end case;

		end rename;
		
	begin -- evaluate_key
		
-- 		put_line ("schematic: evaluating other key ...");
-- 		put_line (gdk_modifier_type'image (key_ctrl));

		case key is
			when GDK_Escape =>
				expect_entry := expect_entry_default;
				reset_selections;
				status_enter_verb;			

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

				-- If an imcomplete command has been entered via console then it starts
				-- waiting for finalization. This can be done by pressing the SPACE key.
				-- Then we call the corresponding subprogram for the actual job right away here:
				
				--if single_cmd_status.finalization_pending and primary_tool = KEYBOARD then
				if single_cmd_status.finalization_pending then
				
					if key = GDK_Space then
							
						case verb is
							when VERB_DELETE	=> delete;
							when VERB_DRAG		=> drag;
							when VERB_DRAW		=> draw;
							when VERB_INVOKE	=> invoke;
							when VERB_MOVE		=> move;
							when VERB_PLACE		=> place;							
							when others			=> null;
						end case;

					end if;
				else
				-- Evaluate the verb and noun (as typed on the keyboard):
					
					case expect_entry is
						when EXP_VERB =>
							--put_line ("VERB entered");

							-- Next we expect an entry to select a noun.
							-- If the verb entry is invalid then expect_entry
							-- will be overwritten by EXP_VERB so that the
							-- operator is required to re-enter a valid verb.
							expect_entry := EXP_NOUN;

							-- As long as no valid noun has been entered
							-- display the default noun:
							noun := noun_default;

							-- EVALUATE KEY FOR VERB:
							case key is
								when GDK_Delete =>
									verb := VERB_DELETE;
									status_enter_noun;

								when GDK_LC_a =>
									verb := VERB_ADD;
									status_enter_noun;
									
								when GDK_LC_g =>
									verb := VERB_DRAG;
									status_enter_noun;

								when GDK_LC_d =>
									verb := VERB_DRAW;
									status_enter_noun;

								when GDK_LC_h =>
									verb := VERB_SHOW;
									status_enter_noun;
									
								when GDK_LC_i =>
									verb := VERB_INVOKE;
									status_enter_noun;
									
								when GDK_LC_m =>
									verb := VERB_MOVE;
									status_enter_noun;

								when GDK_LC_n =>
									verb := VERB_RENAME;
									status_enter_noun;
									
								when GDK_LC_p =>
									verb := VERB_PLACE;
									status_enter_noun;
									
								when GDK_LC_r =>
									verb := VERB_ROTATE;
									status_enter_noun;

								when GDK_LC_s =>
									verb := VERB_SET;
									status_enter_noun;
									
								when others =>
									--put_line ("other key pressed " & gdk_key_type'image (key));
									
									-- If invalid verb entered, overwrite expect_entry by EXP_VERB
									-- and show error in status bar:
									expect_entry := EXP_VERB;
									status_verb_invalid;
							end case;


						when EXP_NOUN =>
							--put_line ("NOUN entered");

							case verb is
								when VERB_ADD		=> add;
								when VERB_DELETE	=> delete;
								when VERB_DRAG		=> drag;
								when VERB_DRAW		=> draw;
								when VERB_INVOKE	=> invoke;
								when VERB_MOVE		=> move;
								when VERB_PLACE		=> place;
								when VERB_RENAME	=> rename;
								when VERB_ROTATE	=> rotate;
								when VERB_SET		=> set;
								when VERB_SHOW		=> show;
								when others => null; -- CS
							end case;
							
					end case;

				end if;		
		end case;

		redraw;
		-- CS use redraw_schematic if only schematic affected
		-- CS redraw after "enter" pressed
		
		update_mode_display (canvas);

		
		exception when event: others =>
			set_status (exception_message (event));

			reset_selections;
		
			redraw;
			update_mode_display (canvas);
		
	end evaluate_key;

	
	overriding procedure evaluate_mouse_position (
		self	: not null access type_view;
		point	: in type_point) 
	is
		use pac_devices_lib;
	begin
		case verb is
			when VERB_ADD =>
				case noun is
					when NOUN_DEVICE =>
						if unit_add.device /= pac_devices_lib.no_element then
							redraw;
						end if;

					when others => null;
				end case;
			
			when VERB_DRAW =>
				case noun is
					when NOUN_NET =>
						if route.path.being_drawn then
							redraw;
						end if;


					when others => null;
				end case;
				
			when VERB_DRAG | VERB_MOVE | VERB_PLACE =>
				case noun is
					when NOUN_LABEL =>
						if label.being_moved then
							redraw_schematic;
						end if;
						
					when NOUN_NAME | NOUN_PURPOSE | NOUN_VALUE => 
						if placeholder_move.being_moved then
							redraw_schematic;
						end if;

					when NOUN_NET =>
						if segment.being_moved then
							redraw_schematic;
						end if;

					when NOUN_UNIT =>
						if unit_move.being_moved then
							redraw_schematic;
						end if;

					when others => null;
				end case;

			when VERB_INVOKE =>
				case noun is
					when NOUN_UNIT =>
						if unit_add.device /= pac_devices_lib.no_element then
							redraw;
						end if;

					when others => null;
				end case;
				
			when others => null;
		end case;
	end evaluate_mouse_position;

	
	overriding procedure button_pressed (
		self	: not null access type_view;
		button	: in type_mouse_button;
		point	: in type_point) 
	is
		procedure left_button is 
			use pac_devices_lib;
		begin
			self.move_cursor (ABSOLUTE, cursor_main, point);

			case verb is
				when VERB_ADD =>
					case noun is
						when NOUN_DEVICE =>

							-- When adding units, we enforce the default grid
							-- and snap the cursor position to the default grid:
							self.reset_grid_and_cursor;
							
							-- If a unit has been selected already, then
							-- the number of "activate" actions must be counted.
							-- The "activate" action in this case is a left click.
							-- After the first "activate" the tool
							-- for placing the unit is set. After the second "activate"
							-- the unit is placed at the current mouse position.
							-- If no unit has been selected yet, then the device
							-- model selection dialog opens.
							if unit_add.device /= pac_devices_lib.no_element then

								increment_activate_counter;
								
								case activate_counter is
									when 1 =>
										unit_add.tool := MOUSE;

									when 2 =>
										-- Finally place the unit at the current 
										-- mouse position:
										finalize_add_device (snap_to_grid (self, point));

									when others => null;
								end case;

							else -- no unit selected yet
								add_device; -- open device model selection
							end if;
								
						when others => null;
								
					end case;
				
				when VERB_DELETE =>
					case noun is
						when NOUN_LABEL =>
							if not clarification_pending then
								delete_label (point);
							else
								delete_selected_label;
							end if;
							
						when NOUN_NET => 
							if not clarification_pending then
								delete_net_segment (point);
							else
								delete_selected_net_segment;
							end if;

						when NOUN_UNIT =>
							if not clarification_pending then
								delete_unit (point);
							else
								delete_selected_unit;
							end if;
							
						when others => null;
					end case;

				when VERB_DRAG =>
					case noun is
						when NOUN_UNIT =>
							if not unit_move.being_moved then

								-- When dragging units, we enforce the default grid
								-- and snap the cursor position to the default grid:
								self.reset_grid_and_cursor;
								
								-- Set the tool being used for moving the unit:
								unit_move.tool := MOUSE;
								
								if not clarification_pending then
									find_units_for_move (point);
								else
									find_attached_segments;
									unit_move.being_moved := true;
									reset_request_clarification;
								end if;

							else
								-- Finally assign the pointer position to the
								-- currently selected unit:
								et_canvas_schematic_units.finalize_drag (
									destination		=> snap_to_grid (self, point),
									log_threshold	=> log_threshold + 1);

							end if;
							
						when NOUN_NET => 
							if not segment.being_moved then

								-- When dragging net segments, we enforce the default grid
								-- and snap the cursor position to the default grid:
								self.reset_grid_and_cursor;
								
								-- Set the tool being used for dragging the net segment:
								segment.tool := MOUSE;
								
								if not clarification_pending then
									find_segments (point);
									segment.point_of_attack := snap_to_grid (self, point);
								else
									segment.being_moved := true;
									reset_request_clarification;
								end if;

							else
								-- Finally assign the cursor position to the
								-- currently selected segment:
								et_canvas_schematic_nets.finalize_drag (
									destination		=> snap_to_grid (self, point),
									log_threshold	=> log_threshold + 1);
							end if;
							
						when others => null;
					end case;
					
				when VERB_DRAW =>
					case noun is
						when NOUN_NET =>
							-- When drawing net segments, we enforce the default grid
							-- and snap the cursor position to the default grid:
							self.reset_grid_and_cursor;

							self.make_net_route (MOUSE, snap_to_grid (self, point));
							
						when others => null;							
					end case;

				when VERB_INVOKE =>
					case noun is

						when NOUN_UNIT =>
							-- If no device has been selected already, then
							-- set the tool used for invoking.
							if unit_add.device = pac_devices_lib.no_element then

								unit_add.tool := MOUSE;

								if not clarification_pending then
									invoke_unit (snap_to_grid (self, point));
								else
									show_units;
								end if;

							else
								finalize_invoke (snap_to_grid (self, point), log_threshold + 1);
							end if;
							
						when others => null;
							
					end case;
					
				when VERB_MOVE =>
					case noun is
						when NOUN_LABEL =>
							if not label.being_moved then

								-- Set the tool being used for moving the label:
								label.tool := MOUSE;

								if not clarification_pending then
									-- NOTE: Only simple labels can be moved.
									-- Tag labels are always attached to a stub
									-- and are moved along when the stub is moved.
									find_labels (point, SIMPLE);
								else
									label.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the mouse position to the
								-- currently selected net label:
								finalize_move_label (
									destination		=> snap_to_grid (self, point),
									log_threshold	=> log_threshold + 1);

							end if;
							
						when NOUN_NAME =>
							if not placeholder_move.being_moved then

								-- Set the tool being used for moving the placeholder:
								placeholder_move.tool := MOUSE;
								placeholder_move.category := et_symbols.NAME;

								if not clarification_pending then
									find_placeholders (
										point		=> point,
										category	=> et_symbols.NAME);
								else
									placeholder_move.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the mouse position to the
								-- currently selected placeholder:
								et_canvas_schematic_units.finalize_move_placeholder (
									destination		=> snap_to_grid (self, point),
									category		=> et_symbols.NAME,
									log_threshold	=> log_threshold + 1);

							end if;

						when NOUN_PURPOSE =>
							if not placeholder_move.being_moved then

								-- Set the tool being used for moving the placeholder:
								placeholder_move.tool := MOUSE;
								placeholder_move.category := et_symbols.PURPOSE;

								if not clarification_pending then
									find_placeholders (
										point		=> point,
										category	=> et_symbols.PURPOSE);
								else
									placeholder_move.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the cursor position to the
								-- currently selected placeholder:
								et_canvas_schematic_units.finalize_move_placeholder (
									destination		=> snap_to_grid (self, point),
									category		=> et_symbols.PURPOSE,
									log_threshold	=> log_threshold + 1);

							end if;
						
						when NOUN_UNIT =>
							if not unit_move.being_moved then

								-- When moving units, we enforce the default grid
								-- and snap the cursor position to the default grid:
								self.reset_grid_and_cursor;
								
								-- Set the tool being used for moving the unit:
								unit_move.tool := MOUSE;
								
								if not clarification_pending then
									find_units_for_move (point);
								else
									unit_move.being_moved := true;
									reset_request_clarification;
								end if;

							else
								-- Finally assign the pointer position to the
								-- currently selected unit:
								et_canvas_schematic_units.finalize_move (
									destination		=> snap_to_grid (self, point),
									log_threshold	=> log_threshold + 1);

							end if;
							
						when NOUN_VALUE =>
							if not placeholder_move.being_moved then

								-- Set the tool being used for moving the placeholder:
								placeholder_move.tool := MOUSE;
								placeholder_move.category := et_symbols.VALUE;

								if not clarification_pending then
									find_placeholders (
										point		=> point,
										category	=> et_symbols.VALUE);
								else
									placeholder_move.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the cursor position to the
								-- currently selected placeholder:
								et_canvas_schematic_units.finalize_move_placeholder (
									destination		=> snap_to_grid (self, point),
									category		=> et_symbols.VALUE,
									log_threshold	=> log_threshold + 1);

							end if;

							
						when others => null;							
					end case;

				when VERB_PLACE =>
					case noun is

						when NOUN_LABEL =>
							if not label.being_moved then
								
								-- Set the tool being used for placing the label:
								label.tool := MOUSE;
								
								if not clarification_pending then
									find_segments (point);
								else
									label.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally place the label at the current 
								-- pointer position:
								et_canvas_schematic_nets.finalize_place_label (
									destination		=> snap_to_grid (self, point),
									log_threshold	=> log_threshold + 1);
							end if;
							
						when others => null;
					end case;

				when VERB_RENAME =>
					case noun is
						when NOUN_DEVICE =>
							if not clarification_pending then
								set_property (point);
							else
								set_property_selected_unit;
							end if;

						when NOUN_NET =>
							if not clarification_pending then
								find_segments (point);
							else
								et_canvas_schematic_nets.window_set_property;
							end if;
							
						when others => null;
					end case;
					
				when VERB_ROTATE =>
					case noun is
						when NOUN_NAME =>
							if not clarification_pending then
								rotate_placeholder (
									point		=> point,
									category	=> et_symbols.NAME);
							else
								rotate_selected_placeholder (et_symbols.NAME);
							end if;
							
						when NOUN_PURPOSE =>
							if not clarification_pending then
								rotate_placeholder (
									point		=> point,
									category	=> et_symbols.PURPOSE);
							else
								rotate_selected_placeholder (et_symbols.PURPOSE);
							end if;
						
						when NOUN_UNIT =>
							if not clarification_pending then
								rotate_unit (point);
							else
								rotate_selected_unit;
							end if;

						when NOUN_VALUE =>
							if not clarification_pending then
								rotate_placeholder (
									point		=> point,
									category	=> et_symbols.VALUE);
							else
								rotate_selected_placeholder (et_symbols.VALUE);
							end if;

							
						when others => null;
					end case;

				when VERB_SET =>
					case noun is
						when NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VALUE | NOUN_VARIANT =>
							if not clarification_pending then
								set_property (point);
							else
								set_property_selected_unit;
							end if;
							
						when others => null;
					end case;

				when VERB_SHOW =>
					case noun is
						when NOUN_DEVICE =>
							if not clarification_pending then
								find_units_for_show (point);
							else
								show_properties_of_selected_device;
							end if;
							
						when NOUN_NET =>
							if not clarification_pending then
								find_segments (point);
							else
								show_properties_of_selected_net;
							end if;
							
						when others => null;
					end case;
					
				when others => null; -- CS
			end case;
			
		end left_button;

		-- If right button clicked, then the operator is clarifying:
		procedure right_button is begin
			case verb is
				when VERB_DELETE =>
					case noun is
						when NOUN_LABEL => 
							if clarification_pending then
								clarify_label;
							end if;
							
						when NOUN_NET => 
							if clarification_pending then
								clarify_net_segment;
							end if;

						when NOUN_UNIT =>
							if clarification_pending then
								clarify_unit;
							end if;
							
						when others => null;							
					end case;

				when VERB_DRAG =>
					case noun is
						when NOUN_UNIT =>
							if clarification_pending then
								clarify_unit;
							end if;

						when NOUN_NET => 
							if clarification_pending then
								clarify_net_segment;
							end if;

						when others => null;							
					end case;
					
				when VERB_DRAW =>
					case noun is
						when NOUN_NET =>
							next_bend_style (route.path);
							
						when others => null;							
					end case;

				when VERB_INVOKE =>
					case noun is

						when NOUN_UNIT => 
							if clarification_pending then
								clarify_unit;
							end if;
							
						when others => null;
							
					end case;
					
				when VERB_MOVE =>
					case noun is

						when NOUN_LABEL => 
							if clarification_pending then
								clarify_label;
							end if;
						
						when NOUN_NAME => 
							if clarification_pending then
								clarify_placeholder;
							end if;

						when NOUN_PURPOSE => 
							if clarification_pending then
								clarify_placeholder;
							end if;

						when NOUN_UNIT =>
							if clarification_pending then
								clarify_unit;
							end if;
							
						when NOUN_VALUE => 
							if clarification_pending then
								clarify_placeholder;
							end if;
							
						when others => null;							
					end case;

				when VERB_PLACE =>
					case noun is
						
						when NOUN_LABEL => 
							if clarification_pending then
								clarify_net_segment;
							end if;

							-- Rotate simple label:
							if label.being_moved then
								toggle_rotation (label.rotation_simple);
							end if;
							
						when others => null;
					end case;

				when VERB_RENAME =>
					case noun is
						when NOUN_DEVICE =>
							if clarification_pending then
								clarify_unit;
							end if;

						when NOUN_NET =>
							if clarification_pending then
								clarify_net_segment;
							end if;

						when others => null;							
					end case;
							
				when VERB_ROTATE =>
					case noun is
						when NOUN_NAME | NOUN_VALUE | NOUN_PURPOSE => 
							if clarification_pending then
								clarify_placeholder;
							end if;

						when NOUN_UNIT =>
							if clarification_pending then
								clarify_unit;
							end if;

						when others => null;							
					end case;

				when VERB_SET =>
					case noun is
						when NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VALUE =>
							if clarification_pending then
								clarify_unit;
							end if;
							
						when others => null;							
					end case;

				when VERB_SHOW =>
					case noun is
						when NOUN_DEVICE =>
							if clarification_pending then
								clarify_unit;
							end if;

						when NOUN_NET =>
							if clarification_pending then
								clarify_net_segment;
							end if;
							
						when others => null;							
					end case;
					
				when others => null; -- CS
			end case;

		end right_button;
			
	begin -- button_pressed
		--log (text => to_string (button) & " at" & to_string (point), level => log_threshold);
		
		case button is
			when 1 => left_button;
			when 3 => right_button;
			when others => null;
		end case;

		redraw;
		-- CS use redraw_schematic if only schematic affected

		
		exception when event: others =>
			set_status (exception_message (event));

			reset_selections;
			redraw;
		
	end button_pressed;

	
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
