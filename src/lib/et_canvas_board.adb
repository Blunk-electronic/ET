------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
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
with ada.containers;

with gtk.window;					use gtk.window;


with et_scripting;
with et_modes;
with et_project;

with et_assembly_variants;			use et_assembly_variants;
with et_canvas_schematic;			use et_canvas_schematic;
with et_display.board;
with et_colors.board;				use et_colors.board;
with et_modes.board;				use et_modes.board;
with et_board_ops;					use et_board_ops;
with et_pcb;
with et_pcb_stack;
with et_design_rules;
with et_text;
with et_meta;
with et_ratsnest;					--use et_ratsnest;
with et_exceptions;					use et_exceptions;

package body et_canvas_board is

	use et_project.modules.pac_generic_modules;

	
	-- This procedure should be called each time after the current active module 
	-- changes. 
	-- It removes all property bars (if being displayed) and
	-- calls other procedures that initialize the values used in property
	-- bars for vias, tracks, ...
	procedure init_property_bars is begin
		reset_preliminary_via;
		init_preliminary_via;

		--  CS init route
		-- CS init text
		-- ...
	end init_property_bars;


	
	procedure set_title_bar (
		-- CS project name
		module		: in pac_module_name.bounded_string)
	is begin
		window.set_title (title & to_string (module));
	end set_title_bar;

	
	procedure set_label_console is
		text_before : constant string := label_console.get_text;
	begin
		label_console.set_text (text_before & et_canvas_schematic.label_console_text);
	end set_label_console;
	

	procedure redraw_board is begin
		redraw (canvas);
	end redraw_board;

	
	procedure redraw_schematic is begin
		et_canvas_schematic.redraw_schematic;
	end redraw_schematic;
	

	procedure redraw is begin
		redraw_schematic;
		redraw_board;
	end redraw;


	
	function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_model_point)	
		return type_point 
	is 
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> type_distance (
						model_point.x 
						- self.frame_bounding_box.x
						- self.board_origin.x) -- because board origin is not the same as drawing origin
			);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (
						self.get_frame_height
						- model_point.y 
						+ self.frame_bounding_box.y
						- self.board_origin.y)  -- because board origin is not the same as drawing origin
			);

		return p;
	end;

	
	function drawing_to_model (
		self			: not null access type_view;
		drawing_point : in type_point)	
		return type_model_point 
	is 
		p : type_model_point; -- to be returned
	begin
		p.x := type_float (get_x (drawing_point))
				+ self.frame_bounding_box.x
				+ type_float (self.board_origin.x);

		p.y := self.get_frame_height
				- type_float (get_y (drawing_point))
			   + self.frame_bounding_box.y
			   - self.board_origin.y;
		
		return p;
	end;


	procedure execute_script (script : in pac_script_name.bounded_string) is
		use ada.directories;
		use et_scripting;
		use et_modes;
		use et_project;

		-- We assemble a command that executes a script
		-- like "board motor_driver execute script my_script.scr:
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_BOARD)) & space &
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
		
		-- execute the board command
		board_cmd (current_active_module, cmd, log_threshold);

		-- Return to previous directory (like  /home/user/my_projects):
		set_directory (cur_dir_bak);
		
		-- The majority of commands requires refreshing the schematic and board drawing.
		
		-- refresh board and schematic
		redraw;
		--redraw (canvas);
		--et_canvas_schematic.pac_canvas.redraw (et_canvas_schematic.pac_canvas.canvas);
		
		-- CS output error message in gui

		log_indentation_down;

	exception when event: others =>
		
		-- Return to previous directory (like  /home/user/my_projects):
		set_directory (cur_dir_bak);

		log_indentation_down;
	end execute_script;

	
	procedure execute_command (self : access gtk_entry_record'class) is 
		use ada.directories;
		use et_string_processing;
		use et_scripting;
		use et_modes;
		use et_project;
		
		-- The operator enters a command like "rename device R1 R2".
		-- The operator is not required to type domain and module name.
		-- Since we are editing a board, the domain and module name itelf
		-- are known. By prepending domain and module name here the full 
		-- command after this declaration will be "board led_driver rename device R1 R2".		
		line_as_typed_by_operator : constant string := 
			to_lower (to_string (DOM_BOARD)) & space &
			to_string (et_canvas_schematic.active_module) & space &
			get_text (self);
		
		cmd : et_string_processing.type_fields_of_line;

		-- The command might launch a script. To prepare for this case we must change
		-- into the project directory. The current directory is the parent directory
		-- of the active project. 
		-- Example: The curreent directory is /home/user/my_projects . The directory
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
			number			=> 1, -- this is the one and only line
			comment_mark 	=> et_scripting.comment_mark,
			delimiter_wrap	=> true, -- strings are enclosed in quotations
			ifs 			=> space); -- fields are separated by space

		--log (text => "full command " & enclose_in_quotes (to_string (cmd)), level => log_threshold + 1);

		log (text => "changing to directory " &
				enclose_in_quotes (to_string (current_active_project)) & " ...",
			level => log_threshold + 1);
		
		set_directory (to_string (current_active_project));
		
		-- execute the board command
		board_cmd (current_active_module, cmd, log_threshold);
		
		-- Return to previous directory (like  /home/user/my_projects):
		log (text => "returning to directory " & enclose_in_quotes (cur_dir_bak) & " ...",
			level => log_threshold + 1);

		set_directory (cur_dir_bak);
		
		-- The majority of commands requires refreshing the schematic and board drawing.

		-- refresh board and schematic
		redraw;
		--redraw (canvas);
		--et_canvas_schematic.pac_canvas.redraw (et_canvas_schematic.pac_canvas.canvas);

		-- CS output error message in gui

		log_indentation_down;

	exception when event: others =>

		-- Return to previous directory (like  /home/user/my_projects):
		log (text => "returning to directory " & enclose_in_quotes (cur_dir_bak) & " ...",
			 level => log_threshold + 1);

		set_directory (cur_dir_bak);

		log_indentation_down;
	end execute_command;

	

	
	function active_module (self : not null access type_view) 
		return string
	is begin
		return to_string (key (current_active_module)); -- motor_driver (without extension)
	end active_module;


	
	function bounding_box (self : not null access type_view)
		return type_bounding_box is
	begin
		return self.paper_bounding_box; -- CS should include all items of the current sheet.
		-- means: also items outside the frame
	end;


	procedure set_grid_x (self : access gtk_entry_record'class) is
		use et_board_ops;
		-- get the current grid
		grid : type_grid := element (current_active_module).board.grid;
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
		use et_board_ops;
		
		-- get the current grid		
		grid : type_grid := element (current_active_module).board.grid;
	begin
		-- Assign grid to y axis:
		grid.y := to_distance (get_text (self));
		
		-- Show the grid:
		gtk_entry (grid_y.get_child).set_text (trim (to_string (grid.y), left));

		-- Finally set the grid in the module database:
		set_grid (current_active_module, grid, log_threshold + 1);
		redraw (canvas);
	end set_grid_y;


	
	procedure gtk_new (
		self	: out type_view_ptr) is
	begin
		self := new type_view;
		init (self);
	end;

	
	-- Draw the origin (a small +) of a text or a text placeholder.
	-- NOTE: This text or text placeholder is not related to the package of a device.
	-- It is a general text like "L2", "TOP", "BOTTOM", "REV 123", "ABC-Systems", ...
	procedure draw_text_origin (
		self    : not null access type_view;
		p		: in type_position) -- the position of the origin
	is		
		use pac_text_board;

		line_horizontal : constant type_line_fine := ( -- from left to right
			start_point		=> to_vector (set (x => get_x (p) - origin_half_size, y => get_y (p))),
			end_point		=> to_vector (set (x => get_x (p) + origin_half_size, y => get_y (p))));

		line_vertical : constant type_line_fine := ( -- from bottom to top
			start_point		=> to_vector (set (x => get_x (p), y => get_y (p) - origin_half_size)),
			end_point		=> to_vector (set (x => get_x (p), y => get_y (p) + origin_half_size)));

	begin -- draw_text_origin
		-- CS if text_origins_enabled then
		
			set_line_width (context.cr, type_view_coordinate (origin_line_width));
		
			draw_line (line_horizontal, origin_line_width);			
			draw_line (line_vertical, origin_line_width);

		--end if;
	end draw_text_origin;


	
	procedure draw_grid (
		self    : not null access type_view;
		area    : type_bounding_box) is separate;

	
	procedure draw_frame (
		self    : not null access type_view)
		is separate;

	
	-- This procedure draws the text that is being placed in a
	-- paired layer. This is about non-conductor layers.
	-- The properties are taken from variable et_canvas_board_texts.text_place.
	-- The verb must be VERB_PLACE and the noun must be NOUN_TEXT. Otherwise
	-- nothing happens here:
	procedure draw_text_being_placed (
		self    	: not null access type_view;
		face		: in type_face;
		category	: in type_layer_category_non_conductor)
	is 
		v_text : type_vector_text;
		
		-- The place where the text shall be placed:
		point : type_point;

		-- The place where the text origin will be drawn:
		origin : type_position;
	begin
		if verb = VERB_PLACE and noun = NOUN_TEXT and text_place.being_moved then

			if text_place.category = category and text_place.face = face then

				-- Set the point where the text is to be drawn:
				point := self.tool_position;

				-- Draw the origin of the text:
				origin := type_position (to_position (point, zero_rotation));
				draw_text_origin (self, origin);

				-- Set the line width of the vector text:
				set_line_width (context.cr, type_view_coordinate (text_place.text.line_width));

				-- Vectorize the text on the fly:
				v_text := vectorize_text (
					content		=> text_place.text.content,
					size		=> text_place.text.size,
					rotation	=> get_rotation (text_place.text.position),
					position	=> point,
					mirror		=> face_to_mirror (face),
					line_width	=> text_place.text.line_width,
					alignment	=> text_place.text.alignment -- right, bottom
					);

				-- Draw the text:
				draw_vector_text (v_text, text_place.text.line_width);

			end if;
		end if;
	end draw_text_being_placed;
	
	
	procedure draw_outline (
		self    : not null access type_view)
		is separate;

	
	procedure draw_silk_screen (
		self    : not null access type_view;
		face	: in type_face) is separate;

	
	procedure draw_assy_doc (
		self    : not null access type_view;
		face	: in type_face) is separate;

	
	procedure draw_stop (
		self    : not null access type_view;
		face	: in type_face) is separate;

	
	procedure draw_stencil (
		self    : not null access type_view;
		face	: in type_face) is separate;

	
	procedure draw_keepout (
		self    : not null access type_view;
		face	: in type_face) is separate;

	
	procedure draw_route_restrict (
		self    : not null access type_view)
		is separate;

	
	procedure draw_via_restrict (
		self    : not null access type_view)
		is separate;

	
	-- Draws objects in conductor layers (incl. vias):
	procedure draw_conductors (
		self    : not null access type_view)
		is separate;

	
	procedure draw_packages (
		self    : not null access type_view;
		face	: in type_face) is separate;

	
	procedure draw_internal (
		self    : not null access type_view;
		area_in	: type_bounding_box) 
	is		
		use et_display.board;

		
		procedure draw_packages is begin
			draw_packages (self, BOTTOM);
			draw_packages (self, TOP);
		end draw_packages;

		
		procedure draw_silkscreen is begin
			if silkscreen_enabled (BOTTOM) then
				draw_silk_screen (self, BOTTOM);
			end if;

			if silkscreen_enabled (TOP) then
				draw_silk_screen (self, TOP);
			end if;
		end draw_silkscreen;

		
		procedure draw_assy_doc is begin
			if assy_doc_enabled (BOTTOM) then
				draw_assy_doc (self, BOTTOM);
			end if;

			if assy_doc_enabled (TOP) then
				draw_assy_doc (self, TOP);
			end if;
		end draw_assy_doc;

		
		procedure draw_keepout is begin
			if keepout_enabled (BOTTOM) then
				draw_keepout (self, BOTTOM);
			end if;

			if keepout_enabled (TOP) then
				draw_keepout (self, TOP);
			end if;
		end draw_keepout;

		
		procedure draw_stop_mask is begin
			if stop_mask_enabled (BOTTOM) then
				draw_stop (self, BOTTOM);
			end if;

			if stop_mask_enabled (TOP) then
				draw_stop (self, TOP);
			end if;
		end draw_stop_mask;

		
		procedure draw_stencil is begin
			if stencil_enabled (BOTTOM) then
				draw_stencil (self, BOTTOM);
			end if;

			if stencil_enabled (TOP) then
				draw_stencil (self, TOP);
			end if;
		end draw_stencil;

		
		procedure draw_pcb_outline is begin
			if outline_enabled then		
				draw_outline (self);
			end if;
		end draw_pcb_outline;

		
		procedure draw_conductor_layers is begin
			draw_route_restrict (self);
			draw_via_restrict (self);
			
			draw_conductors (self);

			-- CS unrouted
		end draw_conductor_layers;

		
		procedure draw_board is begin
			draw_conductor_layers;
			draw_packages;
			draw_silkscreen;
			draw_assy_doc;
			draw_keepout;
			draw_stop_mask;
			draw_stencil;
			draw_pcb_outline;
			
			-- CS draw_submodules			
		end draw_board;
		
		offset : type_offset;

		
	begin -- draw_internal
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


		
-- 		put_line ("draw internal ...");
		
		set_color_background (context.cr, et_colors.no_opacity);
		paint (context.cr);

		-- Backup context for drawing the grid at the end of this procedure.
		save (context.cr);
		
		-- move area according to frame position:
		move_by (area, offset);

		
		-- draw the frame:
		save (context.cr);
		
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			convert_x (self.frame_bounding_box.x),
			convert_y (self.frame_bounding_box.y));

		draw_frame (self);
		restore (context.cr);

		
		-- move area according to board position:
		--move_by (area, to_distance_relative (invert (self.board_origin, X)));
		move_by (area, to_offset (invert (self.board_origin, X)));
		
		save (context.cr);
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		
		-- The drawing must further-on be shifted to the right and up by the board position
		-- so that the board origin is not at the lower left corner of the frame.
		-- The board origin is now somewhere inside the frame.
		translate (
			context.cr,
			convert_x (self.frame_bounding_box.x + self.board_origin.x),
			convert_y (self.frame_bounding_box.y - self.board_origin.y));


		-- draw packages, tracks, vias, silkscreen, pcb outline, ...
		draw_board;

		
		-- Grid and cursor is drawn last so that they
		-- are visible regardless of areas drawn with the 
		-- cairo CLEAR operator:
		
		draw_cursor (self, cursor_main);
		restore (context.cr);

		-- Restore context to draw the grid:
		restore (context.cr);

		if grid_enabled then
			draw_grid (self, area_in);
		end if;
		
	end draw_internal;

	
	procedure move_cursor (
		self		: not null access type_view;
		coordinates	: in type_coordinates;
		cursor		: in out type_cursor;
		position	: in type_point) 
	is begin
		case coordinates is
			when ABSOLUTE =>
				--cursor.position := type_point (round (position, element (current_active_module).board.grid));
				cursor.position := type_point (round (position, self.get_grid));
				
			when RELATIVE =>
				--cursor.position := type_point (round (cursor.position + position, element (current_active_module).board.grid));
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
		--grid : constant type_grid := element (current_active_module).board.grid;
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
				--put_line ("Layout: " & message_border_reached);
			-- CS put in status bar ?
			
	end draw_cursor;

	
	function get_grid (
		self : not null access type_view)
		return type_grid 
	is
		-- Get the default grid as defined in the module database:
		g : type_grid := element (current_active_module).board.grid;
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
	is begin
		return element (current_active_module).board.frame.frame;
	end get_frame;

	
	function get_frame_height (
		self : not null access type_view)
		return type_float_positive 
	is begin
		return type_float_positive (
			element (current_active_module).board.frame.frame.size.y);
	end get_frame_height;

	
	function frame_width (
		self : not null access type_view)
		return type_float_positive 
	is begin
		return type_float_positive (
			element (current_active_module).board.frame.frame.size.x);
	end frame_width;

	
	function title_block_position (
		self : not null access type_view)
		return et_frames.type_position 
	is begin
		return self.get_frame.title_block_pcb.position;
	end title_block_position;

	
	function board_origin (
		self : not null access type_view)
		return type_model_point 
	is begin
		return to_model_point (element (current_active_module).board.origin);
	end board_origin;

	
	function get_verb (
		self	: not null access type_view)
		return string is
	begin
		return to_string (verb);
	end get_verb;

	
	function get_noun (
		self	: not null access type_view)
		return string is
	begin
		return to_string (noun);
	end get_noun;

	
	procedure key_pressed (
		self	: not null access type_view;
		key		: in gdk_key_type) 
	is separate;

	
	procedure mouse_moved (
		self	: not null access type_view;
		point	: in type_point) 
	is separate;	

	
	procedure button_pressed (
		self	: not null access type_view;
		button	: in type_mouse_button;
		point	: in type_point) 
	is separate;

	
	procedure save_drawing (
		self : not null access type_view)
	is begin
		save_module;
	end save_drawing;

	
end et_canvas_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
