------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
with et_exceptions;					use et_exceptions;

package body et_canvas_board is

	use et_project.modules.pac_generic_modules;


	-- This procedure initializes the variable et_canvas_board_vias.via_place
	-- so that the via properties bar shows the user specific settings
	-- or the values as defined in the DRU data set.
	procedure init_via_place is
		use et_pcb;
		use et_canvas_schematic;
		
		use et_design_rules;
		rules : constant type_design_rules := get_pcb_design_rules (current_active_module);

		-- get the user specific settings of the board
		settings : constant type_user_settings := get_user_settings (current_active_module);

		-- We need a list of all net names of the current module:
		use pac_net_names_indexed;
		net_names : constant pac_net_names_indexed.vector := 
			get_indexed_nets (et_canvas_schematic.current_active_module);
		
	begin
		-- Set the drill size and restring according to user specific values:
		-- If user has not specified defaults, use values given in DRU data set:

		-- set drill size:
		if settings.vias.drill.active then
			via_place.drill.diameter	:= settings.vias.drill.size;
		else
			via_place.drill.diameter	:= rules.sizes.drills;
		end if;

		-- set outer restring:
		if settings.vias.restring_outer.active then
			via_place.restring_outer	:= settings.vias.restring_outer.width;
		else
			via_place.restring_outer	:= auto_set_restring (
				OUTER, via_place.drill.diameter);
		end if;
		
		-- set inner restring:
		if settings.vias.restring_inner.active then
			via_place.restring_inner	:= settings.vias.restring_inner.width;
		else
			via_place.restring_inner	:= auto_set_restring (
				INNER, via_place.drill.diameter, rules.sizes.restring.delta_size);
		end if;

		--put_line ("length " & ada.containers.count_type'image (length (net_names)));
		
		--put_line ("name " & to_string (element (net_names.first))
				  --& " idx " & natural'image (to_index (net_names.first)));

		-- If the module contains nets, then set the topmost net in the alphabet.
		-- If there are no nets in the module, then via_place.net
		-- remains un-initalized:
		if is_empty (net_names) then
			set (
				net		=> via_place.net,
				name	=> to_net_name ("")); -- no name
		else
			set (
				net		=> via_place.net,
				name	=> element (net_names.first), -- AGND
				idx		=> to_index (net_names.first)); -- 1
		end if;
		
	end init_via_place;

	-- This procedure should be called each time after the current active module 
	-- changes. 
	-- It removes all property bars (if being displayed) and
	-- calls other procedures that initialize the values used in property
	-- bars for vias, tracks, ...
	procedure init_property_bars is begin
		reset_via_place;
		init_via_place;

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
		model_point : in type_point)	
		return type_point 
	is 
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> get_x (model_point) 
						- self.frame_bounding_box.x
						- get_x (self.board_origin) -- because board origin is not the same as drawing origin
			);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.frame_height) 
						- get_y (model_point) 
						+ self.frame_bounding_box.y
						- get_y (self.board_origin)  -- because board origin is not the same as drawing origin
			);

		return p;
	end;

	function drawing_to_model (
		self			: not null access type_view;
		drawing_point : in type_point)	
		return type_point 
	is 
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> get_x (drawing_point) 
						+ self.frame_bounding_box.x
						+ get_x (self.board_origin) -- because board origin is not the same as drawing origin
			);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.frame_height) 
						- get_y (drawing_point) 
						+ self.frame_bounding_box.y
						- get_y (self.board_origin)  -- because board origin is not the same as drawing origin
			);

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
		board_cmd (et_canvas_schematic.current_active_module, cmd, log_threshold);

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
		board_cmd (et_canvas_schematic.current_active_module, cmd, log_threshold);
		
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
	is
		use et_canvas_schematic;
	begin
		return to_string (key (current_active_module)); -- motor_driver (without extension)
	end active_module;


	
	function bounding_box (self : not null access type_view)
		return type_rectangle is
	begin
		return self.paper_bounding_box; -- CS should include all items of the current sheet.
		-- means: also items outside the frame
	end;


	procedure set_grid_x (self : access gtk_entry_record'class) is
		use et_board_ops;
		use et_canvas_schematic;

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
		use et_canvas_schematic;
		
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
		p		: in type_position; -- the position of the origin
		in_area	: in type_rectangle;
		context	: in type_draw_context) 
	is		
		use et_board_shapes_and_text;
		type type_line is new pac_shapes.type_line with null record;
		
		line_horizontal : constant type_line := ( -- from left to right
			start_point		=> type_point (set (x => get_x (p) - pac_text_fab.origin_half_size, y => get_y (p))),
			end_point		=> type_point (set (x => get_x (p) + pac_text_fab.origin_half_size, y => get_y (p))));

		line_vertical : constant type_line := ( -- from bottom to top
			start_point		=> type_point (set (x => get_x (p), y => get_y (p) - pac_text_fab.origin_half_size)),
			end_point		=> type_point (set (x => get_x (p), y => get_y (p) + pac_text_fab.origin_half_size)));

		use pac_draw_fab;
	begin -- draw_text_origin
		-- CS if text_origins_enabled then
		
			set_line_width (context.cr, type_view_coordinate (pac_text_fab.origin_line_width));
			draw_line (in_area, context, line_horizontal, pac_text_fab.origin_line_width, self.frame_height);
			draw_line (in_area, context, line_vertical, pac_text_fab.origin_line_width, self.frame_height);

		--end if;
	end draw_text_origin;


	
	-- Maps from the meaning of a text to its actutal content.
	function to_placeholder_content (
		meaning : in et_pcb.type_text_meaning)
		return et_text.pac_text_content.bounded_string 
	is
		use et_text;
		use et_meta;
		use et_canvas_schematic;
	
		meta : constant type_board := element (current_active_module).meta.board;

		use pac_assembly_variant_name;
		variant : constant pac_assembly_variant_name.bounded_string := element (current_active_module).active_variant;

		result : pac_text_content.bounded_string;

		use et_pcb;
	begin
		case meaning is
			when COMPANY			=> result := to_content (to_string (meta.company));
			when CUSTOMER			=> result := to_content (to_string (meta.customer));
			when PARTCODE			=> result := to_content (to_string (meta.partcode));
			when DRAWING_NUMBER		=> result := to_content (to_string (meta.drawing_number));
			when ASSEMBLY_VARIANT	=> result := to_content (to_string (variant));
			when PROJECT			=> null; -- CS
			when MODULE				=> result := to_content (to_string (key (current_active_module)));
			when REVISION			=> result := to_content (to_string (meta.revision));
		end case;
		
		return result;
	end to_placeholder_content;

	
	procedure draw_grid (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_rectangle) is separate;

	
	procedure draw_frame (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	
	-- This procedure draws the text that is being placed in a
	-- paired layer. The properties
	-- are taken from variable et_canvas_board_texts.text_place.
	-- The verb must be VERB_PLACE and the noun must be NOUN_TEXT. Otherwise
	-- nothing happens here:
	procedure draw_text_being_placed (
		self    	: not null access type_view;
		in_area		: in type_rectangle := no_rectangle;
		context 	: in type_draw_context;
		face		: in type_face;
		category	: in type_layer_category_non_conductor)
	is 
		use et_board_shapes_and_text;
		use pac_text_fab;
		use et_board_shapes_and_text.pac_text_fab;
		use pac_vector_text_lines;
		vector_text : pac_vector_text_lines.list;

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
				draw_text_origin (self, origin, in_area, context);

				-- Set the line width of the vector text:
				set_line_width (context.cr, type_view_coordinate (text_place.text.line_width));

				-- Vectorize the text:
				vector_text := vectorize_text (
					content		=> text_place.text.content,
					size		=> text_place.text.size,
					rotation	=> rot (text_place.text.position),
					position	=> point,
					mirror		=> face_to_mirror (face),
					line_width	=> text_place.text.line_width,
					alignment	=> text_place.text.alignment -- right, bottom
					);

				-- Draw the text:
				pac_draw_fab.draw_vector_text (in_area, context, vector_text,
					text_place.text.line_width, self.frame_height);

			end if;
		end if;
	end draw_text_being_placed;

	
	-- This procedure draws the text that is being placed in outline/contours.
	-- The properties are taken from variable et_canvas_board_texts.text_place.
	-- The verb must be VERB_PLACE and the noun must be NOUN_TEXT. 
	-- The layer category of text_place must be LAYER_CAT_OUTLINE.
	-- Otherwise nothing happens here:
	procedure draw_text_being_placed_in_outline (
		self    	: not null access type_view;
		in_area		: in type_rectangle := no_rectangle;
		context 	: in type_draw_context)
	is 
		use et_text;
		use et_board_shapes_and_text;
		use et_board_shapes_and_text.pac_text_fab;
		use pac_text_fab.pac_vector_text_lines;
		vector_text : pac_text_fab.pac_vector_text_lines.list;

		-- The place where the text shall be placed:
		point : type_point;

		-- The place where the text origin will be drawn:
		origin : type_position;
	begin
		if verb = VERB_PLACE and noun = NOUN_TEXT and text_place.being_moved 
		and text_place.category = LAYER_CAT_OUTLINE then

			-- Set the point where the text is to be drawn:
			point := self.tool_position;

			-- Draw the origin of the text:
			origin := type_position (to_position (point, zero_rotation));
			draw_text_origin (self, origin, in_area, context);

			-- Set the line width of the vector text:
			set_line_width (context.cr, type_view_coordinate (text_place.text.line_width));

			-- Vectorize the text:
			vector_text := vectorize_text (
				content		=> text_place.text.content,
				size		=> text_place.text.size,
				rotation	=> rot (text_place.text.position),
				position	=> point,
				mirror		=> NO,
				line_width	=> text_place.text.line_width,
				alignment	=> text_place.text.alignment -- right, bottom
				);

			-- Draw the text:
			pac_draw_fab.draw_vector_text (in_area, context, vector_text,
				text_place.text.line_width, self.frame_height);
		end if;
	end draw_text_being_placed_in_outline;

	
	-- This procedure draws the text that is being placed in a
	-- conductor layer.
	-- The properties are taken from variable et_canvas_board_texts.text_place.
	-- The verb must be VERB_PLACE and the noun must be NOUN_TEXT. 
	-- Otherwise nothing happens here:
	procedure draw_text_being_placed_in_conductors (
		self    	: not null access type_view;
		in_area		: in type_rectangle := no_rectangle;
		context 	: in type_draw_context;
		category	: in type_layer_category_conductor;
		layer		: in et_pcb_stack.type_signal_layer)
	is 
		use et_pcb_stack;
		use et_text;
		use et_board_shapes_and_text;
		use et_board_shapes_and_text.pac_text_fab;
		use pac_text_fab.pac_vector_text_lines;
		vector_text : pac_text_fab.pac_vector_text_lines.list;

		-- The place where the text shall be placed:
		point : type_point;

		-- The place where the text origin will be drawn:
		origin : type_position;
	begin
		if verb = VERB_PLACE and noun = NOUN_TEXT and text_place.being_moved then
			
			if text_place.category = category and text_place.signal_layer = layer then

				-- Set the point where the text is to be drawn:
				point := self.tool_position;

				-- Draw the origin of the text:
				origin := type_position (to_position (point, zero_rotation));
				draw_text_origin (self, origin, in_area, context);

				-- Set the line width of the vector text:
				set_line_width (context.cr, type_view_coordinate (text_place.text.line_width));

				-- Vectorize the text:
				vector_text := vectorize_text (
					content		=> text_place.text.content,
					size		=> text_place.text.size,
					rotation	=> rot (text_place.text.position),
					position	=> point,
					mirror		=> NO,
					line_width	=> text_place.text.line_width,
					alignment	=> text_place.text.alignment -- right, bottom
					);

				-- Draw the text:
				pac_draw_fab.draw_vector_text (in_area, context, vector_text,
					text_place.text.line_width, self.frame_height);
			end if;
		end if;
	end draw_text_being_placed_in_conductors;

	
	procedure draw_outline (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_silk_screen (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context;
		face	: in type_face) is separate;

	procedure draw_assy_doc (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context;
		face	: in type_face) is separate;

	procedure draw_stop (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context;
		face	: in type_face) is separate;

	procedure draw_stencil (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context;
		face	: in type_face) is separate;
	
	procedure draw_keepout (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context;
		face	: in type_face) is separate;
	
	procedure draw_route_restrict (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_via_restrict (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	-- Draws objects in conductor layers (incl. vias):
	procedure draw_conductors (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_packages (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context;
		face	: in type_face) is separate;
	
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
		area_shifted_new_position : constant type_distance_relative := 
			to_distance_relative (set (
						x => - self.frame_bounding_box.x,
						y => - self.frame_bounding_box.y));

		use et_display.board;

		procedure draw_packages is begin
			draw_packages (self, area_shifted, context, BOTTOM);
			draw_packages (self, area_shifted, context, TOP);
		end draw_packages;

		procedure draw_silkscreen is begin
			if silkscreen_enabled (BOTTOM) then
				draw_silk_screen (self, area_shifted, context, BOTTOM);
			end if;

			if silkscreen_enabled (TOP) then
				draw_silk_screen (self, area_shifted, context, TOP);
			end if;
		end draw_silkscreen;

		procedure draw_assy_doc is begin
			if assy_doc_enabled (BOTTOM) then
				draw_assy_doc (self, area_shifted, context, BOTTOM);
			end if;

			if assy_doc_enabled (TOP) then
				draw_assy_doc (self, area_shifted, context, TOP);
			end if;
		end draw_assy_doc;

		procedure draw_keepout is begin
			if keepout_enabled (BOTTOM) then
				draw_keepout (self, area_shifted, context, BOTTOM);
			end if;

			if keepout_enabled (TOP) then
				draw_keepout (self, area_shifted, context, TOP);
			end if;
		end draw_keepout;

		procedure draw_stop_mask is begin
			if stop_mask_enabled (BOTTOM) then
				draw_stop (self, area_shifted, context, BOTTOM);
			end if;

			if stop_mask_enabled (TOP) then
				draw_stop (self, area_shifted, context, TOP);
			end if;
		end draw_stop_mask;

		procedure draw_stencil is begin
			if stencil_enabled (BOTTOM) then
				draw_stencil (self, area_shifted, context, BOTTOM);
			end if;

			if stencil_enabled (TOP) then
				draw_stencil (self, area_shifted, context, TOP);
			end if;
		end draw_stencil;

		procedure draw_pcb_outline is begin
			if outline_enabled then		
				draw_outline (self, area_shifted, context);
			end if;
		end draw_pcb_outline;

		procedure draw_conductor_layers is begin
			draw_route_restrict (self, area_shifted, context);
			draw_via_restrict (self, area_shifted, context);
			
			draw_conductors (self, area_shifted, context);

			-- CS unrouted
		end draw_conductor_layers;

		procedure draw_board is begin
			draw_packages;
			draw_silkscreen;
			draw_assy_doc;
			draw_keepout;
			draw_stop_mask;
			draw_stencil;
			draw_pcb_outline;
			draw_conductor_layers;
			
			-- CS draw_submodules
			
		end draw_board;
		
	begin -- draw_internal
		
-- 		put_line ("draw internal ...");
		
		set_color_background (context.cr, et_colors.no_opacity);
		paint (context.cr);

		-- Backup context for drawing the grid at the end of this procedure.
		save (context.cr);
		
		-- move area_shifted according to frame position:
		move_by (area_shifted, area_shifted_new_position);

		
		-- draw the frame:
		save (context.cr);
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			convert_x (self.frame_bounding_box.x),
			convert_y (self.frame_bounding_box.y));

		draw_frame (self, area_shifted, context);
		restore (context.cr);

		
		-- move area_shifted according to board position:
		move_by (area_shifted, to_distance_relative (invert (self.board_origin, X)));
		
		save (context.cr);
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		
		-- The drawing must further-on be shifted to the right and up by the board position
		-- so that the board origin is not at the lower left corner of the frame.
		-- The board origin is now somewhere inside the frame.
		translate (
			context.cr,
			convert_x (self.frame_bounding_box.x + get_x (self.board_origin)),
			convert_y (self.frame_bounding_box.y - get_y (self.board_origin)));


		-- draw packages, tracks, vias, silkscreen, pcb outline, ...
		draw_board;

		
		-- Grid and cursor is drawn last so that they
		-- are visible regardless of areas drawn with the 
		-- cairo CLEAR operator:
		
		draw_cursor (self, area_shifted, context, cursor_main);
		restore (context.cr);

		-- Restore context to draw the grid:
		restore (context.cr);

		if grid_enabled then
			draw_grid (self, context, area);
		end if;
		
	end draw_internal;

	
	procedure move_cursor (
		self		: not null access type_view;
		coordinates	: in type_coordinates;
		cursor		: in out type_cursor;
		position	: in type_point) 
	is
		use et_canvas_schematic;		
	begin
		case coordinates is
			when ABSOLUTE =>
				--cursor.position := type_point (round (position, element (current_active_module).board.grid));
				cursor.position := type_point (round (position, self.get_grid));
				
			when RELATIVE =>
				--cursor.position := type_point (round (cursor.position + position, element (current_active_module).board.grid));
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
		use et_canvas_schematic;		
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
		
		update_coordinates_display (self);
		self.shift_area (cursor);
	end move_cursor;

	
	procedure draw_cursor (
		self		: not null access type_view;
		in_area		: in type_rectangle := no_rectangle;
		context 	: in type_draw_context;
		cursor		: in type_cursor)
	is
		use pac_draw_fab;
		
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
			height		=> self.frame_height);

		draw_line (
			area		=> in_area,
			context		=> context,
			line		=> lv,
			width		=> type_distance_positive (width),
			height		=> self.frame_height);
		
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
		use et_canvas_schematic;
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
		return et_frames.type_frame is

		use et_canvas_schematic;
	begin
		return element (current_active_module).board.frame.frame;
	end get_frame;

	function frame_height (
		self : not null access type_view)
		return type_distance_positive is 

		use et_canvas_schematic;
	begin
		return type_distance_positive (element (current_active_module).board.frame.frame.size.y);
	end frame_height;

	function frame_width (
		self : not null access type_view)
		return type_distance_positive is 

		use et_canvas_schematic;
	begin
		return type_distance_positive (element (current_active_module).board.frame.frame.size.x);
	end frame_width;
	
	function title_block_position (
		self : not null access type_view)
		return et_frames.type_position is
	begin
		return self.get_frame.title_block_pcb.position;
	end title_block_position;

	function board_origin (
		self : not null access type_view)
		return type_point is

		use et_canvas_schematic;
	begin
		return element (current_active_module).board.origin;
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


	procedure place_text (destination : in type_point) is
		use et_packages;
		use et_board_shapes_and_text;
		use pac_text_fab;
		use et_canvas_schematic;
	begin
		if text_place.being_moved then
			move_to (text_place.text.position, destination);
			
			if text_place.category in type_layer_category_non_conductor then

				place_text_in_non_conductor_layer (
					module_cursor 	=> current_active_module,
					layer_category	=> text_place.category,
					face			=> text_place.face,
					text			=> text_place.text,
					log_threshold	=> log_threshold + 1);

			elsif text_place.category in type_layer_category_outline then

				place_text_in_outline_layer (
					module_cursor 	=> current_active_module,
					layer_category	=> text_place.category,
					text			=> text_place.text,
					log_threshold	=> log_threshold + 1);

				
			elsif text_place.category in type_layer_category_conductor then
				
				place_text_in_conductor_layer (
					module_cursor 	=> current_active_module,
					layer_category	=> text_place.category,
					text			=> (type_text_fab (text_place.text) with 
										content		=> text_place.text.content,
										vectors		=> text_place.text.vectors,
										layer		=> text_place.signal_layer),
					log_threshold	=> log_threshold + 1);

			else
				raise semantic_error_1 with
					"ERROR: Text not allowed in this layer category !";
				-- CS should never happen
			end if;
		end if;	
	end place_text;

	-- Builds the final via-to-be-placed from the information
	-- provided by temporarily variable via_place.
	-- Inserts the via in the module.
	procedure place_via (destination : in type_point) is
		use et_canvas_schematic;
		via : type_via (category => via_place.category);
	begin
		if via_place.being_moved then
			
			via.position := via_place.drill.position;
			via.diameter := via_place.drill.diameter;
			
			via.restring_inner := via_place.restring_inner;
			
			move_to (via.position, destination);

			case via_place.category is
				when THROUGH =>
					via.restring_outer := via_place.restring_outer;
								  
				when BURIED =>
					via.layers := via_place.layers_buried;
										  
				when BLIND_DRILLED_FROM_TOP =>
					via.restring_top := via_place.restring_outer;
					via.lower := via_place.destination_blind;
					
				when BLIND_DRILLED_FROM_BOTTOM =>
					via.restring_bottom := via_place.restring_outer;
					via.upper := via_place.destination_blind;

			end case;

			place_via (
				module_cursor	=> current_active_module,
				net_name		=> get_name (via_place.net),
				via				=> via,
				log_threshold	=> log_threshold + 1);

		end if;
	end place_via;
	
	procedure evaluate_key (
		self	: not null access type_view;
		key		: in gdk_key_type) 
	is
		use gdk.types;
		use gdk.types.keysyms;

		use et_modes;

		procedure delete is begin
			case key is
				when GDK_LC_d =>
					noun := NOUN_DEVICE;
					set_status (status_click_left & "delete non-electrical device."
						& status_hint_for_abort);
					
				when others => status_noun_invalid;
			end case;
		end delete;

		procedure fill is begin
			case key is
				when GDK_LC_p =>
					noun := NOUN_POLYGON;
					fill_conductor_polygons (current_active_module, log_threshold + 1);

					set_status ("conductor polygons filled");
					
				when others => status_noun_invalid;
			end case;
		end fill;
		
		procedure place is begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_t =>
					noun := NOUN_TEXT;
					show_text_properties;
					set_status (status_place_text);

				when GDK_LC_v =>
					noun := NOUN_VIA;
					show_via_properties;
					set_status (status_place_via);
					
				-- If space pressed, then the operator wishes to operate via keyboard:	
				when GDK_Space =>
					case noun is
						when NOUN_TEXT =>
							place_text (cursor_main.position);

						when NOUN_VIA =>
							place_via (cursor_main.position);
							
						when others => null;
					end case;
					
				when others => status_noun_invalid;
			end case;
		end place;
		
	begin -- evaluate_key
		
-- 		put_line ("board: evaluating other key ...");
-- 		put_line (gdk_modifier_type'image (key_ctrl));

		case key is
			when GDK_Escape =>
				expect_entry := expect_entry_default;
				
				-- Verb and noun emain as they are
				-- so that the mode is unchanged.
				
				reset_request_clarification;
				status_enter_verb;

				reset_text_place; -- after placing a text
				reset_via_place; -- after placing a via
				
			when GDK_F11 =>
				et_canvas_schematic.previous_module;

			when GDK_F12 =>
				et_canvas_schematic.next_module;
				
			when others =>
				
				-- If the command is waiting for finalization, usually by pressing
				-- the space key, AND the primary tool is the keyboard, then
				-- we call the corresponding subprogram right away here:
				if single_cmd_status.finalization_pending and primary_tool = KEYBOARD then
					case verb is
						when VERB_PLACE		=> place;
						when others			=> null;
					end case;
			
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
							
							case key is
								when GDK_Delete =>
									verb := VERB_DELETE;
									status_enter_noun;

								when GDK_LC_d =>
									verb := VERB_DRAW;
									status_enter_noun;

								when GDK_LC_f =>
									verb := VERB_FILL;
									status_enter_noun;
									
								when GDK_LC_p =>
									verb := VERB_PLACE;
									status_enter_noun;
									
								when GDK_LC_r =>
									verb := VERB_ROUTE;
									status_enter_noun;
									
								when others =>
									--put_line ("other key pressed " & gdk_key_type'image (key));

									-- If invalid verb entered, overwrite expect_entry by EXP_VERB
									-- and show error in status bar:
									expect_entry := EXP_VERB;
									status_verb_invalid;
							end case;

							---- Clean up: ???
							---- Some toolbars or property bars must be removed:
							--et_canvas_board_texts.remove_text_properties; -- after placing text

							
						when EXP_NOUN =>
							--put_line ("NOUN entered");

							case verb is
								when VERB_DELETE	=> delete;
								when VERB_FILL		=> fill;
								when VERB_PLACE		=> place;
								when others => null; -- CS
							end case;
							
					end case;

				end if;
		end case;

		redraw;		
		update_mode_display (canvas);

		
		exception when event: others =>
			set_status (exception_message (event));

			--reset_selections;
		
			redraw;
			update_mode_display (canvas);
		
	end evaluate_key;

	procedure evaluate_mouse_position (
		self	: not null access type_view;
		point	: in type_point) 
	is begin
		case verb is
			when VERB_PLACE =>
				case noun is
					when NOUN_TEXT =>
						if text_place.being_moved then
							redraw;
						end if;

					when NOUN_VIA =>
						if via_place.being_moved then
							redraw;
						end if;
						
					when others => null;
				end case;

			when others => null;
		end case;
		
	end evaluate_mouse_position;
	
	procedure button_pressed (
		self	: not null access type_view;
		button	: in type_mouse_button;
		point	: in type_point) 
	is
		snap_point : constant type_point := snap_to_grid (self, point);

		procedure left_button is
		begin
			self.move_cursor (ABSOLUTE, cursor_main, point);

			case verb is
				when VERB_PLACE =>
					case noun is
						when NOUN_TEXT =>
							place_text (snap_point);

						when NOUN_VIA =>
							place_via (snap_point);
							
						when others => null;
					end case;
					
				when others => null;

			end case;
		end left_button;

		procedure right_button is
		begin
			null;
		end right_button;

		
	begin -- button_pressed
		--log (text => to_string (button) & " at" & to_string (point), level => log_threshold);
		
		case button is
			when 1 => left_button;
			when 3 => right_button;
			when others => null;
		end case;

		redraw;

		exception when event: others =>
			set_status (exception_message (event));

			-- CS reset_selections;
			redraw;

		
	end button_pressed;

	
	procedure save_drawing (
		self : not null access type_view)
	is 
		use et_canvas_schematic;
	begin
		save_module;
	end save_drawing;

	
end et_canvas_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
