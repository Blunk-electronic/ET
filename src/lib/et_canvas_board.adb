------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with ada.text_io;				use ada.text_io;

with et_canvas_schematic;
with et_display.board;
with et_colors.board;			use et_colors.board;
with et_modes.board;			use et_modes.board;
with et_pcb;
with et_pcb_stack;
with et_text;
with et_meta;

package body et_canvas_board is

	use et_project.modules.pac_generic_modules;
	
	procedure set_title_bar (
		-- CS project name
		module		: in et_general.type_module_name.bounded_string)
	is
		use et_general;
	begin
		window.set_title (title & to_string (module));
	end set_title_bar;


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
	
	function to_string (
		self	: not null access type_view;
		point	: in type_point;
		axis	: in et_general.type_axis_2d)
		return string 
	is
		use et_general;
		use et_canvas_schematic;
	begin
		case axis is
			when X => return to_string (round (x (point), element (current_active_module).board.grid.x));
			when Y => return to_string (round (y (point), element (current_active_module).board.grid.y));
		end case;
	end;

	function to_string (
		self	: not null access type_view;
		point	: in type_point)
		return string is
		use et_canvas_schematic;		
	begin
		return round_to_string (point, element (current_active_module).board.grid);
	end;
	
	function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_point)	
		return type_point is 
		use et_general;
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> model_point.x 
						- self.frame_bounding_box.x
						- x (self.board_origin) -- because board origin is not the same as drawing origin
			);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.frame_height) 
						- model_point.y 
						+ self.frame_bounding_box.y
						- y (self.board_origin)  -- because board origin is not the same as drawing origin
			);

		return p;
	end;

	function snap_to_grid (
		self	: not null access type_view;
		point	: in type_point)
		return type_point is
		use et_canvas_schematic;
	begin
		return type_point (round (point, element (current_active_module).board.grid));
	end snap_to_grid;

	
	function drawing_to_model (
		self			: not null access type_view;
		drawing_point : in type_point)	
		return type_point is 
		use et_general;
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> drawing_point.x 
						+ self.frame_bounding_box.x
						+ x (self.board_origin) -- because board origin is not the same as drawing origin
			);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.frame_height) 
						- drawing_point.y 
						+ self.frame_bounding_box.y
						- y (self.board_origin)  -- because board origin is not the same as drawing origin
			);

		return p;
	end;


	
	function active_module (self : not null access type_view) 
		return string is
		use et_general.type_module_name;
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
		context	: in type_draw_context) is
		
		use et_terminals;
		type type_line is new et_terminals.pac_shapes.type_line with null record;
		
		line_horizontal : constant type_line := ( -- from left to right
			start_point		=> type_point (set (x => x (p) - pac_text.origin_half_size, y => y (p))),
			end_point		=> type_point (set (x => x (p) + pac_text.origin_half_size, y => y (p))));

		line_vertical : constant type_line := ( -- from bottom to top
			start_point		=> type_point (set (x => x (p), y => y (p) - pac_text.origin_half_size)),
			end_point		=> type_point (set (x => x (p), y => y (p) + pac_text.origin_half_size)));

	begin -- draw_text_origin
		-- CS if text_origins_enabled then
		
			set_line_width (context.cr, type_view_coordinate (pac_text.origin_line_width));
			pac_draw_package.draw_line (in_area, context, line_horizontal, self.frame_height);
			pac_draw_package.draw_line (in_area, context, line_vertical, self.frame_height);

		--end if;
	end draw_text_origin;

	-- Maps from face to mirror status of a vectorized text.
	-- Use it for drawing non-device related texts and placeholders.
	function face_to_mirror (f : in type_face) return et_text.type_vector_text_mirrored is 
		use et_text;
	begin
		case f is
			when TOP	=> return NO;
			when BOTTOM	=> return YES;
		end case;
	end face_to_mirror;

	-- Maps from signal layer to mirror status of a vectorized text.
	-- Use it for drawing non-device related texts and placeholders.
	function signal_layer_to_mirror (
		current_layer, bottom_layer : in et_pcb_stack.type_signal_layer)
		return et_text.type_vector_text_mirrored is
		use et_text;
		use et_pcb_stack;
	begin
		if current_layer = bottom_layer then
			return YES;
		else
			return NO;
		end if;
	end signal_layer_to_mirror;
	
	-- Maps from the meaning of a text to its actutal content.
	function to_placeholder_content (
		meaning : in et_pcb.type_text_meaning)
		return et_text.type_text_content.bounded_string is

		use et_general;
		use et_text;
		use et_meta;
		use et_canvas_schematic;
	
		meta : constant type_board := element (current_active_module).meta.board;

		use type_variant_name;
		variant : constant type_variant_name.bounded_string := element (current_active_module).active_variant;

		result : type_text_content.bounded_string;

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
		use et_general;

		-- The given area must be shifted (left and up) by the position
		-- of the drawing frame. This is required for all objects in the 
		-- drawing frame.
		-- Take a copy of the given area:
		area_shifted : type_rectangle := area;

		-- Calculate the new position of area_shifted:
		area_shifted_new_position : type_point := type_point (set (
						x => - self.frame_bounding_box.x,
						y => - self.frame_bounding_box.y));

		use et_display.board;
	begin
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
		move_by (area_shifted, type_point (invert (self.board_origin, X)));
		
		save (context.cr);
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		
		-- The drawing must further-on be shifted to the right and up by the board position
		-- so that the board origin is not at the lower left corner of the frame.
		-- The board origin is now somewhere inside the frame.
		translate (
			context.cr,
			convert_x (self.frame_bounding_box.x + x (self.board_origin)),
			convert_y (self.frame_bounding_box.y - y (self.board_origin)));

		-- The order of drawing layers is so that top layers
		-- always obscure layers underneath:
		
	-- BOTTOM
		-- silkscreen
		if silkscreen_enabled (BOTTOM) then
			draw_silk_screen (self, area_shifted, context, BOTTOM);
		end if;

		-- stop mask
		if stop_mask_enabled (BOTTOM) then
			draw_stop (self, area_shifted, context, BOTTOM);
		end if;

		-- stencil / solder paste / solder cream
		if stencil_enabled (BOTTOM) then
			draw_stencil (self, area_shifted, context, BOTTOM);
		end if;

		-- draw packages on the bottom side of the board
		draw_packages (self, area_shifted, context, BOTTOM);
		
		-- keepout
		if keepout_enabled (BOTTOM) then
			draw_keepout (self, area_shifted, context, BOTTOM);
		end if;
		
		-- assembly documentation
		if assy_doc_enabled (BOTTOM) then
			draw_assy_doc (self, area_shifted, context, BOTTOM);
		end if;
	
	-- CONDUCTOR LAYERS
		draw_route_restrict (self, area_shifted, context);
		draw_via_restrict (self, area_shifted, context);
		draw_conductors (self, area_shifted, context);
		-- CS draw unrouted
		
	-- TOP		
		-- silkscreen
		if silkscreen_enabled (TOP) then
			draw_silk_screen (self, area_shifted, context, TOP);
		end if;

		-- stop mask
		if stop_mask_enabled (TOP) then
			draw_stop (self, area_shifted, context, TOP);
		end if;

		-- stencil / solder paste / solder cream
		if stencil_enabled (TOP) then
			draw_stencil (self, area_shifted, context, TOP);
		end if;

		-- draw packages on the top side of the board
		draw_packages (self, area_shifted, context, TOP);
		
		-- keepout
		if keepout_enabled (TOP) then
			draw_keepout (self, area_shifted, context, TOP);
		end if;
		
		-- assembly documentation
		if assy_doc_enabled (TOP) then
			draw_assy_doc (self, area_shifted, context, TOP);
		end if;

		
		-- CS draw_submodules

	-- OUTLINE
		if outline_enabled then		
			draw_outline (self, area_shifted, context);
		end if;


		
		-- Grid and cursor is drawn here so that they are in the foreground:
		
	-- CURSOR
		draw_cursor (self, area_shifted, context, cursor_main);
		restore (context.cr);

	-- GRID
		-- Restore context to draw the grid:
		restore (context.cr);
		if grid_enabled then
			draw_grid (self, context, area);
		end if;
		
	end draw_internal;

	
	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total)
		return type_view_coordinate is 
	begin
		return type_view_coordinate 
			(
			self.frame_height 
			- y
			);
	end;
		
	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total)
		return type_distance_total is 
	begin
		return (
			self.frame_height 
			- y
			);
	end;


	procedure move_cursor (
		self		: not null access type_view;
		coordinates	: in type_coordinates;
		cursor		: in out type_cursor;
		position	: in type_point) is
		use et_general;
		use et_canvas_schematic;		
	begin
		case coordinates is
			when ABSOLUTE =>
				cursor.position := type_point (round (position, element (current_active_module).board.grid));
				
			when RELATIVE =>
				cursor.position := type_point (round (cursor.position + position, element (current_active_module).board.grid));
		end case;

		update_coordinates_display (self);
		self.shift_area (cursor);		
	end move_cursor;

	procedure move_cursor (
		self		: not null access type_view;
		direction	: in type_cursor_direction;
		cursor		: in out type_cursor) is

		-- Get the currently active grid:
		use et_general;
		use et_canvas_schematic;		
		grid : constant type_grid := element (current_active_module).board.grid;

		-- Find the grid point nearest available to the current cursor position:
		position_snapped : constant type_point := type_point (round (
							point	=> cursor.position,
							grid	=> grid));

	begin
		case direction is
			when RIGHT =>
				cursor.position := type_point (move (position_snapped, 0.0, grid.x));

			when LEFT =>
				cursor.position := type_point (move (position_snapped, 180.0, grid.x));

			when UP =>
				cursor.position := type_point (move (position_snapped, 90.0, grid.y));

			when DOWN =>
				cursor.position := type_point (move (position_snapped, -90.0, grid.y));
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
	begin
		size := cursor_half_size / type_distance_positive (self.scale);
		
		-- set start and end point of horizontal line
		lh.start_point := type_point (set (
			x	=> x (cursor.position) - size,
			y	=> y (cursor.position)));

		lh.end_point := type_point (set (
			x	=> x (cursor.position) + size,
			y	=> y (cursor.position)));

		-- set start and end point of vertical line
		lv.start_point := type_point (set (
			x	=> x (cursor.position),
			y	=> y (cursor.position) + size));

		lv.end_point := type_point (set (
			x	=> x (cursor.position),
			y	=> y (cursor.position) - size));


		-- The line width is inversely proportional to the scale:
		cairo.set_line_width (context.cr, type_view_coordinate (cursor_line_width) / self.scale);
		set_color_cursor (context.cr);

		pac_draw_package.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> lh,
			height		=> self.frame_height);

		pac_draw_package.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> lv,
			height		=> self.frame_height);
		
		cairo.stroke (context.cr);		
	end draw_cursor;


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

	
	procedure evaluate_key (
		self	: not null access type_view;
		key		: in gdk_key_type) is

		use gdk.types;
		use gdk.types.keysyms;

		use et_modes;
	begin
-- 		put_line ("board: evaluating other key ...");
-- 		put_line (gdk_modifier_type'image (key_ctrl));

		if key = GDK_Escape then
			expect_entry := expect_entry_default;
			verb := verb_default;
			noun := noun_default;

			reset_request_clarification;
			status_enter_verb;
		else
				
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

						when GDK_LC_d => -- GDK_D
							verb := VERB_DRAW;
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


				when EXP_NOUN =>
					--put_line ("NOUN entered");

					case verb is
						when VERB_DELETE =>
							
							case key is
								when GDK_LC_d =>
									noun := NOUN_DEVICE;
									set_status (status_click_left & "delete non-electrical device."
										& status_hint_for_abort);
									
								when others => status_noun_invalid;
							end case;

						when others => null; -- CS
					end case;
					
			end case;

		end if;

		redraw;		
		update_mode_display (canvas);
		
	end evaluate_key;

	overriding procedure button_pressed (
		self	: not null access type_view;
		button	: in type_mouse_button;
		point	: in type_point) 
	is
	begin
		log (text => to_string (button) & " at" & to_string (point), level => log_threshold);
		
		case button is
			when 1 => -- left button
				
				self.move_cursor (ABSOLUTE, cursor_main, point);

-- 				case verb is
-- 					when VERB_DELETE =>
-- 
-- 						case noun is
-- 							when NOUN_UNIT => null;
-- 
-- 							when NOUN_NET => null;
-- 
-- 							when others =>
-- 								null;
-- 								
-- 						end case;
-- 
-- 					when VERB_DRAW =>
-- 
-- 						case noun is
-- 							when NOUN_NET => null;
-- 
-- 							when others =>
-- 								null;
-- 								
-- 						end case;
-- 
-- 					when others => null; -- CS
-- 				end case;
				
				self.queue_draw; -- without frame and grid initialization

			when others => null;
		end case;

		redraw;
	end button_pressed;
	
end et_canvas_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
