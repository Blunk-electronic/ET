------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS FOR SCHEMATIC                              --
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

with ada.text_io;					use ada.text_io;
with ada.numerics;					use ada.numerics;

with et_terminals;
with et_devices;

with et_canvas_board;
with et_display.schematic;			use et_display.schematic;
with et_colors;						use et_colors;
with et_colors.schematic;			use et_colors.schematic;
with et_modes.schematic;			use et_modes.schematic;


package body et_canvas_schematic is

	procedure set_title_bar (
		-- CS project name
		module		: in et_general.type_module_name.bounded_string;
		sheet		: in type_sheet) is
		use et_general;
	begin
		window.set_title (title & to_string (module) &
			" sheet " & to_sheet (sheet));
	end set_title_bar;

	
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
	
	
	function to_string (
		self	: not null access type_view;
		point	: in type_point;
		axis	: in et_general.type_axis_2d)
		return string 
	is
		use et_general;
		use et_project.modules.pac_generic_modules;
	begin
		case axis is
			when X => return to_string (round (x (point), element (current_active_module).grid.x));
			when Y => return to_string (round (y (point), element (current_active_module).grid.y));
		end case;
	end;

	function to_string (
		self	: not null access type_view;
		point	: in type_point)
		return string is
		use et_project.modules.pac_generic_modules;
	begin
		return round_to_string (point, element (current_active_module).grid);
	end;

	function snap_to_grid (
		self	: not null access type_view;
		point	: in type_point)
		return type_point is
	begin
		return type_point (round (point, element (current_active_module).grid));
	end snap_to_grid;


	
	function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_point)	
		return type_point is 
		use et_general;
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> model_point.x - self.frame_bounding_box.x);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.frame_height) 
						- model_point.y 
						+ self.frame_bounding_box.y);
	
		return p;
	end;

	function drawing_to_model (
		self			: not null access type_view;
		drawing_point	: in type_point)	
		return type_point is 
		use et_general;
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> drawing_point.x + self.frame_bounding_box.x);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.frame_height) 
						- drawing_point.y 
						+ self.frame_bounding_box.y);

		return p;
	end;
	
	function active_module return et_general.type_module_name.bounded_string is
		use et_general.type_module_name;
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
		net		: in et_general.type_net_name.bounded_string;
		label	: in et_schematic.type_net_label) is separate;
	
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
		device_name		: in et_devices.type_name;
		device_value	: in et_devices.type_value.bounded_string; -- like 100R or TL084
		device_purpose	: in et_devices.type_purpose.bounded_string; -- like "brightness control"
		unit_name		: in et_devices.type_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...
		unit_count		: in et_devices.type_unit_count;
		unit_position	: in type_point; -- x/y on the schematic sheet
		unit_rotation	: in type_rotation;
		sch_placeholder_name	: in et_symbols.type_text_placeholder;
		sch_placeholder_value	: in et_symbols.type_text_placeholder;
		sch_placeholder_purpose : in et_symbols.type_text_placeholder;
		brightness		: in type_brightness := NORMAL)
		is separate;

	-- Draws all units:
	procedure draw_units (
		self	: not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	-- Draws a single selected unit:
	-- The unit will be drawn highlighted.
-- 	procedure draw_selected_unit (
-- 		self	: not null access type_view;
-- 		in_area	: in type_rectangle := no_rectangle;
-- 		context : in type_draw_context;
-- 		unit	: in type_selected_unit) is separate;
	-- CS: not used currently
	
	-- Draws a single selected net segment and the net labels attached to it.
	-- The net segment and the labels will be drawn highlighted.
-- 	procedure draw_selected_net_segment ( 
-- 		self	: not null access type_view;
-- 		in_area	: in type_rectangle := no_rectangle;
-- 		context : in type_draw_context;
-- 		segment	: in type_selected_segment) is separate;
-- 	-- CS: not used currently

	procedure draw_net_route_being_drawn (
		self	: not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context)
	is
		use et_schematic;
		use pac_draw_misc;

		type type_line is new pac_shapes.type_line with null record;
		line : type_line;

		procedure compute_route (s, e : in type_point) is 
			use pac_shapes;

			-- Do the actual route calculation.
			r : type_route := to_route (s, e, net_route.bend_style);

			procedure draw is begin
				-- draw the net segment:
				draw_line (
					area		=> in_area,
					context		=> context,
					line		=> line,
					height		=> self.frame_height);
			end draw;
			
		begin -- compute_route

			-- The calculated route may required a bend point.
			-- Set/clear the "bended" flag of the net_segment being drawn.
			net_route.bended := r.bended;

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
				net_route.bend_point := r.bend_point;

				line.start_point := r.start_point;
				line.end_point := r.bend_point;
				
				draw;

				line.start_point := r.bend_point;
				line.end_point := r.end_point;
				
				draw;
				
			end if;
		end compute_route;
		
	begin -- draw_net_route_being_drawn
		if verb = VERB_DRAW and noun = NOUN_NET and net_route.being_drawn = true then

			-- The route start point has been set eariler by procedures
			-- evaluate_key or button_pressed.
			-- For drawing here, the route end point is to be taken from
			-- either the mouse pointer or the cursor position:
			case net_route.tool is
				
				when MOUSE => 
					
					compute_route (
						s	=> net_route.start_point,				-- start of route
						e	=> snap_to_grid (self, mouse_position (self)));	-- end of route

				when KEYBOARD =>

					compute_route (
						s	=> net_route.start_point,	-- start of route
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
		area_shifted_new_position : type_point := type_point (set (
						x => - self.frame_bounding_box.x,
						y => - self.frame_bounding_box.y));
		
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
		module	: in et_general.type_module_name.bounded_string)  -- motor_driver
	is
		use et_general;
		use et_project.modules;
		use et_project.modules.pac_generic_modules;
		cursor : et_project.modules.pac_generic_modules.cursor := find (generic_modules, module);
	begin
		if cursor /= pac_generic_modules.no_element then -- module exists in project
			current_active_module := cursor;
		else
			log (WARNING, "Generic module " & enclose_in_quotes (to_string (module)) 
				 & " does not exist !",
				 console => true);

			
			-- CS list available modules
		end if;
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

	procedure move_cursor (
		self		: not null access type_view;
		coordinates	: in type_coordinates;
		cursor		: in out type_cursor;
		position	: in type_point) is
		use et_general;
		use et_project.modules.pac_generic_modules;
	begin
		case coordinates is
			when ABSOLUTE =>
				cursor.position := type_point (round (position, element (current_active_module).grid));
				
			when RELATIVE =>
				cursor.position := type_point (round (cursor.position + position, element (current_active_module).grid));
		end case;

		update_coordinates_display (self);
		self.shift_area (cursor);		
	end move_cursor;

	procedure move_cursor (
		self		: not null access type_view;
		direction	: in type_cursor_direction;
		cursor		: in out type_cursor) is

		-- Get the currently active grid:
		use et_project.modules.pac_generic_modules;
		grid : constant type_grid := element (current_active_module).grid;

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

		pac_draw_misc.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> lh,
			height		=> self.frame_height);

		pac_draw_misc.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> lv,
			height		=> self.frame_height);
		
		cairo.stroke (context.cr);		
	end draw_cursor;

	function get_frame (
		self : not null access type_view)
		return et_frames.type_frame is

		use et_project.modules.pac_generic_modules;
	begin
		return element (current_active_module).frames.frame;
	end get_frame;

	function frame_height (
		self : not null access type_view)
		return type_distance_positive is 

		use et_project.modules.pac_generic_modules;
	begin
		return type_distance_positive (element (current_active_module).frames.frame.size.y);
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
	

	
	procedure evaluate_key (
		self	: not null access type_view;
		key		: in gdk_key_type) is

		use gdk.types;
		use gdk.types.keysyms;

		use et_modes;

		procedure delete is begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_u =>
					noun := NOUN_UNIT;
					
					set_status (et_canvas_schematic_units.status_delete);
					
				when GDK_LC_n =>
					noun := NOUN_NET;
					
					set_status (et_canvas_schematic_nets.status_delete);

				-- If space pressed, then the operator wishes to operate via keyboard:	
				when GDK_Space =>
					case noun is
						when NOUN_UNIT =>
							if not clarification_pending then
								delete_unit (cursor_main.position);
							else
								delete_selected_unit;
							end if;

						when NOUN_NET => 
							if not clarification_pending then
								delete_net_segment (cursor_main.position);
							else
								delete_selected_net_segment;
							end if;

						when others =>
							null;
							
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


				when GDK_LC_u =>
					noun := NOUN_UNIT;

					set_status (et_canvas_schematic_units.status_drag);


				-- If space pressed then the operator wishes to operate
				-- by keyboard:
				when GDK_Space =>
		
					case noun is
						when NOUN_NET =>
							if not segment.being_moved then
								
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
							if not unit.being_moved then
								
								-- Set the tool being used for moving the unit:
								unit.tool := KEYBOARD;
								
								if not clarification_pending then
									find_units (cursor_main.position);
								else
									find_attached_segments;
									unit.being_moved := true;
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
					
					set_status (status_click_left & "or " 
						& status_press_space 
						& status_set_start_point 
						& status_hint_for_abort);
					
					reset_net_route;

				-- If space pressed, then the operator wishes to operate via keyboard:
				when GDK_Space =>
					case noun is
						when NOUN_NET =>

							-- Set the tool being used for this net so that procedure
							-- draw_net_segment_being_drawn knows where to get the end point from.
							net_route.tool := KEYBOARD;

							if not net_route.being_drawn then

								net_route.start_point := cursor_main.position;
								
								-- Before processing the start point further, it must be validated:
								if valid_for_net_segment (net_route.start_point, log_threshold + 3) then

									net_route.being_drawn := true;
									
									set_status (status_start_point & to_string (net_route.start_point) & ". " &
										status_press_space & status_set_end_point & status_hint_for_abort);
								end if;

							else
								-- set end point
								if net_route.bended = NO then
									
									net_route.end_point := cursor_main.position;

									-- Before processing the end point further, it must be validated:
									if valid_for_net_segment (net_route.end_point, log_threshold + 3) then

										insert_net_segment (
											module			=> current_active_module,
											sheet			=> current_active_sheet,
											segment			=> (
													start_point	=> net_route.start_point,
													end_point	=> net_route.end_point,
													others		=> <>), -- no labels and no ports, just a bare segment
											log_threshold	=>	log_threshold + 1);

										reset_net_route;
									end if;

								else
									-- Before processing the BEND point further, it must be validated:
									if valid_for_net_segment (net_route.bend_point, log_threshold + 3) then

										insert_net_segment (
											module			=> current_active_module,
											sheet			=> current_active_sheet,
											segment			=> (
													start_point	=> net_route.start_point,
													end_point	=> net_route.bend_point,
													others		=> <>), -- no labels and no ports, just a bare segment
											log_threshold	=>	log_threshold + 1);

										-- END POINT:
										net_route.end_point := cursor_main.position;

										-- Before processing the END point further, it must be validated:
										if valid_for_net_segment (net_route.end_point, log_threshold + 3) then
										
											insert_net_segment (
												module			=> current_active_module,
												sheet			=> current_active_sheet,
												segment			=> (
														start_point	=> net_route.bend_point,
														end_point	=> net_route.end_point,
														others		=> <>), -- no labels and no ports, just a bare segment
												log_threshold	=>	log_threshold + 1);
										
											reset_net_route;
										end if;
									end if;

								end if;
							end if;
							
						when others => null;
					end case;

				-- If B pressed, then a bend style is being selected.
				-- this affects only certain modes and is ignored otherwise:
				when GDK_LC_b =>
					case noun is
						when NOUN_NET =>
							et_schematic.pac_shapes.next_bend_style (net_route);
							
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


				when GDK_LC_u =>
					noun := NOUN_UNIT;

					set_status (et_canvas_schematic_units.status_move);

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

						when NOUN_UNIT =>
							if not unit.being_moved then
								
								-- Set the tool being used for moving the unit:
								unit.tool := KEYBOARD;
								
								if not clarification_pending then
									find_units (cursor_main.position);
								else
									--move_selected_unit;
									unit.being_moved := true;
									reset_request_clarification;
								end if;
								
							else
								-- Finally assign the cursor position to the
								-- currently selected unit:
								et_canvas_schematic_units.finalize_move (
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

-- 						when NOUN_NET => 
-- 							if clarification_pending then
-- 								clarify_net_segment;
-- 							end if;

						when others => null;
							
					end case;

					
				when others => status_noun_invalid;
			end case;
		end move;

		procedure rotate is begin
			case key is
				-- EVALUATE KEY FOR NOUN:
				when GDK_LC_u =>
					noun := NOUN_UNIT;
					
					set_status (et_canvas_schematic_units.status_rotate);
	

				-- If space pressed, then the operator wishes to operate via keyboard:	
				when GDK_Space =>
					case noun is
						when NOUN_UNIT =>
							if not clarification_pending then
								rotate_unit (cursor_main.position);
							else
								rotate_selected_unit;
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
					
				when others => status_noun_invalid;
			end case;
		end rotate;

		
	begin -- evaluate_key
		
-- 		put_line ("schematic: evaluating other key ...");
-- 		put_line (gdk_modifier_type'image (key_ctrl));

		if key = GDK_Escape then
			expect_entry := expect_entry_default;
			verb := verb_default;
			noun := noun_default;
			
			reset_request_clarification;
			
			reset_net_route; -- after drawing a net route
			reset_segment; -- after move/drag
			reset_segments_being_dragged; -- after dragging a unit
			reset_unit; -- after moving a unit
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

					-- EVALUATE KEY FOR VERB:
					case key is
						when GDK_Delete =>
							verb := VERB_DELETE;
							status_enter_noun;
							
						when GDK_LC_g =>
							verb := VERB_DRAG;
							status_enter_noun;

						when GDK_LC_d =>
							verb := VERB_DRAW;
							status_enter_noun;
							
						when GDK_LC_m =>
							verb := VERB_MOVE;
							status_enter_noun;

						when GDK_LC_r =>
							verb := VERB_ROTATE;
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
						when VERB_DELETE	=> delete;
						when VERB_DRAG		=> drag;
						when VERB_DRAW		=> draw;
						when VERB_MOVE		=> move;
						when VERB_ROTATE	=> rotate;

						when others => null; -- CS
					end case;
					
			end case;

		end if;

		redraw;
		update_mode_display (canvas);
		
	end evaluate_key;

	overriding procedure evaluate_mouse_position (
		self	: not null access type_view;
		point	: in type_point) is
	begin
		case verb is
			when VERB_DRAW =>
				case noun is
					when NOUN_NET =>
						if net_route.being_drawn then
							redraw;
						end if;


					when others => null;
				end case;

			when VERB_DRAG | VERB_MOVE =>
				case noun is
					when NOUN_NET =>
						if segment.being_moved then
							redraw;
						end if;

					when NOUN_UNIT =>
						if unit.being_moved then
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
		procedure left_button is begin
			self.move_cursor (ABSOLUTE, cursor_main, point);

			case verb is
				when VERB_DELETE =>

					case noun is
						when NOUN_UNIT =>
							if not clarification_pending then
								delete_unit (point);
							else
								delete_selected_unit;
							end if;
							
						when NOUN_NET => 
							if not clarification_pending then
								delete_net_segment (point);
							else
								delete_selected_net_segment;
							end if;

						when others => null;
					end case;

				when VERB_DRAG =>

					case noun is
						when NOUN_UNIT =>
							if not unit.being_moved then
								
								-- Set the tool being used for moving the unit:
								unit.tool := MOUSE;
								
								if not clarification_pending then
									find_units (point);
								else
									find_attached_segments;
									unit.being_moved := true;
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

							-- Set the tool being used for this net so that procedure
							-- draw_net_segment_being_drawn knows where to get the end point from.
							net_route.tool := MOUSE;
							
							if not net_route.being_drawn then

								net_route.start_point := snap_to_grid (self, point);

								-- Before processing the start point further, it must be validated:
								if valid_for_net_segment (net_route.start_point, log_threshold + 3) then

									net_route.being_drawn := true;
									
									set_status (status_start_point & to_string (net_route.start_point) & ". " &
										status_click_left & status_set_end_point & status_hint_for_abort);
								end if;

							else
								-- set end point
								if net_route.bended = NO then
									
									net_route.end_point := snap_to_grid (self, point);

									-- Before processing the end point further, it must be validated:
									if valid_for_net_segment (net_route.end_point, log_threshold + 3) then

										insert_net_segment (
											module			=> current_active_module,
											sheet			=> current_active_sheet,
											segment			=> (
													start_point	=> net_route.start_point,
													end_point	=> net_route.end_point,
													others		=> <>), -- no labels and no ports, just a bare segment
											log_threshold	=>	log_threshold + 1);

										reset_net_route;
									end if;

								else
									-- Before processing the BEND point further, it must be validated:
									if valid_for_net_segment (net_route.bend_point, log_threshold + 3) then

										insert_net_segment (
											module			=> current_active_module,
											sheet			=> current_active_sheet,
											segment			=> (
													start_point	=> net_route.start_point,
													end_point	=> net_route.bend_point,
													others		=> <>), -- no labels and no ports, just a bare segment
											log_threshold	=>	log_threshold + 1);

										-- END POINT:
										net_route.end_point := snap_to_grid (self, point);

										-- Before processing the END point further, it must be validated:
										if valid_for_net_segment (net_route.end_point, log_threshold + 3) then
										
											insert_net_segment (
												module			=> current_active_module,
												sheet			=> current_active_sheet,
												segment			=> (
														start_point	=> net_route.bend_point,
														end_point	=> net_route.end_point,
														others		=> <>), -- no labels and no ports, just a bare segment
												log_threshold	=>	log_threshold + 1);
										
											reset_net_route;
										end if;
									end if;

								end if;
							end if;
							
						when others => null;							
					end case;

				when VERB_MOVE =>

					case noun is
						when NOUN_UNIT =>
							if not unit.being_moved then
								
								-- Set the tool being used for moving the unit:
								unit.tool := MOUSE;
								
								if not clarification_pending then
									find_units (point);
								else
									unit.being_moved := true;
									reset_request_clarification;
								end if;

							else
								-- Finally assign the pointer position to the
								-- currently selected unit:
								et_canvas_schematic_units.finalize_move (
									destination		=> snap_to_grid (self, point),
									log_threshold	=> log_threshold + 1);

							end if;
							
						when others => null;							
					end case;

				when VERB_ROTATE =>

					case noun is
						when NOUN_UNIT =>
							if not clarification_pending then
								rotate_unit (point);
							else
								rotate_selected_unit;
							end if;
							
						when others => null;
					end case;

					
				when others => null; -- CS
			end case;
			
		end left_button;

		procedure right_button is begin
			case verb is
				when VERB_DELETE =>
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
							et_schematic.pac_shapes.next_bend_style (net_route);
							
						when others => null;							
					end case;
					
				when VERB_MOVE =>
					case noun is
						when NOUN_UNIT =>
							if clarification_pending then
								clarify_unit;
							end if;

						when others => null;							
					end case;

				when VERB_ROTATE =>
					case noun is
						when NOUN_UNIT =>
							if clarification_pending then
								clarify_unit;
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
	end button_pressed;
	
end et_canvas_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
