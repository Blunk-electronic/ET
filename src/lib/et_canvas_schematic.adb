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

	procedure draw_nets (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_texts (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;
	
	procedure draw_units (
		self	: not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_symbol (
		self		: not null access type_view;
		in_area		: in type_rectangle := no_rectangle;
		context 	: in type_draw_context;
		
		symbol		: in et_symbols.type_symbol;

		device_name		: in et_devices.type_name;
		device_value	: in et_devices.type_value.bounded_string; -- like 100R or TL084
		device_purpose	: in et_devices.type_purpose.bounded_string; -- like "brightness control"
		unit_name		: in et_devices.type_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...
		unit_count		: in et_devices.type_unit_count;
		
		position		: in type_point; -- x/y on the schematic sheet -- CS rename to unit_position
		unit_rotation	: in type_rotation;

		sch_placeholder_name	: in et_symbols.type_text_placeholder;
		sch_placeholder_value	: in et_symbols.type_text_placeholder;
		sch_placeholder_purpose : in et_symbols.type_text_placeholder		
		)
	is
		use et_symbols;
		use et_symbols.pac_shapes;
		use pac_draw_symbols;
		
		use type_lines;
		use type_arcs;
		use type_circles;
		use type_ports;
		use type_texts;

		type type_line is new pac_shapes.type_line with null record;
		type type_arc is new pac_shapes.type_arc with null record;		
		type type_circle is new pac_shapes.type_circle with null record;
		
		procedure draw_line (c : in type_lines.cursor) is 
			-- Take a copy of the given line:
			line : type_line := (pac_shapes.type_line (element (c)) with null record);
		begin
			rotate_by (line, unit_rotation);
			move_by (line, position);
			set_line_width (context.cr, type_view_coordinate (element (c).width));
			draw_line (in_area, context, line, self.frame_height);
		end draw_line;

		procedure draw_arc (c : in type_arcs.cursor) is 
			-- Take a copy of the given arc:
			arc : type_arc := (pac_shapes.type_arc (element (c)) with null record);
		begin
			rotate_by (arc, unit_rotation);
			move_by (arc, position);
			set_line_width (context.cr, type_view_coordinate (element (c).width));
			draw_arc (in_area, context, arc, self.frame_height);
		end draw_arc;

		procedure draw_circle (c : in type_circles.cursor) is 
			circle : type_circle := (pac_shapes.type_circle (element (c)) with null record);
		begin
			rotate_by (circle, unit_rotation);
			move_by (circle, position);
			set_line_width (context.cr, type_view_coordinate (element (c).width));

			-- the circle is not filled -> actual "filled" is NO
			draw_circle (in_area, context, circle, NO, self.frame_height);
		end draw_circle;

		procedure draw_port (c : in type_ports.cursor) is
			start_point			: type_point := element (c).position;
			end_point			: type_point := element (c).position;

			line : type_line;
			circle : type_circle;
			
			pos_port_name		: type_point;
			pos_terminal_name	: type_point;
			
			procedure draw_port_name is
				use et_text;
				use pac_text;

				-- The vertical alignment is untouched and is always CENTER.
				-- The horizontal alignment depends on the total rotation
				-- which is a sum of port rotation and unit rotation.
				alignment : type_text_alignment := (horizontal => center, vertical => center);
				rotation_total : constant type_rotation := add (element (c).rotation, unit_rotation);
			begin
				if rotation_total = 0.0 or rotation_total = 360.0 or rotation_total = -360.0 then
					alignment.horizontal := RIGHT;

				elsif rotation_total = 90.0 or rotation_total = -270.0 then
					alignment.horizontal := RIGHT;
					
				elsif rotation_total = 180.0 or rotation_total = -180.0 then
					alignment.horizontal := LEFT;
					
				elsif rotation_total = -90.0 or rotation_total = 270.0 then
					alignment.horizontal := LEFT;
					
				else
					raise constraint_error; -- CS should never happen
				end if;

				-- Rotate the position of the port name by the unit rotation:
				rotate_by (pos_port_name, unit_rotation);

				-- Move the name by the unit position:
				move_by (pos_port_name, position);
				
				set_color_symbols (context.cr);

				pac_draw_symbols.draw_text (
					area		=> in_area,
					context		=> context,
					content		=> to_content (to_string (key (c))),
					size		=> element (c).port_name_size,
					font		=> et_symbols.text_font,
					position	=> pos_port_name,
					origin		=> false,  -- no origin required

					-- Text rotation about its anchor point.
					-- This is documentational text. Its rotation must
					-- be snapped to either HORIZONAL or VERTICAL so that
					-- it is readable from the front or the right.
					rotation	=> to_rotation (snap (rotation_total)),
					alignment	=> alignment,
					height		=> self.frame_height);

			end draw_port_name;

			procedure draw_terminal_name is
				use et_text;
				use pac_text;

				-- The vertical alignment is untouched and is always BOTTOM.
				-- The horizontal alignment depends on the total rotation
				-- which is a sum of port rotation and unit rotation.
				alignment : type_text_alignment := (horizontal => CENTER, vertical => BOTTOM);
				rotation_total : constant type_rotation := add (element (c).rotation, unit_rotation);

				use et_terminals;
				use et_devices;
				properties : type_port_properties_access;
			begin
				-- Rotate the position of the terminal name by the unit rotation:
				rotate_by (pos_terminal_name, unit_rotation);
				
				-- Compute the position of the origin of the terminal name regarding 
				-- its distance from the line of the port:
				if rotation_total = 0.0 or rotation_total = 360.0 or rotation_total = -360.0 then
					set (axis => Y, value => y (start_point) + terminal_name_spacing_line, point => pos_terminal_name);
					alignment.horizontal := RIGHT;

				elsif rotation_total = 90.0 or rotation_total = -270.0 then
					set (axis => X, value => x (start_point) - terminal_name_spacing_line, point => pos_terminal_name);
					alignment.horizontal := RIGHT;
					
				elsif rotation_total = 180.0 or rotation_total = -180.0 then
					set (axis => Y, value => y (start_point) + terminal_name_spacing_line, point => pos_terminal_name);
					alignment.horizontal := LEFT;
					
				elsif rotation_total = -90.0 or rotation_total = 270.0 then
					set (axis => X, value => x (start_point) - terminal_name_spacing_line, point => pos_terminal_name);
					alignment.horizontal := LEFT;
					
				else
					raise constraint_error; -- CS should never happen
				end if;

				-- Move the name by the unit position:
				move_by (pos_terminal_name, position);
				
				set_color_symbols (context.cr);

				-- Get the properties of the port. Properties is a record that provides
				-- the terminal name. Other things of properties are not relevant here:
				properties := et_schematic_ops.port_properties (
					module_cursor	=> current_active_module,
					device_name		=> device_name,
					unit_name		=> unit_name,
					port_name		=> key (c));

				pac_draw_symbols.draw_text (
					area		=> in_area,
					context		=> context,
					content		=> to_content (to_string (properties.terminal)), -- H4, 1, 16
					size		=> element (c).terminal_name_size,
					font		=> et_symbols.text_font,
					position	=> pos_terminal_name,
					origin		=> false,  -- no origin required

					-- Text rotation about its anchor point.
					-- This is documentational text. Its rotation must
					-- be snapped to either HORIZONAL or VERTICAL so that
					-- it is readable from the front or the right.
					rotation	=> to_rotation (snap (rotation_total)),
					alignment	=> alignment,
					height		=> self.frame_height);

			end draw_terminal_name;
			
		begin -- draw_port
			set_color_symbols (context.cr);
			set_line_width (context.cr, type_view_coordinate (et_symbols.port_line_width));
			
			-- Compute following positions according to port rotation and length:
			-- - end point of port
			-- - position of port name
			-- - position of terminal name (Distance from start point only.
			--   distance from line of port will be computed later 
			--   by procedure draw_terminal_name.)
			--
			-- NOTE: These computations leave the rotation of the unit outside. For the moment we
			-- assume the unit is not rotated. We look at the default rotation of the ports.
			-- The the final port positions will be computed later.
			if element (c).rotation = 0.0 then -- end point points to the left
				set (axis => X, value => x (start_point) - element (c).length, point => end_point);

				-- compute the position of the port name:
				pos_port_name := end_point;
				set (axis => X, value => x (end_point) - port_name_spacing, point => pos_port_name);

				-- Compute the position of the origin of the terminal name regarding its distance
				-- from the start point:
				pos_terminal_name := start_point;				
				set (axis => X, value => x (start_point) - terminal_name_spacing_start, point => pos_terminal_name);
				
			elsif element (c).rotation = 90.0 then -- end point points downwards
				set (axis => Y, value => y (start_point) - element (c).length, point => end_point);

				-- compute the position of the port name:
				pos_port_name := end_point;
				set (axis => Y, value => y (end_point) - port_name_spacing, point => pos_port_name);

				-- Compute the position of the origin of the terminal name regarding its distance
				-- from the start point:
				pos_terminal_name := start_point;				
				set (axis => Y, value => y (start_point) - terminal_name_spacing_start, point => pos_terminal_name);
				
			elsif element (c).rotation = 180.0 then  -- end point points to the left
				set (axis => X, value => x (start_point) + element (c).length, point => end_point);

				-- compute the position of the port name:
				pos_port_name := end_point;
				set (axis => X, value => x (end_point) + port_name_spacing, point => pos_port_name);

				-- Compute the position of the origin of the terminal name regarding its distance
				-- from the start point:
				pos_terminal_name := start_point;				
				set (axis => X, value => x (start_point) + terminal_name_spacing_start, point => pos_terminal_name);
				
			elsif element (c).rotation = 270.0 or element (c).rotation = -90.0 then -- end point points upwards
				set (axis => Y, value => y (start_point) + element (c).length, point => end_point);

				-- compute the position of the port name:
				pos_port_name := end_point;
				set (axis => Y, value => y (end_point) + port_name_spacing, point => pos_port_name);

				-- Compute the position of the origin of the terminal name regarding its distance
				-- from the start point:
				pos_terminal_name := start_point;
				set (axis => Y, value => y (start_point) + terminal_name_spacing_start, point => pos_terminal_name);
				
			else
				raise constraint_error; -- CS do something helpful. should never happen
			end if;

			-- Rotate the start and end point by rotation of unit:
			rotate_by (start_point, unit_rotation);
			rotate_by (end_point, unit_rotation);

			line.start_point := start_point;
			line.end_point := end_point;
			move_by (line, position);
			
			-- Draw the line of the port:
			draw_line (in_area, context, line, self.frame_height);


			-- Draw the circle around a port if the layer is enabled:
			if ports_enabled then
			
				-- The start point of the port must have a small green circle around it.
				-- set color and line width
				set_color_ports (context.cr);
				set_line_width (context.cr, type_view_coordinate (port_circle_line_width));

				circle.center := line.start_point;
				circle.radius := port_circle_radius;

				-- the circle is not filled -> actual "filled" is NO
				draw_circle (in_area, context, circle, NO, self.frame_height);

				-- CS draw port direction, weakness, power level ?
				-- probably better in draw_terminal_name or draw_port_name ?

	-- 				use properties := schematic_ops.port_properties (
	-- 					module_cursor	=> current_active_module,
	-- 					device_name		=> device_name,
	-- 					unit_name		=> unit_name,
	-- 					port_name		=> key (c));
				
			end if;
			
			-- draw port name
			if element (c).port_name_visible = YES then
				draw_port_name;
			end if;
			
			-- Draw terminal name if this is the symbol of a real device. 
			-- Virtual symbols do not have terminal names.
			if symbol.appearance = PCB and then element (c).terminal_name_visible = YES then
				draw_terminal_name;
			end if;
			
		end draw_port;

		-- This procedure draws fixed documentational texts like "MUX" or "CT16" as they 
		-- are frequently placed inside symbols.
		-- Call this procedure after drawing the symbol body because it
		-- does not change the color to symbol color.
		procedure draw_text (c : in type_texts.cursor) is 
			use pac_text;
			p : type_point := element (c).position;
		begin
			-- Rotate the position of the text.
			-- This adds the unit_rotation to the given rotation.
			rotate_by (p, unit_rotation);

			-- Move text by unit position
			move_by (p, position);
			
			draw_text 
				(
				area		=> in_area,
				context		=> context,
				content		=> element (c).content,
				size		=> element (c).size,
				font		=> et_symbols.text_font,
				position	=> p,
				origin		=> false, -- no origin required
				
				-- Text rotation around its anchor point.
				-- This is documentational text. Its rotation must
				-- be snapped to either HORIZONAL or VERTICAL so that
				-- it is readable from the front or the right.
				rotation	=> to_rotation (snap (element (c).rotation + unit_rotation)),

				alignment	=> element (c).alignment,
				height		=> self.frame_height
				);
		end draw_text;

		-- This procedure draws text placeholders for device name, value and purpose:
		procedure draw_placeholders is 
			use et_devices;
			use pac_text;
			
			p : type_point;
		begin
			set_color_placeholders (context.cr);
			
			-- DEVICE NAME:
			p := sch_placeholder_name.position;

			--put_line (to_string (device_name) & " " & to_string (unit_name) & " " & to_string (unit_count));

			if device_names_enabled then

				-- Move placeholder by unit position
				move_by (p, position);
				
				draw_text (
					area		=> in_area,
					context		=> context,
					content		=> to_content (to_full_name (device_name, unit_name, unit_count)), -- IC4.PWR
					size		=> symbol.name.size,
					font		=> et_symbols.name_font,
					position	=> p,
					origin		=> true, -- origin required
					
					-- Text rotation around its anchor point.
					-- NOTE: No snapping to HORIZONAL or VERTICAL required here.
					-- This has been done in schematic_ops.rotate_unit already.
					rotation	=> to_rotation (sch_placeholder_name.rotation),
					
					alignment	=> sch_placeholder_name.alignment,
					height		=> self.frame_height
					);
			end if;
			
			-- VALUE
			if device_values_enabled then
				
				-- The value may be empty. We do not draw it in this case:
				if not is_empty (device_value) then

					p := sch_placeholder_value.position;

					-- Move text by unit position
					move_by (p, position);
					
					draw_text (
						area		=> in_area,
						context		=> context,
						content		=> to_content (to_string (device_value)), -- 100R
						size		=> symbol.value.size,
						font		=> et_symbols.value_font,
						position	=> p,
						origin		=> true, -- origin required
						
						-- Text rotation around its anchor point.
						-- NOTE: No snapping to HORIZONAL or VERTICAL required here.
						-- This has been done in schematic_ops.rotate_unit already.
						rotation	=> to_rotation (sch_placeholder_value.rotation),

						alignment	=> sch_placeholder_value.alignment,
						height		=> self.frame_height
						);
				end if;
			end if;
			
			-- PURPOSE
			if device_purposes_enabled then
			
				-- The purpose may be empty. We do not draw it in this case:
				if not is_empty (device_purpose) then

					p := sch_placeholder_purpose.position;

					-- Move text by unit position
					move_by (p, position);
					
					draw_text (
						area		=> in_area,
						context		=> context,
						content		=> to_content (to_string (device_purpose)), -- "brightness control"
						size		=> symbol.purpose.size,
						font		=> et_symbols.purpose_font,
						position	=> p,
						origin		=> true, -- origin required
						
						-- Text rotation around its anchor point.
						-- NOTE: No snapping to HORIZONAL or VERTICAL required here.
						-- This has been done in schematic_ops.rotate_unit already.
						rotation	=> to_rotation (sch_placeholder_purpose.rotation),

						alignment	=> sch_placeholder_purpose.alignment,
						height		=> self.frame_height
						);
				end if;
			end if;
			
		end draw_placeholders;

		procedure draw_origin is 
			type type_line is new et_schematic.pac_shapes.type_line with null record;
			
			line_horizontal : constant type_line := ( -- from left to right
				start_point		=> type_point (set (x => x (position) - origin_half_size, y => y (position))),
				end_point		=> type_point (set (x => x (position) + origin_half_size, y => y (position))));

			line_vertical : constant type_line := ( -- from bottom to top
				start_point		=> type_point (set (x => x (position), y => y (position) - origin_half_size)),
				end_point		=> type_point (set (x => x (position), y => y (position) + origin_half_size)));

		begin
		-- NOTE: This is about the origin of the symbol !
			set_color_origin (context.cr);
			set_line_width (context.cr, type_view_coordinate (origin_line_width));
			
			-- NOTE: The origin is never rotated.

			pac_draw_misc.draw_line (in_area, context, line_horizontal, self.frame_height);
			pac_draw_misc.draw_line (in_area, context, line_vertical, self.frame_height);
		end draw_origin;
		
	begin -- draw_symbol
		
		-- SYMBOL BODY
		set_color_symbols (context.cr);

		iterate (symbol.shapes.lines, draw_line'access);
		iterate (symbol.shapes.arcs, draw_arc'access);
		iterate (symbol.shapes.circles, draw_circle'access);

		
		-- SYMBOL PORTS
		iterate (symbol.ports, draw_port'access); -- has internal color settings

		-- SYMBOL TEXTS
		set_color_symbols (context.cr);
		iterate (symbol.texts, draw_text'access);
		
		-- Draw placeholders if this is the symbol of a real device. 
		-- Virtual symbols do not have placeholders.
		if symbol.appearance = PCB then
			draw_placeholders;
		end if;

		-- draw origin (the crosshair) at the center of the symbol
		draw_origin;

	end draw_symbol;

	
	procedure draw_units_2 (
		self	: not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;
	
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

		-- draw objects inside the drawing frame:
	--  draw_units (self, area_shifted, context);
		
		save (context.cr);
			
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			convert_x (self.frame_bounding_box.x),
			convert_y (self.frame_bounding_box.y));

		draw_units_2 (self, area_shifted, context);
		
		draw_frame (self, area_shifted, context);

		-- CS: rework the order of drawing layers so that top layers
		-- always obscure layers underneath.
		
		-- Draw nets if layer is enabled:
		if nets_enabled then
			draw_nets (self, area_shifted, context);
		end if;

		-- Draw texts if layer is enabled:
		if texts_enabled then
			draw_texts (self, area_shifted, context);
		end if;
		
		draw_submodules (self, area_shifted, context);

		-- The cursor is drawn last so that is in the foreground:
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

	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance)
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
		y		: in type_distance)
		return type_distance is 
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
	
	

	-- Deletes a net segment in the vicinity of given point.
	-- If more than one segment near point found, then it sets the
	-- cursor selected_segment to the first segment and requests
	-- for clarification.
	procedure delete_net_segment (point : in type_point) is 
		use et_schematic_ops.nets;
		use pac_selected_segments;
		segment_cursor : pac_selected_segments.cursor;
	begin
		log (text => "deleting net segment ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all segments in the vicinity of the given point:
		selected_segments := collect_segments (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		-- evaluate the number of segments found here:
		case length (selected_segments) is
			when 0 =>
				reset_request_clarification;
				
			when 1 =>
				segment_cursor := selected_segments.first;
			
				delete_selected_segment (
					module_cursor	=> current_active_module,
					segment			=> element (segment_cursor),
					log_threshold	=> log_threshold + 1);

				reset_request_clarification;
				set_status (status_preamble_click_left & "delete net segment." & status_hint_for_abort);
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first segment
				selected_segment := selected_segments.first;
		end case;
		
		log_indentation_down;
	end delete_net_segment;

	-- Advances cursor selected_segment to next segment in list selected_segments.
	procedure clarify_net_segment is
		use et_schematic;
		use et_schematic_ops.nets;
		use pac_selected_segments;
		s : type_net_segments.cursor;
	begin
		-- On every call of this procedure we must advance from one
		-- segment to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_segment
		-- moves back to the start of the segment list.
		if next (selected_segment) /= pac_selected_segments.no_element then
			next (selected_segment);
		else
			selected_segment := selected_segments.first;
		end if;

		-- show the selected segment in the status bar
		s := element (selected_segment).segment;

		-- CS highlight the selected segment
		
		set_status (to_string (s));
	end clarify_net_segment;

	-- Deletes the net segment being pointed at by cursor selected_segment.
	procedure delete_selected_net_segment is
		use et_schematic_ops.nets;
		use pac_selected_segments;
	begin
		log (text => "deleting net segment after clarification ...", level => log_threshold);
		log_indentation_up;

		delete_selected_segment (
			module_cursor	=> current_active_module,
			segment			=> element (selected_segment),
			log_threshold	=> log_threshold + 1);

		reset_request_clarification;
		set_status (status_preamble_click_left & "delete net segment." & status_hint_for_abort);
		
		log_indentation_down;
	end delete_selected_net_segment;



	
	-- Deletes a unit in the vicinity of given point.
	-- If more than one unit near point found, then it sets the
	-- cursor selected_unit to the first unit and requests
	-- for clarification.
	procedure delete_unit (point : in type_point) is 
		use et_schematic_ops.units;
		use pac_selected_units;
		unit_cursor : pac_selected_units.cursor;
	begin
		log (text => "deleting unit ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all units in the vicinity of the given point:
		selected_units := collect_units (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		-- evaluate the number of units found here:
		case length (selected_units) is
			when 0 =>
				reset_request_clarification;
				
			when 1 =>
				unit_cursor := selected_units.first;
			
				delete_selected_unit (
					module_cursor	=> current_active_module,
					unit			=> element (unit_cursor),
					log_threshold	=> log_threshold + 1);

				reset_request_clarification;
				set_status (status_preamble_click_left & "delete unit." & status_hint_for_abort);
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first unit
				selected_unit := selected_units.first;
		end case;
		
		log_indentation_down;
	end delete_unit;

	-- Advances cursor selected_unit to next unit in list selected_units.
	procedure clarify_unit is
		use et_schematic;
		use et_schematic_ops.units;
		use pac_selected_units;
		u : type_units.cursor;
	begin
		-- On every call of this procedure we must advance from one
		-- unit to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_unit
		-- moves back to the start of the unit list.
		if next (selected_unit) /= pac_selected_units.no_element then
			next (selected_unit);
		else
			selected_unit := selected_units.first;
		end if;

		-- show the selected unit in the status bar
		u := element (selected_unit).unit;

		-- CS highlight the selected unit
		
		set_status (to_string (u));
	end clarify_unit;
	
	-- Deletes the unit being pointed at by cursor selected_unit.
	procedure delete_selected_unit is
		use et_schematic_ops.units;
		use pac_selected_units;
	begin
		log (text => "deleting unit after clarification ...", level => log_threshold);
		log_indentation_up;

		delete_selected_unit (
			module_cursor	=> current_active_module,
			unit			=> element (selected_unit),
			log_threshold	=> log_threshold + 1);

		reset_request_clarification;
		set_status (status_preamble_click_left & "delete unit." & status_hint_for_abort);
		
		log_indentation_down;
	end delete_selected_unit;


	
	
	procedure evaluate_key (
		self	: not null access type_view;
		key		: in gdk_key_type) is

		use gdk.types;
		use gdk.types.keysyms;

		use et_modes;

		procedure delete is begin
			case key is
				when GDK_LC_u =>
					noun := NOUN_UNIT;
					set_status (status_preamble_click_left & "delete unit." & status_hint_for_abort);
					
				when GDK_LC_n =>
					noun := NOUN_NET;
					set_status (status_preamble_click_left & "delete net segment." & status_hint_for_abort);

				when GDK_Return =>

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

		procedure draw is begin
			case key is
				when GDK_LC_n =>
					noun := NOUN_NET;
					status_clear;
					
				when others => status_noun_invalid;
			end case;
		end draw;
		
	begin -- evaluate_key
		
-- 		put_line ("schematic: evaluating other key ...");
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
						when VERB_DELETE => delete;

						when VERB_DRAW => draw;

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

						when others =>
							null;
							
					end case;

				when VERB_DRAW =>

					case noun is
						when NOUN_NET => null;

						when others =>
							null;
							
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

						when others =>
							null;
							
					end case;

				when others => null; -- CS
			end case;

		end right_button;
			
	begin -- button_pressed
		log (text => to_string (button) & " at" & to_string (point), level => log_threshold);
		
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
