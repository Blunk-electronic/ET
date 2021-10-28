------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             DRAW SYMBOL                                  --
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

separate (et_canvas_schematic)

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
	preview			: in boolean := false)
is
	use et_symbols;
	use pac_geometry_2;
	use pac_text;
	
	use pac_lines;
	use pac_arcs;
	use pac_circles;
	use pac_ports;
	use pac_texts;
	
	type type_line is new pac_geometry_2.type_line with null record;
	type type_arc is new pac_geometry_2.type_arc with null record;		
	type type_circle is new pac_geometry_2.type_circle with null record;
	
	procedure draw_line (c : in pac_lines.cursor) is 
		-- Take a copy of the given line:
		line : type_line := (pac_geometry_2.type_line (element (c)) with null record);
	begin
		rotate_by (line, unit_rotation);
		move_by (line, to_distance_relative (unit_position));
		set_line_width (context.cr, type_view_coordinate (element (c).width));
		draw_line (in_area, context, line, element (c).width, self.frame_height);
	end draw_line;

	procedure draw_arc (c : in pac_arcs.cursor) is 
		-- Take a copy of the given arc:
		arc : type_arc := (pac_geometry_2.type_arc (element (c)) with null record);
	begin
		rotate_by (arc, unit_rotation);
		move_by (arc, to_distance_relative (unit_position));
		set_line_width (context.cr, type_view_coordinate (element (c).width));
		draw_arc (in_area, context, arc, element (c).width, self.frame_height);
	end draw_arc;

	procedure draw_circle (c : in pac_circles.cursor) is 
		circle : type_circle := (pac_geometry_2.type_circle (element (c)) with null record);
	begin
		rotate_by (circle, unit_rotation);
		move_by (circle, to_distance_relative (unit_position));
		set_line_width (context.cr, type_view_coordinate (element (c).width));

		-- the circle is not filled -> actual "filled" is NO
		draw_circle (in_area, context, circle, NO, element (c).width, self.frame_height);
	end draw_circle;

	procedure draw_port (c : in pac_ports.cursor) is
		start_point			: type_point := element (c).position;
		end_point			: type_point := element (c).position;

		line : type_line;
		circle : type_circle;
		
		pos_port_name		: type_point;
		pos_terminal_name	: type_point;
		
		procedure draw_port_name is
			use et_text;
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
			move_by (pos_port_name, to_distance_relative (unit_position));
			
			set_color_symbols (context.cr, brightness);

			draw_text (
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
				set (axis => Y, value => get_y (start_point) + terminal_name_spacing_line, point => pos_terminal_name);
				alignment.horizontal := RIGHT;

			elsif rotation_total = 90.0 or rotation_total = -270.0 then
				set (axis => X, value => get_x (start_point) - terminal_name_spacing_line, point => pos_terminal_name);
				alignment.horizontal := RIGHT;
				
			elsif rotation_total = 180.0 or rotation_total = -180.0 then
				set (axis => Y, value => get_y (start_point) + terminal_name_spacing_line, point => pos_terminal_name);
				alignment.horizontal := LEFT;
				
			elsif rotation_total = -90.0 or rotation_total = 270.0 then
				set (axis => X, value => get_x (start_point) - terminal_name_spacing_line, point => pos_terminal_name);
				alignment.horizontal := LEFT;
				
			else
				raise constraint_error; -- CS should never happen
			end if;

			-- Move the name by the unit position:
			move_by (pos_terminal_name, to_distance_relative (unit_position));
			
			set_color_symbols (context.cr, brightness);

			-- Get the properties of the port. Properties is a record that provides
			-- the terminal name. Other things of properties are not relevant here:
			properties := et_schematic_ops.port_properties (
				module_cursor	=> current_active_module,
				device_name		=> device_name,
				unit_name		=> unit_name,
				port_name		=> key (c));

			draw_text (
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
		set_color_symbols (context.cr, brightness);
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
			set (axis => X, value => get_x (start_point) - element (c).length, point => end_point);

			-- compute the position of the port name:
			pos_port_name := end_point;
			set (axis => X, value => get_x (end_point) - port_name_spacing, point => pos_port_name);

			-- Compute the position of the origin of the terminal name regarding its distance
			-- from the start point:
			pos_terminal_name := start_point;				
			set (axis => X, value => get_x (start_point) - terminal_name_spacing_start, point => pos_terminal_name);
			
		elsif element (c).rotation = 90.0 then -- end point points downwards
			set (axis => Y, value => get_y (start_point) - element (c).length, point => end_point);

			-- compute the position of the port name:
			pos_port_name := end_point;
			set (axis => Y, value => get_y (end_point) - port_name_spacing, point => pos_port_name);

			-- Compute the position of the origin of the terminal name regarding its distance
			-- from the start point:
			pos_terminal_name := start_point;				
			set (axis => Y, value => get_y (start_point) - terminal_name_spacing_start, point => pos_terminal_name);
			
		elsif element (c).rotation = 180.0 then  -- end point points to the left
			set (axis => X, value => get_x (start_point) + element (c).length, point => end_point);

			-- compute the position of the port name:
			pos_port_name := end_point;
			set (axis => X, value => get_x (end_point) + port_name_spacing, point => pos_port_name);

			-- Compute the position of the origin of the terminal name regarding its distance
			-- from the start point:
			pos_terminal_name := start_point;				
			set (axis => X, value => get_x (start_point) + terminal_name_spacing_start, point => pos_terminal_name);
			
		elsif element (c).rotation = 270.0 or element (c).rotation = -90.0 then -- end point points upwards
			set (axis => Y, value => get_y (start_point) + element (c).length, point => end_point);

			-- compute the position of the port name:
			pos_port_name := end_point;
			set (axis => Y, value => get_y (end_point) + port_name_spacing, point => pos_port_name);

			-- Compute the position of the origin of the terminal name regarding its distance
			-- from the start point:
			pos_terminal_name := start_point;
			set (axis => Y, value => get_y (start_point) + terminal_name_spacing_start, point => pos_terminal_name);
			
		else
			raise constraint_error; -- CS do something helpful. should never happen
		end if;

		-- Rotate the start and end point by rotation of unit:
		rotate_by (start_point, unit_rotation);
		rotate_by (end_point, unit_rotation);

		line.start_point := start_point;
		line.end_point := end_point;
		move_by (line, to_distance_relative (unit_position));
		
		-- Draw the line of the port:
		draw_line (in_area, context, line, port_line_width, self.frame_height);


		-- Draw the circle around a port if the layer is enabled:
		if ports_enabled then
		
			-- The start point of the port must have a small green circle around it.
			-- set color and line width
			set_color_ports (context.cr, brightness);
			set_line_width (context.cr, type_view_coordinate (port_circle_line_width));

			circle.center := line.start_point;
			circle.radius := port_circle_radius;

			-- the circle is not filled -> argument "filled" is NO
			draw_circle (in_area, context, circle, NO, port_circle_line_width, self.frame_height);

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

		-- If this is a preview, then no terminal name is to be drawn.
		-- Otherwise, draw terminal name:
		-- Draw terminal name if this is the symbol of a real device. 
		-- Virtual symbols do not have terminal names.
		if not preview then
			if symbol.appearance = PCB and then element (c).terminal_name_visible = YES then
				draw_terminal_name;
			end if;
		end if;
		
	end draw_port;

	-- This procedure draws fixed documentational texts like "MUX" or "CT16" as they 
	-- are frequently placed inside symbols.
	-- Call this procedure after drawing the symbol body because it
	-- does not change the color to symbol color.
	procedure draw_text (c : in pac_texts.cursor) is 
		p : type_point := element (c).position;
	begin
		-- Rotate the position of the text.
		-- This adds the unit_rotation to the given rotation.
		rotate_by (p, unit_rotation);

		-- Move text by unit position
		move_by (p, to_distance_relative (unit_position));
		
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
		p : type_point;
	begin
		set_color_placeholders (context.cr, brightness);
		
		-- DEVICE NAME:
		p := sch_placeholder_name.position;

		--put_line (to_string (device_name) & " " & to_string (unit_name) & " " & to_string (unit_count));

		if device_names_enabled then

			-- Move placeholder by unit position
			move_by (p, to_distance_relative (unit_position));
			
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
				move_by (p, to_distance_relative (unit_position));
				
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
				move_by (p, to_distance_relative (unit_position));
				
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
		ohz : constant type_distance_positive := et_symbols.origin_half_size;
		
		line_horizontal : constant type_line := ( -- from left to right
			start_point		=> type_point (set (
								x => get_x (unit_position) - ohz,
								y => get_y (unit_position))),
			
			end_point		=> type_point (set (
								x => get_x (unit_position) + ohz,
								y => get_y (unit_position))));

		line_vertical : constant type_line := ( -- from bottom to top
			start_point		=> type_point (set (
								x => get_x (unit_position),
								y => get_y (unit_position) - ohz)),
			
			end_point		=> type_point (set (
								x => get_x (unit_position),
								y => get_y (unit_position) + ohz)));

	begin
	-- NOTE: This is about the origin of the symbol !
		set_color_origin (context.cr, brightness);
		set_line_width (context.cr, type_view_coordinate (et_symbols.origin_line_width));
		
		-- NOTE: The origin is never rotated.

		draw_line (in_area, context, line_horizontal, et_symbols.origin_line_width, self.frame_height);
		draw_line (in_area, context, line_vertical, et_symbols.origin_line_width, self.frame_height);
	end draw_origin;
	
begin -- draw_symbol
	
	-- SYMBOL BODY
	set_color_symbols (context.cr, brightness);

	iterate (symbol.shapes.lines, draw_line'access);
	iterate (symbol.shapes.arcs, draw_arc'access);
	iterate (symbol.shapes.circles, draw_circle'access);

	
	-- SYMBOL PORTS
	iterate (symbol.ports, draw_port'access); -- has internal color settings

	-- SYMBOL TEXTS
	set_color_symbols (context.cr, brightness);
	iterate (symbol.texts, draw_text'access);
	
	-- Draw placeholders if this is the symbol of a real device. 
	-- Virtual symbols do not have placeholders.
	if symbol.appearance = PCB then
		draw_placeholders;
	end if;

	-- draw origin (the crosshair) at the center of the symbol
	draw_origin;

end draw_symbol;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
