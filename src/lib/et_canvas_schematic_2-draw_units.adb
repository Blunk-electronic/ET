------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              DRAW UNITS                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
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

with et_colors;							use et_colors;

with et_text;
with et_alignment;
with et_symbol_text;
with et_symbol_model;
with et_port_names;
with et_port_visibility;
with et_symbol_library;					use et_symbol_library;
with et_symbol_name;
with et_symbol_ports;
with et_unit_name;						use et_unit_name;
with et_units;							use et_units;
with et_device_appearance;				use et_device_appearance;
with et_device_purpose;					use et_device_purpose;
with et_device_model;					use et_device_model;
with et_device_library;					use et_device_library;
with et_device_model_names;
with et_device_value;					use et_device_value;
with et_device_name;					use et_device_name;
with et_devices_electrical;				use et_devices_electrical;

with et_terminals;

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;

with et_schematic_ops;
with et_schematic_ops.units;
with et_display.schematic;				use et_display.schematic;

with et_canvas_schematic_preliminary_object; 	use et_canvas_schematic_preliminary_object;


separate (et_canvas_schematic_2)

procedure draw_units is
	
	use et_colors.schematic;
	use et_canvas_schematic_units;

	
	procedure draw_symbol (
		symbol			: in et_symbol_model.type_symbol;
		device_name		: in type_device_name := (others => <>);
		device_value	: in pac_device_value.bounded_string := to_value (""); -- like 100R or TL084
		device_purpose	: in pac_device_purpose.bounded_string := to_purpose (""); -- like "brightness control"
		unit_name		: in pac_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...

		-- The unit count is required in order to decide whether a suffix
		-- for the unit name is to be drawn. If the device has only a single unit,
		-- then the unit name will not be drawn:
		unit_count		: in type_unit_count;

		-- CS: Unit position and rotation should be unified
		-- to a single argument ?:
		unit_position	: in type_vector_model; -- x/y on the schematic sheet
		unit_rotation	: in type_rotation := zero_rotation;
		
		sch_placeholder_name	: in type_text_placeholder;
		sch_placeholder_value	: in type_text_placeholder;
		sch_placeholder_purpose : in type_text_placeholder;
		brightness		: in type_brightness := NORMAL;
		preview			: in boolean := false)
	is
		use et_symbol_model;
		use et_symbol_ports;
		use pac_text_schematic;
		use pac_geometry_2;
		
		use pac_symbol_lines;
		use pac_symbol_arcs;
		use pac_symbol_circles;
		use pac_ports;

		use et_symbol_text;
		use pac_symbol_texts;
		
		
		procedure draw_line (c : in pac_symbol_lines.cursor) is 
			l : type_symbol_line renames element (c);
			p : pac_geometry_2.type_position; 
			-- CS rework, use unit position (incl. x,y,rotation)
			-- instead.
		begin
			p.place := unit_position;
			p.rotation := unit_rotation;

			draw_line (l, p, l.width, do_stroke => true);
		end draw_line;

		
		procedure draw_arc (c : in pac_symbol_arcs.cursor) is 
			a : type_symbol_arc renames element (c);
			p : pac_geometry_2.type_position;
			-- CS rework, use unit position (incl. x,y,rotation)
			-- instead.
		begin
			p.place := unit_position;
			p.rotation := unit_rotation;

			draw_arc (a, p, a.width, do_stroke => true);
		end draw_arc;

		
		procedure draw_circle (c : in pac_symbol_circles.cursor) is 
			i : type_symbol_circle renames element (c);
			p : pac_geometry_2.type_position;
			-- CS rework, use unit position (incl. x,y,rotation)
			-- instead.
		begin
			p.place := unit_position;
			p.rotation := unit_rotation;
  
			-- the circle is not filled -> actual "filled" is NO
			draw_circle (i, p, NO, i.width, do_stroke => true);
		end draw_circle;

		
		procedure draw_port (c : in pac_ports.cursor) is
			A : type_vector_model := element (c).position;
			B : type_vector_model := element (c).position;

			line : type_line;
			circle : type_circle;
			
			pos_port_name		: type_vector_model;
			pos_terminal_name	: type_vector_model;

			use et_alignment;
			use et_port_visibility;
			
			
			procedure draw_port_name is
				use et_port_names;
				use et_text;
				-- The vertical alignment is untouched and is always CENTER.
				-- The horizontal alignment depends on the total rotation
				-- which is a sum of port rotation and unit rotation.
				alignment : type_text_alignment := (horizontal => ALIGN_CENTER, vertical => ALIGN_CENTER);
				rotation_total : constant type_rotation := add (element (c).rotation, unit_rotation);

				use pac_draw_text;
			begin
				if rotation_total = 0.0 or rotation_total = 360.0 or rotation_total = -360.0 then
					alignment.horizontal := ALIGN_RIGHT;

				elsif rotation_total = 90.0 or rotation_total = -270.0 then
					alignment.horizontal := ALIGN_RIGHT;
					
				elsif rotation_total = 180.0 or rotation_total = -180.0 then
					alignment.horizontal := ALIGN_LEFT;
					
				elsif rotation_total = -90.0 or rotation_total = 270.0 then
					alignment.horizontal := ALIGN_LEFT;
					
				else
					raise constraint_error; -- CS should never happen
				end if;

				-- Rotate the position of the port name by the unit rotation:
				rotate_by (pos_port_name, unit_rotation);

				-- Move the name by the unit position:
				move_by (pos_port_name, unit_position);
				
				set_color_symbols (brightness);

				draw_text (
					content		=> to_content (to_string (key (c))),
					size		=> element (c).port_name_size,
					font		=> et_symbol_text.text_font,
					anchor		=> pos_port_name,
					origin		=> false,  -- no origin required

					-- Text rotation about its anchor point.
					-- This is documentational text. Its rotation must
					-- be snapped to either HORIZONAL or VERTICAL so that
					-- it is readable from the front or the right.
					rotation	=> to_rotation (snap (rotation_total)),
					alignment	=> alignment);

			end draw_port_name;


			
			procedure draw_terminal_name is
				use et_text;
				-- The vertical alignment is untouched and is always BOTTOM.
				-- The horizontal alignment depends on the total rotation
				-- which is a sum of port rotation and unit rotation.
				alignment : type_text_alignment := (horizontal => ALIGN_CENTER, vertical => ALIGN_BOTTOM);
				rotation_total : constant type_rotation := add (element (c).rotation, unit_rotation);

				use et_terminals;
				properties : type_port_properties_access;

				use pac_draw_text;
				use et_schematic_ops.units;
			begin
				-- Rotate the position of the terminal name by the unit rotation:
				rotate_by (pos_terminal_name, unit_rotation);
				
				-- Compute the position of the origin of the terminal name regarding 
				-- its distance from the line of the port:
				if rotation_total = 0.0 or rotation_total = 360.0 or rotation_total = -360.0 then
					set (axis => AXIS_Y, value => get_y (A) + terminal_name_spacing_line, point => pos_terminal_name);
					alignment.horizontal := ALIGN_RIGHT;

				elsif rotation_total = 90.0 or rotation_total = -270.0 then
					set (axis => AXIS_X, value => get_x (A) - terminal_name_spacing_line, point => pos_terminal_name);
					alignment.horizontal := ALIGN_RIGHT;
					
				elsif rotation_total = 180.0 or rotation_total = -180.0 then
					set (axis => AXIS_Y, value => get_y (A) + terminal_name_spacing_line, point => pos_terminal_name);
					alignment.horizontal := ALIGN_LEFT;
					
				elsif rotation_total = -90.0 or rotation_total = 270.0 then
					set (axis => AXIS_X, value => get_x (A) - terminal_name_spacing_line, point => pos_terminal_name);
					alignment.horizontal := ALIGN_LEFT;
					
				else
					raise constraint_error; -- CS should never happen
				end if;

				-- Move the name by the unit position:
				move_by (pos_terminal_name, unit_position);
				
				set_color_symbols (brightness);

				-- Get the properties of the port. Properties is a record that provides
				-- the terminal name. Other things of properties are not relevant here:
				properties := get_port_properties (
					module_cursor	=> active_module,
					device_name		=> device_name,
					unit_name		=> unit_name,
					port_name		=> key (c));

				draw_text (
					content		=> to_content (to_string (properties.terminal)), -- H4, 1, 16
					size		=> element (c).terminal_name_size,
					font		=> et_symbol_text.text_font,
					anchor		=> pos_terminal_name,
					origin		=> false,  -- no origin required

					-- Text rotation about its anchor point.
					-- This is documentational text. Its rotation must
					-- be snapped to either HORIZONAL or VERTICAL so that
					-- it is readable from the front or the right.
					rotation	=> to_rotation (snap (rotation_total)),
					alignment	=> alignment);

			end draw_terminal_name;

			
		begin -- draw_port
			
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
				set (axis => AXIS_X, value => get_x (A) - element (c).length, point => B);

				-- compute the position of the port name:
				pos_port_name := B;
				set (axis => AXIS_X, value => get_x (B) - port_name_spacing, point => pos_port_name);

				-- Compute the position of the origin of the terminal name regarding its distance
				-- from the start point:
				pos_terminal_name := A;				
				set (axis => AXIS_X, value => get_x (A) - terminal_name_spacing_start, point => pos_terminal_name);
				
			elsif element (c).rotation = 90.0 then -- end point points downwards
				set (axis => AXIS_Y, value => get_y (A) - element (c).length, point => B);

				-- compute the position of the port name:
				pos_port_name := B;
				set (axis => AXIS_Y, value => get_y (B) - port_name_spacing, point => pos_port_name);

				-- Compute the position of the origin of the terminal name regarding its distance
				-- from the start point:
				pos_terminal_name := A;				
				set (axis => AXIS_Y, value => get_y (A) - terminal_name_spacing_start, point => pos_terminal_name);
				
			elsif element (c).rotation = 180.0 then  -- end point points to the left
				set (axis => AXIS_X, value => get_x (A) + element (c).length, point => B);

				-- compute the position of the port name:
				pos_port_name := B;
				set (axis => AXIS_X, value => get_x (B) + port_name_spacing, point => pos_port_name);

				-- Compute the position of the origin of the terminal name regarding its distance
				-- from the start point:
				pos_terminal_name := A;				
				set (axis => AXIS_X, value => get_x (A) + terminal_name_spacing_start, point => pos_terminal_name);
				
			elsif element (c).rotation = 270.0 or element (c).rotation = -90.0 then -- end point points upwards
				set (axis => AXIS_Y, value => get_y (A) + element (c).length, point => B);

				-- compute the position of the port name:
				pos_port_name := B;
				set (axis => AXIS_Y, value => get_y (B) + port_name_spacing, point => pos_port_name);

				-- Compute the position of the origin of the terminal name regarding its distance
				-- from the start point:
				pos_terminal_name := A;
				set (axis => AXIS_Y, value => get_y (A) + terminal_name_spacing_start, point => pos_terminal_name);
				
			else
				raise constraint_error; -- CS do something helpful. should never happen
			end if;

			
			set_A (line, A);
			set_B (line, B);

			
			-- Draw the line of the port:
			set_color_symbols (brightness);
			draw_line (line, (unit_position, unit_rotation), port_line_width,
				do_stroke => true);


			-- Draw the circle around a port if the layer is enabled:
			if ports_enabled then
				-- put_line ("draw port");
				
				-- The start point of the port must have a small green circle around it.
				-- set color and line width
				set_color_ports (brightness);

				set_center (circle, get_A (line));
				set_radius (circle, port_circle_radius);

				-- the circle is not filled -> argument "filled" is NO
				draw_circle (
					circle		=> circle, 
					pos			=> (unit_position, unit_rotation), 
					filled		=> NO,
					width		=> port_circle_line_width, 
					do_stroke	=> true);

				-- CS draw port direction, weakness, power level ?
				-- probably better in draw_terminal_name or draw_port_name ?

	-- 				use properties := schematic_ops.port_properties (
	-- 					module_cursor	=> active_module,
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
				if symbol.appearance = APPEARANCE_PCB and then element (c).terminal_name_visible = YES then
					draw_terminal_name;
				end if;
			end if;			
		end draw_port;

		
		-- This procedure draws fixed documentational texts like "MUX" or "CT16" as they 
		-- are frequently placed inside symbols.
		-- Call this procedure after drawing the symbol body because it
		-- does not change the color to symbol color.
		procedure draw_text (c : in pac_symbol_texts.cursor) is 
			p : type_vector_model := element (c).position;

			use pac_draw_text;
		begin
			-- Rotate the position of the text.
			-- This adds the unit_rotation to the given rotation.
			rotate_by (p, unit_rotation);

			-- Move text by unit position
			move_by (p, unit_position);
			
			draw_text (
				content		=> element (c).content,
				size		=> element (c).size,
				font		=> et_symbol_text.text_font,
				anchor		=> p,
				origin		=> false, -- no origin required
				
				-- Text rotation around its anchor point.
				-- This is documentational text. Its rotation must
				-- be snapped to either HORIZONAL or VERTICAL so that
				-- it is readable from the front or the right.
				rotation	=> to_rotation (snap (element (c).rotation + unit_rotation)),

				alignment	=> element (c).alignment);
		end draw_text;


		
		-- This procedure draws text placeholders for device name, value and purpose:
		procedure draw_placeholders is 
			use et_text;
			p : type_vector_model;

			use pac_draw_text;
		begin
			set_color_placeholders (brightness);
			
			-- DEVICE NAME:
			p := sch_placeholder_name.position;

			--put_line (to_string (device_name) & " " & to_string (unit_name) & " " & to_string (unit_count));

			if device_names_enabled then

				-- Move placeholder by unit position
				move_by (p, unit_position);
				
				draw_text (
					content		=> to_content (get_full_name (device_name, unit_name, unit_count)), -- IC4.PWR
					size		=> symbol.placeholders.name.size,
					font		=> name_font,
					anchor		=> p,
					origin		=> true, -- origin required
					
					-- Text rotation around its anchor point.
					-- NOTE: No snapping to HORIZONAL or VERTICAL required here.
					-- This has been done in schematic_ops.rotate_unit already.
					rotation	=> to_rotation (sch_placeholder_name.rotation),
					
					alignment	=> sch_placeholder_name.alignment);
			end if;
			
			-- VALUE
			if device_values_enabled then
				
				-- The value may be empty. We do not draw it in this case:
				if not is_empty (device_value) then

					p := sch_placeholder_value.position;

					-- Move text by unit position
					move_by (p, unit_position);
					
					draw_text (
						content		=> to_content (to_string (device_value)), -- 100R
						size		=> symbol.placeholders.value.size,
						font		=> value_font,
						anchor		=> p,
						origin		=> true, -- origin required
						
						-- Text rotation around its anchor point.
						-- NOTE: No snapping to HORIZONAL or VERTICAL required here.
						-- This has been done in schematic_ops.rotate_unit already.
						rotation	=> to_rotation (sch_placeholder_value.rotation),

						alignment	=> sch_placeholder_value.alignment);
				end if;
			end if;
			
			-- PURPOSE
			if device_purposes_enabled then
			
				-- The purpose may be empty. We do not draw it in this case:
				if not is_empty (device_purpose) then

					p := sch_placeholder_purpose.position;

					-- Move text by unit position
					move_by (p, unit_position);
					
					draw_text (
						content		=> to_content (to_string (device_purpose)), -- "brightness control"
						size		=> symbol.placeholders.purpose.size,
						font		=> purpose_font,
						anchor		=> p,
						origin		=> true, -- origin required
						
						-- Text rotation around its anchor point.
						-- NOTE: No snapping to HORIZONAL or VERTICAL required here.
						-- This has been done in schematic_ops.rotate_unit already.
						rotation	=> to_rotation (sch_placeholder_purpose.rotation),

						alignment	=> sch_placeholder_purpose.alignment);
				end if;
			end if;
			
		end draw_placeholders;

		
		
		procedure draw_origin is begin
			-- NOTE: This is about the origin of the symbol !
			-- set_color_origin (brightness);
			
			-- NOTE: The origin is never rotated.
			draw_origin ((unit_position, 0.0));

		end draw_origin;


		
		
	begin -- draw_symbol
		
		-- SYMBOL BODY
		set_color_symbols (brightness);

		iterate (symbol.shapes.lines, draw_line'access);
		iterate (symbol.shapes.arcs, draw_arc'access);
		iterate (symbol.shapes.circles, draw_circle'access);

		
		-- SYMBOL PORTS
		iterate (symbol.ports, draw_port'access); -- has internal color settings

		-- SYMBOL TEXTS
		set_color_symbols (brightness);
		iterate (symbol.texts, draw_text'access);
		
		-- Draw placeholders if this is the symbol of a real device. 
		-- Virtual symbols do not have placeholders.
		if symbol.appearance = APPEARANCE_PCB then
			draw_placeholders;
		end if;

		draw_origin;

	end draw_symbol;




	use pac_devices_sch;
	use pac_units;
	
	-- The name, value and purpose of the current device:
	device_name	: type_device_name; -- like R1, IC100	
	device_value : pac_device_value.bounded_string; -- like 100R or TL084
	device_purpose : pac_device_purpose.bounded_string; -- like "brightness control"

	-- The number of units provided by the current device:
	unit_count	: type_unit_count; -- the total number of units

	-- The name of the current unit:
	unit_name	: pac_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...

	-- The placeholders as given by the schematic:
	sch_placeholder_name	: type_text_placeholder (meaning => NAME);
	sch_placeholder_value	: type_text_placeholder (meaning => VALUE);
	sch_placeholder_purpose : type_text_placeholder (meaning => PURPOSE);

	unit_rotation : type_rotation;

	
	-- This function returns true if the given placeholder has been moved from the
	-- default position and rotation or if the alignment has been changed:
-- 	function moved_by_operator (placeholder : in et_symbol_model.type_text_placeholder)
-- 		return boolean is
-- 		use et_symbol_model;
-- 		use et_text;
-- 		use type et_text.type_text_alignment;
-- 		result : boolean := false;
-- 	begin
-- 		case placeholder.meaning is
-- 			when NAME =>
-- 				if  placeholder.position /= sch_placeholder_name.position or
-- 					placeholder.rotation /= sch_placeholder_name.rotation or
-- 					placeholder.alignment /= sch_placeholder_name.alignment then
-- 					result := true;
-- 				end if;
-- 
-- 			when VALUE =>
-- 				if 	placeholder.position /= sch_placeholder_value.position or
-- 					placeholder.rotation /= sch_placeholder_value.rotation or
-- 					placeholder.alignment /= sch_placeholder_value.alignment then
-- 					result := true;
-- 				end if;
-- 			
-- 			when PURPOSE =>
-- 				if 	placeholder.position /= sch_placeholder_purpose.position or
-- 					placeholder.rotation /= sch_placeholder_purpose.rotation or
-- 					placeholder.alignment /= sch_placeholder_purpose.alignment then
-- 					result := true;
-- 				end if;
-- 		end case;
-- 
-- 		return result;
-- 	end moved_by_operator;

	
	
	
	-- Returns true if the given placeholder is selected.
	function placeholder_is_selected (
		d : in pac_devices_sch.cursor;
		u : in pac_units.cursor)
		return boolean
	is
		use pac_proposed_placeholders;
		use pac_unit_name;
	begin
		-- If there are no selected placeholders at all, then there is nothing to do:
		if is_empty (proposed_placeholders) then
			return false;
		else
			if selected_placeholder /= pac_proposed_placeholders.no_element then
				-- Compare given device and unit name with selected unit:
				if key (d) = key (element (selected_placeholder).device) and then
					key (u) = key (element (selected_placeholder).unit) then
					-- CS: Improvement: compare cursors directly ?
					
					return true;
				else
					return false;
				end if;
			else
				return false;
			end if;
		end if;
	end placeholder_is_selected;



	
	procedure query_devices (
		device_cursor : in pac_devices_sch.cursor) 
	is
		use et_device_model_names;
		
		-- get the model of the current device
		device_model : pac_device_model_file.bounded_string :=
			element (device_cursor).model;	-- ../libraries/devices/transistor/pnp.dev

		unit_position : type_vector_model; -- only x and y relevant

		brightness : type_brightness := NORMAL;


		-- This procedure maps from the given unit cursor
		-- to the actual symbol and draws the symbol:
		procedure draw_unit (
			unit_cursor : in type_unit_cursors) 
		is begin
			draw_symbol (						
				symbol			=> get_symbol (unit_cursor),

				device_name		=> device_name,
				device_value	=> device_value,
				device_purpose	=> device_purpose,

				unit_name		=> unit_name,
				unit_count		=> unit_count,
				
				unit_position	=> unit_position,
				unit_rotation	=> unit_rotation,

				sch_placeholder_name	=> sch_placeholder_name,
				sch_placeholder_value	=> sch_placeholder_value,
				sch_placeholder_purpose => sch_placeholder_purpose,

				brightness		=> brightness);
		end draw_unit;

		
		
		procedure query_units (unit_cursor : in pac_units.cursor) is
			use et_symbol_model;

			-- Get the position of the unit candidate as it is
			-- in the schematic:
			pos : constant type_object_position := get_position (unit_cursor);
			
			device_cursor_lib : pac_devices_lib.cursor;
		begin
			-- get the name of the unit
			unit_name := key (unit_cursor);
			-- put_line (to_string (unit_name));

			-- Get the x/y-position of the unit.
			-- If the unit is selected and being moved, then the x/y position
			-- will be overwritten by the position of the mouse or the cursor.
			unit_position := get_place (pos);

			-- There are two cases when a unit is to be drawn:
			-- 1. The unit is on the current active sheet.
			-- 2. The unit is being moved from one sheet to another sheet.
			
			-- CASE 1: We draw units which are on the active sheet:
			if get_sheet (pos) = active_sheet then
				
				-- The default brightness is NORMAL. If the unit is selected, 
				-- then the brightness will be increased:
				brightness := NORMAL;

				if is_selected (unit_cursor) then
					-- put_line ("selected");
					
					-- increase brightness
					brightness := BRIGHT;

					-- overwrite position if unit is moving
					if is_moving (unit_cursor) then
						unit_position := get_object_tool_position;
					end if;
				end if;

				-- get the rotation of the unit
				unit_rotation := get_rotation (pos);

				
				-- If this is a real device, then get a copy of the 
				-- placeholders of the unit.
				-- NOTE: The position of the placeholders is relative to
				-- the unit position !
				-- If any of the placeholder of the unit is selected
				-- and being moved, then calculate its new relative
				-- position according to the tool being used. Otherwise
				-- the position remains untouched. 
				-- NOTE: There can only be just one placeholder being moved.
				-- Which one is determined by the selector placeholder.category.
				
				if element (unit_cursor).appearance = APPEARANCE_PCB then
					sch_placeholder_name := element (unit_cursor).placeholders.name;
					sch_placeholder_value := element (unit_cursor).placeholders.value;
					sch_placeholder_purpose := element (unit_cursor).placeholders.purpose;

					
					if placeholder_is_selected (device_cursor, unit_cursor) then

						-- increase brightness
						brightness := BRIGHT;

						if placeholder_move.being_moved then

							case placeholder_move.category is
								when NAME =>									
									-- Calculate the absolute position of the NAME placeholder 
									-- as it was according to database BEFORE the move:
									placeholder_move.absolute_position := sch_placeholder_name.position;
									move_by (placeholder_move.absolute_position, unit_position);

									-- Depending on the tool used, calculate the new position of the 
									-- placeholder relative to the unit position:
									case placeholder_move.tool is
										when MOUSE =>
											move_by (
												point	=> sch_placeholder_name.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, snap_to_grid (get_mouse_position)));
         
										when KEYBOARD =>
											move_by (
												point	=> sch_placeholder_name.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, get_cursor_position));
         
									end case;

									
								when PURPOSE =>
									-- Calculate the absolute position of the PURPOSE placeholder 
									-- as it was according to database BEFORE the move:
									placeholder_move.absolute_position := sch_placeholder_purpose.position;
									move_by (placeholder_move.absolute_position, unit_position);

									-- Depending on the tool used, calculate the new position of the 
									-- placeholder relative to the unit position:
									case placeholder_move.tool is
										when MOUSE =>
											move_by (
												point	=> sch_placeholder_purpose.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, snap_to_grid (get_mouse_position)));
         
										when KEYBOARD =>
											move_by (
												point	=> sch_placeholder_purpose.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, get_cursor_position));
         
									end case;

									
								when VALUE =>

									-- Calculate the absolute position of the VALUE placeholder 
									-- as it was according to database BEFORE the move:
									placeholder_move.absolute_position := sch_placeholder_value.position;
									move_by (placeholder_move.absolute_position, unit_position);

									-- Depending on the tool used, calculate the new position of the 
									-- placeholder relative to the unit position:
									case placeholder_move.tool is
										when MOUSE =>
											move_by (
												point	=> sch_placeholder_value.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, snap_to_grid (get_mouse_position)));
         
										when KEYBOARD =>
											move_by (
												point	=> sch_placeholder_value.position,
												offset	=> get_distance_relative (
													placeholder_move.absolute_position, get_cursor_position));
         
									end case;

							end case;
									
						end if;
					end if;
				end if;

				device_cursor_lib := get_device_model_cursor (device_model);
				unit_count := get_unit_count (device_cursor_lib);

				-- locate and draw the unit:
				draw_unit (locate_unit (device_cursor_lib, unit_name));
			end if;

			
			-- CASE 2: The unit being moved changes the sheet.
			-- CS: NOT TESTED !
			if object_sheet_changes then
				
				if is_selected (unit_cursor) then

					-- increase brightness
					brightness := BRIGHT;

					-- overwrite position if unit is moving
					if is_moving (unit_cursor) then
						unit_position := get_object_tool_position;
					end if;

					-- get the rotation of the unit
					unit_rotation := get_rotation (element (unit_cursor).position);

					
					-- If this is a real device, then get a copy of the 
					-- placeholders of the unit.
					-- NOTE: The position of the placeholders is relative to
					-- the unit position !
					if element (unit_cursor).appearance = APPEARANCE_PCB then
						sch_placeholder_name := element (unit_cursor).placeholders.name;
						sch_placeholder_value := element (unit_cursor).placeholders.value;
						sch_placeholder_purpose := element (unit_cursor).placeholders.purpose;		
					end if;

					device_cursor_lib := get_device_model_cursor (device_model);
					unit_count := get_unit_count (device_cursor_lib);

					-- locate and draw the unit:
					draw_unit (locate_unit (device_cursor_lib, unit_name));
				end if;
			end if;

		end query_units;


		
	begin
		--put_line ("device " & to_string (key (device_cursor)));
		
		-- Get device name, value, purpose and number of units of the current device.
		-- Procedure draw_symbol needs them later:
		if element (device_cursor).appearance = APPEARANCE_PCB then
			device_name := key (device_cursor); -- like R1, IC100
			device_value := element (device_cursor).value; -- like 100R or TL084
			device_purpose := element (device_cursor).purpose; -- like "brightness control"
		end if;
		
		-- Iterate the units of the current device:
		iterate (element (device_cursor).units, query_units'access);
		-- CS use query_element (device_cursor, query_device'access);
	end query_devices;





	-- Drawing the unit of a real device requires placeholders
	-- for name, value and purpose. We fetch them from the symbol model
	-- in this case.
	-- If the symbol is virtual, then the placeholders are meaningless
	-- and assume default values.
	procedure fetch_placeholders_ext (
		symbol_cursor : in pac_symbols.cursor)
	is 
		use et_symbol_model;
		use pac_symbols;
		sym : type_symbol renames element (symbol_cursor);
	begin
		-- CS: use function get_default_placeholders ?
		
		if is_real (sym) then
			sch_placeholder_name	:= sym.placeholders.name;
			sch_placeholder_value	:= sym.placeholders.value;
			sch_placeholder_purpose := sym.placeholders.purpose;
		else
			sch_placeholder_name	:= (meaning => NAME, others => <>);
			sch_placeholder_value	:= (meaning => VALUE, others => <>);
			sch_placeholder_purpose := (meaning => PURPOSE, others => <>);
		end if;
	end fetch_placeholders_ext;

	


	-- Drawing the symbol of a real device requires placeholders
	-- for name, value an purpose. We fetch them from the symbol model
	-- in this case.
	-- If the symbol is virtual, then the placeholders are meaningless
	-- and assume default values.
	procedure fetch_placeholders_int (
		unit_cursor : in pac_units_internal.cursor)
	is
		use pac_units_internal;
		sym : type_unit_internal renames element (unit_cursor);
	begin
		-- CS: use function get_default_placeholders ?
		
		case element (unit_cursor).appearance is
			when APPEARANCE_PCB =>
				sch_placeholder_name	:= sym.symbol.placeholders.name;
				sch_placeholder_value	:= sym.symbol.placeholders.value;
				sch_placeholder_purpose := sym.symbol.placeholders.purpose;
				
			when APPEARANCE_VIRTUAL =>
				sch_placeholder_name	:= (meaning => NAME, others => <>);
				sch_placeholder_value	:= (meaning => VALUE, others => <>);
				sch_placeholder_purpose := (meaning => PURPOSE, others => <>);
		end case;
	end fetch_placeholders_int;




	
	
	
	-- Draws the unit being added. If there is no unit being added,
	-- then nothing happens here. The unit is drawn in a preview.
	procedure draw_unit_being_added is
		brightness : type_brightness := BRIGHT;
		
		use pac_devices_lib;

		
		procedure locate_symbol (unit_cursor : in type_unit_cursors) is
			use pac_units_external;
			use pac_units_internal;

			-- The place where the unit will be drawn.
			-- Depends on the tool used for placing the unit:
			destination : type_vector_model := get_primary_tool_position;
			
			use et_symbol_model;
			use et_symbol_name;
			use pac_symbols;
			symbol_model : pac_symbol_model_file.bounded_string; -- like libraries/symbols/NAND.sym
			symbol_cursor : pac_symbols.cursor;

			
		begin
			case unit_cursor.ext_int is
				when EXT =>
					-- put_line ("external unit");
					-- put_line ("rotation " & to_string (unit_add.rotation));
					
					-- If the unit is external, we must fetch the symbol and the placeholders
					-- via its model file:
					symbol_model := element (unit_cursor.external).model;
					locate_symbol (symbol_model, symbol_cursor);

					fetch_placeholders_ext (symbol_cursor);
						
					draw_symbol (
						symbol		=> pac_symbols.element (symbol_cursor),

						device_name		=> unit_add.device_pre,
						unit_name		=> unit_add.name,
						unit_count		=> unit_add.total,
						
						unit_position	=> destination,
						unit_rotation	=> unit_add.rotation,
						
						sch_placeholder_name	=> sch_placeholder_name,
						sch_placeholder_value	=> sch_placeholder_value,
						sch_placeholder_purpose => sch_placeholder_purpose,

						brightness		=> brightness,
						preview			=> true);


				when INT =>
					--put_line ("internal unit");						
					
					-- If the unit is internal, we fetch it the symbol and the placeholders 
					-- directly from the unit:
					
					fetch_placeholders_int (unit_cursor.internal);
					
					draw_symbol (
						symbol		=> element (unit_cursor.internal).symbol,

						device_name		=> unit_add.device_pre,
						unit_name		=> unit_add.name,
						unit_count		=> unit_add.total,
						
						unit_position	=> destination,
						unit_rotation	=> unit_add.rotation,
						
						sch_placeholder_name	=> sch_placeholder_name,
						sch_placeholder_value	=> sch_placeholder_value,
						sch_placeholder_purpose => sch_placeholder_purpose,
						
						brightness		=> brightness,
						preview			=> true);
			end case;
		end locate_symbol;


	begin
		if unit_add.valid then
			locate_symbol (locate_unit (unit_add.device, unit_add.name));
		end if;
	end draw_unit_being_added;






	
	
	procedure draw_unit_being_fetched is
		brightness : type_brightness := BRIGHT;
		
		use pac_devices_lib;

		
		procedure locate_symbol (unit_cursor : in type_unit_cursors) is
			use pac_units_external;
			use pac_units_internal;

			-- The place where the unit will be drawn.
			-- Depends on the tool used for placing the unit:
			destination : type_vector_model := get_primary_tool_position;

			use et_symbol_name;
			use et_symbol_model;
			use pac_symbols;
			symbol_model : pac_symbol_model_file.bounded_string; -- like libraries/symbols/NAND.sym
			symbol_cursor : pac_symbols.cursor;
			
		begin			
			case unit_cursor.ext_int is
				when EXT =>
					--put_line ("external unit");
					
					-- If the unit is external, we must fetch the symbol and the placeholders
					-- via its model file:
					symbol_model := element (unit_cursor.external).model;
					locate_symbol (symbol_model, symbol_cursor);

					fetch_placeholders_ext (symbol_cursor);
						
					draw_symbol (
						symbol		=> pac_symbols.element (symbol_cursor),

						device_name		=> unit_fetch.device_pre,
						unit_name		=> unit_fetch.name,
						unit_count		=> unit_fetch.total,
						
						unit_position	=> destination,
						unit_rotation	=> unit_fetch.rotation,
						
						sch_placeholder_name	=> sch_placeholder_name,
						sch_placeholder_value	=> sch_placeholder_value,
						sch_placeholder_purpose => sch_placeholder_purpose,

						brightness		=> brightness,
						preview			=> true);


				when INT =>
					--put_line ("internal unit");						
					
					-- If the unit is internal, we fetch it the symbol and the placeholders 
					-- directly from the unit:
					
					fetch_placeholders_int (unit_cursor.internal);
					
					draw_symbol (
						symbol		=> element (unit_cursor.internal).symbol,

						device_name		=> unit_fetch.device_pre,
						unit_name		=> unit_fetch.name,
						unit_count		=> unit_fetch.total,
						
						unit_position	=> destination,
						unit_rotation	=> unit_fetch.rotation,
						
						sch_placeholder_name	=> sch_placeholder_name,
						sch_placeholder_value	=> sch_placeholder_value,
						sch_placeholder_purpose => sch_placeholder_purpose,
						
						brightness		=> brightness,
						preview			=> true);
			end case;
		end locate_symbol;


	begin
		if unit_fetch.valid then
			locate_symbol (locate_unit (unit_fetch.device, unit_fetch.name));
		end if;
	end draw_unit_being_fetched;


	
begin
	-- 	put_line ("draw units ...");
	
	iterate (element (active_module).devices, query_devices'access);
	-- CS use query_element (active_module, query_module'access);

	-- Draw the unit being added. If no unit is being added,
	-- then nothing happens here:
	draw_unit_being_added;

	-- Draw the unit being fetched. If no unit is being fetched,
	-- then nothing happens here:
	draw_unit_being_fetched;
	
end draw_units;



-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
