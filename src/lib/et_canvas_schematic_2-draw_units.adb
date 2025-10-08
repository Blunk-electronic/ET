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

with et_primitive_objects;				use et_primitive_objects;
with et_colors;							use et_colors;

with et_text;
with et_alignment;
with et_symbol_shapes;
with et_symbol_text;
with et_symbol_model;					use et_symbol_model;
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


	brightness : type_brightness := NORMAL;
	
	
	-- This procedure draws a single unit:
	procedure draw_unit (
		symbol			: in type_symbol;
		device_name		: in type_device_name;
		device_value	: in pac_device_value.bounded_string; -- like 100R or TL084
		device_purpose	: in pac_device_purpose.bounded_string := empty_purpose; -- like "brightness control"
		unit_name		: in pac_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...

		-- The unit count is required in order to decide whether a suffix
		-- for the unit name is to be drawn. If the device has only a single unit,
		-- then the unit name will not be drawn:
		unit_count		: in type_unit_count;

		-- CS: Unit position and rotation should be unified
		-- to a single argument ?:
		unit_position	: in type_vector_model; -- x/y on the schematic sheet
		unit_rotation	: in type_rotation := zero_rotation;
		
		placeholders	: in type_default_placeholders;
		preview			: in boolean := false)
	is
		
		use et_symbol_model;
		use et_symbol_ports;
		use pac_text_schematic;
		use pac_geometry_2;

		use et_symbol_shapes;		
		use pac_symbol_lines;
		use pac_symbol_arcs;
		use pac_symbol_circles;
		
		use pac_symbol_ports;

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



		
		-- This procedure draws the line of a port candidate.
		-- The start of the line is where a net segment is attached to.
		-- The end of the line points toward the body of the unit.
		-- Depending on the rotation of the port (as defined in the
		-- device model) the start (A) and end (B) of the line is computed here.
		procedure draw_port (c : in pac_symbol_ports.cursor) is
			port : type_symbol_port renames element (c);

			-- For the rotation of port and terminal name, the
			-- total rotation is required:
			rotation_total : constant type_rotation := add (port.rotation, unit_rotation);
			
			-- This is the start point of the line:
			A : type_vector_model := port.position;

			-- This is the end point of the line.
			-- It will be computed according to the rotation of
			-- the port (as given by the device model) and the
			-- length of the line (port.length):
			B : type_vector_model := port.position;

			-- The position of the origin of port
			-- and terminal name:
			pos_port_name		: type_vector_model;
			pos_terminal_name	: type_vector_model;


			-- Compute the following positions according to rotation and length of the port:
			-- - start and end point of the line (The end points towards the unit body.)
			-- - From start and end of the line (A/B) the preliminary positions of
			--   port name and terminal name are computed.
			--
			-- NOTE: Regarding the position of terminal and port name:
			-- For the moment, the computations below leave the rotation of the 
			-- unit in the schematic outside. For the moment we assume that the unit is not rotated
			-- in the schematic. We look at the default rotation of the ports as they
			-- are defined in the device model.
			-- The final coordinates of terminal and port name will be computed later.
			procedure compute_positions is begin
				
				if port.rotation = 0.0 then -- end point points to the left
					-- Compute the end point. It is left of the start point:
					set (axis => AXIS_X, value => get_x (A) - port.length, point => B);

					-- Compute the position of the port name. 
					-- It is some distance left of the end point (usually inside the body of the unit):
					pos_port_name := B;
					set (axis => AXIS_X, 
						 value => get_x (B) - port_name_spacing, point => pos_port_name);

					-- Compute the position of the terminal name.
					-- It is some distance left of the start point (usually outside the body of the unit):
					pos_terminal_name := A;				
					set (axis => AXIS_X, 
						 value => get_x (A) - terminal_name_spacing_start, point => pos_terminal_name);

					
				elsif port.rotation = 90.0 then -- end point points downwards
					-- Compute the end point. It is below the start point:
					set (axis => AXIS_Y, value => get_y (A) - port.length, point => B);

					-- Compute the position of the port name. 
					-- It is some distance below the end point (usually inside the body of the unit):
					pos_port_name := B;
					set (axis => AXIS_Y, 
						 value => get_y (B) - port_name_spacing, point => pos_port_name);

					-- Compute the position of the terminal name.
					-- It is some distance below the start point (usually outside the body of the unit):
					pos_terminal_name := A;				
					set (axis => AXIS_Y, 
						 value => get_y (A) - terminal_name_spacing_start, point => pos_terminal_name);

					
				elsif port.rotation = 180.0 then  -- end point points to the left
					-- Compute the end point. It is right of the start point:
					set (axis => AXIS_X, value => get_x (A) + port.length, point => B);

					-- Compute the position of the port name. 
					-- It is some distance right of the end point (usually inside the body of the unit):
					pos_port_name := B;
					set (axis => AXIS_X, 
						 value => get_x (B) + port_name_spacing, point => pos_port_name);

					-- Compute the position of the terminal name.
					-- It is some distance right of the start point (usually outside the body of the unit):
					pos_terminal_name := A;				
					set (axis => AXIS_X, 
						 value => get_x (A) + terminal_name_spacing_start, point => pos_terminal_name);

					
				elsif port.rotation = 270.0 or port.rotation = -90.0 then -- end point points upwards
					-- Compute the end point. It is above the start point:
					set (axis => AXIS_Y, value => get_y (A) + port.length, point => B);

					-- Compute the position of the port name. 
					-- It is some distance above the end point (usually inside the body of the unit):
					pos_port_name := B;
					set (axis => AXIS_Y, 
						 value => get_y (B) + port_name_spacing, point => pos_port_name);

					-- Compute the position of the terminal name.
					-- It is some distance above the start point (usually outside the body of the unit):
					pos_terminal_name := A;
					set (axis => AXIS_Y, 
						 value => get_y (A) + terminal_name_spacing_start, point => pos_terminal_name);

					
				else
					raise constraint_error; -- CS do something helpful. should never happen
				end if;
			end compute_positions;

			

			-- Draws the line and the circle of the port:
			procedure draw_line_and_circle is 
				-- The line that represents the port:
				line : type_line;

				-- The circle at the start of the line where net
				-- segments are attached to:
				circle : type_circle;
			begin

				-- Set start and and points of the line:
				set_A (line, A);
				set_B (line, B);

				-- Draw the line:
				set_color_symbols (brightness);
				draw_line (line, (unit_position, unit_rotation), port_line_width,
					do_stroke => true);

				-- Draw the circle around the start point
				-- of the line if the port-layer is enabled:
				if ports_enabled then
					-- put_line ("draw port");
					
					-- The start point of the port must have a small green circle around it.
					-- set color and line width
					set_color_ports (brightness);

					-- Set center and radius of the circle:
					set_center (circle, get_A (line));
					set_radius (circle, port_circle_radius);

					-- Draw the circle. It is not filled:
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
			end draw_line_and_circle;

			
			
			-- This procedure draws the port name at its final
			-- position taking into account the rotation of the unit in the schematic:
			procedure draw_port_name is
				use et_port_names;
				use et_text;

				-- The vertical alignment is untouched and is always CENTER.
				-- The horizontal alignment depends on the total rotation
				-- which is a sum of port rotation and unit rotation.
				use et_alignment;
				alignment : type_text_alignment := (horizontal => ALIGN_CENTER, vertical => ALIGN_CENTER);

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
					content		=> to_content (to_string (get_port_name (c))),
					size		=> port.port_name_size,
					font		=> et_symbol_text.text_font,
					anchor		=> pos_port_name,
					origin		=> false,  -- no origin required

					-- Text rotation about its anchor point.
					-- This is documentational text. Its rotation must
					-- be snapped to either HORIZONAL or VERTICAL so that
					-- it is readable from the front or the right.
					rotation	=> to_rotation (to_rotation_doc (rotation_total)),
					alignment	=> alignment);

			end draw_port_name;

			

			-- This procedure draws the terminal name at its final
			-- position taking into account the rotation of the unit in the schematic:
			procedure draw_terminal_name is
				use et_text;
				-- The vertical alignment is untouched and is always BOTTOM.
				-- The horizontal alignment depends on the total rotation
				-- which is a sum of port rotation and unit rotation.
				use et_alignment;
				alignment : type_text_alignment := (horizontal => ALIGN_CENTER, vertical => ALIGN_BOTTOM);

				use et_terminals;
				properties : type_port_properties_access;

				use pac_draw_text;
				use et_schematic_ops.units;
			begin
				-- Rotate the position of the terminal name by the unit rotation:
				rotate_by (pos_terminal_name, unit_rotation);
				
				-- Move the name by the unit position:
				move_by (pos_terminal_name, unit_position);

				-- Now some fine adjustment is required to place the terminal
				-- name some distance away from the line of the port.
				-- Compute the position of the origin of the terminal name regarding 
				-- its distance from the line of the port:
				
				if rotation_total = 0.0 or rotation_total = 360.0 or rotation_total = -360.0 then
					-- The line is horizontal. So the terminal name must be 
					-- moved up above the line by some distance:
					move (pos_terminal_name, DIR_UP, terminal_name_spacing_line);
					alignment.horizontal := ALIGN_RIGHT;

				elsif rotation_total = 90.0 or rotation_total = -270.0 then
					-- The line is vertical. So the terminal name must be 
					-- moved left of the line by some distance:
					move (pos_terminal_name, DIR_LEFT, terminal_name_spacing_line);
					alignment.horizontal := ALIGN_RIGHT;
					
				elsif rotation_total = 180.0 or rotation_total = -180.0 then
					-- The line is horizontal. So the terminal name must be 
					-- moved up above the line by some distance:
					move (pos_terminal_name, DIR_UP, terminal_name_spacing_line);
					alignment.horizontal := ALIGN_LEFT;
					
				elsif rotation_total = -90.0 or rotation_total = 270.0 then
					-- The line is vertical. So the terminal name must be 
					-- moved left of the line by some distance:
					move (pos_terminal_name, DIR_LEFT, terminal_name_spacing_line);
					alignment.horizontal := ALIGN_LEFT;
					
				else
					raise constraint_error; -- CS should never happen
				end if;

				
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
					size		=> port.terminal_name_size,
					font		=> et_symbol_text.text_font,
					anchor		=> pos_terminal_name,
					origin		=> false,  -- no origin required

					-- Text rotation about its anchor point.
					-- This is documentational text. Its rotation must
					-- be snapped to either HORIZONAL or VERTICAL so that
					-- it is readable from the front or the right.
					rotation	=> to_rotation (to_rotation_doc (rotation_total)),
					alignment	=> alignment);

			end draw_terminal_name;


			use et_port_visibility;

			
		begin
			-- Compute preliminary positions of terminal and port names.
			-- Compute start and end points of the line:
			compute_positions;
	
			draw_line_and_circle;
				
			-- Draw port name if it is set to visible in the device model:
			if port.port_name_visible = YES then
				draw_port_name;
			end if;

			-- If this is a preview, then no terminal name is to be drawn.
			-- Otherwise, draw terminal name:
			-- Draw the terminal name if this is the symbol of a real device. 
			-- Virtual symbols do not have terminals and thus no terminal names.
			if not preview then
				if is_real (symbol) and then port.terminal_name_visible = YES then
					draw_terminal_name;
				end if;
			end if;			
		end draw_port;

		
		
		
		-- This procedure draws fixed documentational texts 
		-- like "MUX" or "CT16" as they 
		-- are frequently placed inside symbols.
		-- Call this procedure after drawing the symbol body because it
		-- does not change the color to symbol color.
		procedure draw_text (c : in pac_symbol_texts.cursor) is 
			text : type_symbol_text renames element (c);
			p : type_vector_model := text.position;

			use pac_draw_text;
		begin
			-- Rotate the position of the text.
			-- This adds the unit_rotation to the given rotation.
			rotate_by (p, unit_rotation);

			-- Move text by unit position
			move_by (p, unit_position);
			
			draw_text (
				content		=> text.content,
				size		=> text.size,
				font		=> et_symbol_text.text_font,
				anchor		=> p,
				origin		=> false, -- no origin required
				
				-- Text rotation about its anchor point.
				-- This is documentational text. Its rotation must
				-- be snapped to either HORIZONAL or VERTICAL so that
				-- it is readable from the front or the right.
				rotation	=> to_rotation (
								to_rotation_doc (text.rotation + unit_rotation)),

				alignment	=> text.alignment);
		end draw_text;



		
		
		-- This procedure draws text placeholders 
		-- for device name, value and purpose:
		procedure draw_placeholders is 
			use et_text;
			use pac_draw_text;


			-- Draws the device name placeholder filled
			-- by the actual device name like IC20.A:
			procedure draw_name is 
				-- This is the place where the placeholder
				-- will be anchored. If the placeholder is being
				-- moved by the operator, then the tool position
				-- determines the anchor point. Otherwise the position
				-- as defined by the caller of draw_unit will be used:
				p : type_vector_model;

				-- This flag is required in order to restore
				-- the previous brightness in case the placeholder is
				-- to be drawn highlighted:
				restore_normal_brightness : boolean := false;				
			begin
				--put_line (to_string (device_name) & " " 
				-- & to_string (unit_name) 
				-- & " " & to_string (unit_count));

				-- The placeholder will be drawn only if the corresponding
				-- layer is enabled:
				if device_names_enabled then

					-- 1. If the whole unit is to be highlighted, then
					--    the placeholder will also be drawn highlighted.
					-- 2. If the unit is not selected but only the placeholder,
					--    then only the placeholder will be highlighted.
					--    The normal brightness must be restored once the
					--    placeholder has been drawn:
					if brightness = NORMAL then 
						if is_selected (placeholders.name) then
							set_color_placeholders (BRIGHT);
							restore_normal_brightness := true;
						end if;
					end if;

					
					-- If the placeholder is being moved, then the
					-- tool position determines where the placeholder is drawn.
					-- Otherwise the placeholder is drawn as given by the schematic:
					if is_moving (placeholders.name) then
						p := get_object_tool_position;
					else
						-- The placeholder position is relative to the unit position:
						p := placeholders.name.position;
						
						-- Move placeholder by unit position to the final position:
						move_by (p, unit_position);
					end if;

					
					-- Draw the full text like IC20.A
					draw_text (
						content		=> to_content (get_full_name (device_name, unit_name, unit_count)), -- IC4.PWR
						size		=> symbol.placeholders.name.size,
						font		=> name_font,
						anchor		=> p,
						origin		=> true, -- origin required
						rotation	=> to_rotation (placeholders.name.rotation),
						alignment	=> placeholders.name.alignment);


					-- Restore the previous brightness if the placeholder
					-- has been drawn highlighted:
					if restore_normal_brightness then
						set_color_placeholders (NORMAL);
					end if;
				end if;
			end draw_name;

			
			
			
			-- Draws the device value placeholder filled
			-- by the actual device value like 100k:
			procedure draw_value is 
				-- This is the place where the placeholder
				-- will be anchored. If the placeholder is being
				-- moved by the operator, then the tool position
				-- determines the anchor point. Otherwise the position
				-- as defined by the caller of draw_unit will be used:
				p : type_vector_model;

				-- This flag is required in order to restore
				-- the previous brightness in case the placeholder is
				-- to be drawn highlighted:
				restore_normal_brightness : boolean := false;				
			begin
				-- The placeholder will be drawn only if the corresponding
				-- layer is enabled:
				if device_values_enabled then
					
					-- The value may be empty. We do not draw it in this case:
					if not is_empty (device_value) then

						-- 1. If the whole unit is to be highlighted, then
						--    the placeholder will also be drawn highlighted.
						-- 2. If the unit is not selected but only the placeholder,
						--    then only the placeholder will be highlighted.
						--    The normal brightness must be restored once the
						--    placeholder has been drawn:
						if brightness = NORMAL then 
							if is_selected (placeholders.value) then
								set_color_placeholders (BRIGHT);
								restore_normal_brightness := true;
							end if;
						end if;

						
						-- If the placeholder is being moved, then the
						-- tool position determines where the placeholder is drawn.
						-- Otherwise the placeholder is drawn as given by the schematic:						
						if is_moving (placeholders.value) then
							p := get_object_tool_position;
						else
							-- The placeholder position is relative to the unit position:
							p := placeholders.value.position;
							
							-- Move placeholder by unit position to the final position:
							move_by (p, unit_position);
						end if;

						
						-- Draw the full text like 100k:
						draw_text (
							content		=> to_content (to_string (device_value)), -- 100k
							size		=> symbol.placeholders.value.size,
							font		=> value_font,
							anchor		=> p,
							origin		=> true, -- origin required
							rotation	=> to_rotation (placeholders.value.rotation),
							alignment	=> placeholders.value.alignment);

						
						-- Restore the previous brightness if the placeholder
						-- has been drawn highlighted:
						if restore_normal_brightness then
							set_color_placeholders (NORMAL);
						end if;						
					end if;
				end if;
			end draw_value;


			

			-- Draws the device value placeholder filled
			-- by the actual device value like "Brightness Control":
			procedure draw_purpose is 
				-- This is the place where the placeholder
				-- will be anchored. If the placeholder is being
				-- moved by the operator, then the tool position
				-- determines the anchor point. Otherwise the position
				-- as defined by the caller of draw_unit will be used:
				p : type_vector_model;

				-- This flag is required in order to restore
				-- the previous brightness in case the placeholder is
				-- to be drawn highlighted:
				restore_normal_brightness : boolean := false;
			begin
				-- The placeholder will be drawn only if the corresponding
				-- layer is enabled:
				if device_purposes_enabled then
				
					-- The purpose may be empty. We do not draw it in this case:
					if not is_empty (device_purpose) then

						-- 1. If the whole unit is to be highlighted, then
						--    the placeholder will also be drawn highlighted.
						-- 2. If the unit is not selected but only the placeholder,
						--    then only the placeholder will be highlighted.
						--    The normal brightness must be restored once the
						--    placeholder has been drawn:
						if brightness = NORMAL then 
							if is_selected (placeholders.purpose) then
								set_color_placeholders (BRIGHT);
								restore_normal_brightness := true;
							end if;
						end if;


						-- If the placeholder is being moved, then the
						-- tool position determines where the placeholder is drawn.
						-- Otherwise the placeholder is drawn as given by the schematic:
						if is_moving (placeholders.purpose) then
							p := get_object_tool_position;
						else
							-- The placeholder position is relative to the unit position:
							p := placeholders.purpose.position;
							
							-- Move placeholder by unit position to the final position:
							move_by (p, unit_position);
						end if;

						
						-- Draw the fill text like "Brightness Control":
						draw_text (
							content		=> to_content (to_string (device_purpose)), -- "brightness control"
							size		=> symbol.placeholders.purpose.size,
							font		=> purpose_font,
							anchor		=> p,
							origin		=> true, -- origin required
							rotation	=> to_rotation (placeholders.purpose.rotation),
							alignment	=> placeholders.purpose.alignment);


						-- Restore the previous brightness if the placeholder
						-- has been drawn highlighted:
						if restore_normal_brightness then
							set_color_placeholders (NORMAL);
						end if;
					end if;
				end if;
			end draw_purpose;
		
			
		begin
			-- Set the brightness for all placeholders.
			-- If the global brightness is NORMAL, then highlighting
			-- of selected placeholders will be handled individually:
			set_color_placeholders (brightness);

			draw_name;
			draw_value;
			draw_purpose;			
		end draw_placeholders;

		
		
		procedure draw_origin is begin
			-- NOTE: This is about the origin of the symbol !
			-- set_color_origin (brightness);
			
			-- NOTE: The origin is never rotated.
			draw_origin ((unit_position, 0.0));

		end draw_origin;


		
		
	begin
		
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
		if is_real (symbol) then
			draw_placeholders;
		end if;

		draw_origin;

	end draw_unit;






	

	
	
	procedure query_module (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module)
	is
		use pac_devices_sch;
		device_cursor : pac_devices_sch.cursor := module.devices.first; 

		
		-- This procedure draws the units of the candidate device:
		procedure query_device (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is
			use pac_units;
			unit_cursor : pac_units.cursor := device.units.first;
			
			-- Get the device model of the candidate device:
			device_cursor_lib : constant pac_devices_lib.cursor := get_device_model (device);
			
			-- The number of units provided by the current device:
			unit_count : constant type_unit_count := get_unit_count (device_cursor_lib);


			-- The name, value and purpose of the candidate device if it is real:
			device_value : pac_device_value.bounded_string; -- like 100R or TL084
			device_purpose : pac_device_purpose.bounded_string; -- like "brightness control"


			
			-- This procedure queries a unit:
			procedure query_unit (
				unit_name	: in pac_unit_name.bounded_string;
				unit		: in type_unit) 
			is
				-- Get the position of the unit candidate as it is
				-- in the schematic:
				pos : constant type_object_position := get_position (unit);

				unit_position : type_vector_model; -- only x and y relevant
				unit_rotation : type_rotation;
			

				-- This procedure maps from the given unit cursor
				-- to the actual symbol and draws the symbol:
				procedure draw_unit (
					unit_cursor : in type_unit_cursors)				
				is begin
					draw_unit (						
						symbol			=> get_symbol (unit_cursor),

						device_name		=> device_name,
						device_value	=> device_value,
						device_purpose	=> device_purpose,

						unit_name		=> unit_name,
						unit_count		=> unit_count,
						
						unit_position	=> unit_position,
						unit_rotation	=> unit_rotation,

						-- Get the placeholders (for name, value, purpose)
						-- of the unit as defined in the schematic.
						-- (If the unit is virtual, then default
						-- placeholders are returned and later ignored when the
						-- unit is drawn):
						placeholders 	=> get_placeholders (unit));
				end draw_unit;


				
				procedure draw_on_active_sheet is begin
					-- CASE 1: We draw units which are on the active sheet:
					if get_sheet (pos) = active_sheet then
						
						-- The default brightness is NORMAL. If the unit is selected, 
						-- then the brightness will be increased:
						brightness := NORMAL;

						if is_selected (unit) then
							-- put_line ("selected");
							
							-- The whole unit will be drawn highlighted:
							brightness := BRIGHT;

							-- overwrite position if unit is moving
							if is_moving (unit) then
								unit_position := get_object_tool_position;
							end if;
						end if;

						-- get the rotation of the unit
						unit_rotation := get_rotation (pos);

						-- locate and draw the unit:
						draw_unit (locate_unit (device_cursor_lib, unit_name));
					end if;
				end draw_on_active_sheet;


				
				procedure draw_if_sheet_changes is begin
					-- CASE 2: The unit being moved changes the sheet.
					-- CS: NOT TESTED !
					if object_sheet_changes then
						
						if is_selected (unit_cursor) then

							-- The whole unit will be drawn highlighted:
							brightness := BRIGHT;

							-- overwrite position if unit is moving
							if is_moving (unit) then
								unit_position := get_object_tool_position;
							end if;

							-- get the rotation of the unit
							unit_rotation := get_rotation (unit);
							
							-- locate and draw the unit:
							draw_unit (locate_unit (device_cursor_lib, unit_name));
						end if;
					end if;
				end draw_if_sheet_changes;
				
				
			begin
				-- put_line ("unit " & to_string (unit_name));

				-- Get the x/y-position of the unit.
				-- If the unit is selected and being moved, then the x/y position
				-- will be overwritten by the position of the mouse or the cursor.
				unit_position := get_place (pos);

				-- There are two cases when a unit is to be drawn:
				
				-- 1. The unit is on the current active sheet.
				draw_on_active_sheet;
				
				-- 2. The unit is being moved from one sheet to another sheet.
				-- CS: draw_if_sheet_changes;
			end query_unit;


		begin
			-- put_line ("device " & to_string (device_name));

			-- If the candidate device is real, then fetch its 
			-- value and purpose:
			if is_real (device) then
				device_value := device.value; -- like 100R or TL084
				device_purpose := device.purpose; -- like "brightness control"
			end if;

			-- Iterate through the units of the candidate device:
			while has_element (unit_cursor) loop
				query_element (unit_cursor, query_unit'access);
				next (unit_cursor);
			end loop;
		end query_device;


	begin
		-- Iterate through the devices of the module:
		while has_element (device_cursor) loop
			query_element (device_cursor, query_device'access);
			next (device_cursor);
		end loop;
	end query_module;


	



	
	
	
	-- Draws the unit being added. If there is no unit being added,
	-- then nothing happens here. The unit is drawn in a preview.
	procedure draw_unit_being_added is
		
		procedure query_unit (unit_cursor : in type_unit_cursors) is

			-- The place where the unit will be drawn.
			-- Depends on the tool used for placing the unit:
			destination : type_vector_model := get_primary_tool_position;
			
			symbol_cursor : pac_symbols.cursor;
		begin
			-- The whole unit will be drawn highlighted:
			brightness := BRIGHT;
			
			case unit_cursor.ext_int is
				when EXT =>
					-- put_line ("external unit");
					-- put_line ("rotation " & to_string (unit_add.rotation));
					
					symbol_cursor := get_symbol (unit_cursor.external);
					
					draw_unit (
						symbol			=> get_symbol (symbol_cursor),
						device_name		=> unit_add.device_pre,
						device_value	=> unit_add.value,
						unit_name		=> unit_add.name,
						unit_count		=> unit_add.total,						
						unit_position	=> destination,
						unit_rotation	=> unit_add.rotation,						
						placeholders	=> rotate_placeholders (
							get_placeholders (symbol_cursor), unit_add.rotation),

						preview			=> true);


				when INT =>
					--put_line ("internal unit");						
					
					draw_unit (
						symbol			=> get_symbol (unit_cursor.internal),
						device_name		=> unit_add.device_pre,
						device_value	=> unit_add.value,
						unit_name		=> unit_add.name,
						unit_count		=> unit_add.total,						
						unit_position	=> destination,
						unit_rotation	=> unit_add.rotation,						
						placeholders	=> rotate_placeholders (
							get_placeholders (unit_cursor.internal), unit_add.rotation),

						preview			=> true);
			end case;
		end query_unit;


	begin
		if unit_add.valid then
			query_unit (locate_unit (unit_add.device, unit_add.name));
		end if;
	end draw_unit_being_added;






	
	
	procedure draw_unit_being_fetched is
		
		procedure query_unit (unit_cursor : in type_unit_cursors) is

			-- The place where the unit will be drawn.
			-- Depends on the tool used for placing the unit:
			destination : type_vector_model := get_primary_tool_position;

			symbol_cursor : pac_symbols.cursor;			
		begin	
			-- The whole unit will be drawn highlighted:
			brightness := BRIGHT;
			
			case unit_cursor.ext_int is
				when EXT =>
					--put_line ("external unit");
					
					symbol_cursor := get_symbol (unit_cursor.external);

					draw_unit (
						symbol			=> get_symbol (symbol_cursor),
						device_name		=> unit_fetch.device_pre,
						device_value	=> unit_fetch.value,
						unit_name		=> unit_fetch.name,
						unit_count		=> unit_fetch.total,						
						unit_position	=> destination,
						unit_rotation	=> unit_fetch.rotation,						
						placeholders	=> rotate_placeholders (
							get_placeholders (symbol_cursor), unit_fetch.rotation),
						
						preview			=> true);


				when INT =>
					--put_line ("internal unit");						
					
					draw_unit (
						symbol			=> get_symbol (unit_cursor.internal),
						device_name		=> unit_fetch.device_pre,
						device_value	=> unit_fetch.value,
						unit_name		=> unit_fetch.name,
						unit_count		=> unit_fetch.total,						
						unit_position	=> destination,
						unit_rotation	=> unit_fetch.rotation,						
						placeholders	=> rotate_placeholders (
							get_placeholders (unit_cursor.internal), unit_fetch.rotation),

						preview			=> true);
			end case;
		end query_unit;


	begin
		if unit_fetch.valid then
			query_unit (locate_unit (unit_fetch.device, unit_fetch.name));
		end if;
	end draw_unit_being_fetched;

	

	
begin
	-- put_line ("draw units ...");
	query_element (active_module, query_module'access);

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
