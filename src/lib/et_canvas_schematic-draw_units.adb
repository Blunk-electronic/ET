------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              DRAW UNITS                                  --
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
with ada.numerics;				use ada.numerics;
with ada.containers;
with cairo;						use cairo;
with pango.layout;				use pango.layout;

with et_general;				use et_general;
with et_project;				use et_project;
with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

with et_schematic;

use et_project.type_modules;
use et_schematic.type_devices;
use et_schematic.type_units;

with et_devices;
with et_symbols;
with et_text;

separate (et_canvas_schematic)

procedure draw_units (
	self	: not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	-- The name, value and purpose of the current device:
	device_name	: et_devices.type_name; -- like R1, IC100	
	device_value : et_devices.type_value.bounded_string; -- like 100R or TL084
	device_purpose : et_devices.type_purpose.bounded_string; -- like "brightness control"

	-- The number of units provided by the current device:
	unit_count	: et_devices.type_unit_count; -- the total number of units

	-- The name of the current unit:
	unit_name	: et_devices.type_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...

	-- The placeholders as given by the schematic:
	sch_placeholder_name	: et_symbols.type_text_placeholder (meaning => et_symbols.NAME);
	sch_placeholder_value	: et_symbols.type_text_placeholder (meaning => et_symbols.VALUE);
	sch_placeholder_purpose : et_symbols.type_text_placeholder (meaning => et_symbols.PURPOSE);

	unit_rotation : type_rotation;
	
	-- This function returns true if the given placeholder has been moved from the
	-- default position and rotation or if the alignment has been changed:
	function moved_by_operator (placeholder : in et_symbols.type_text_placeholder)
		return boolean is
		use et_symbols;
		use type et_text.type_text_alignment;
		result : boolean := false;
	begin
		case placeholder.meaning is
			when NAME =>
				if  placeholder.position /= sch_placeholder_name.position or
					placeholder.rotation /= sch_placeholder_name.rotation or
					placeholder.alignment /= sch_placeholder_name.alignment then
					result := true;
				end if;

			when VALUE =>
				if 	placeholder.position /= sch_placeholder_value.position or
					placeholder.rotation /= sch_placeholder_value.rotation or
					placeholder.alignment /= sch_placeholder_value.alignment then
					result := true;
				end if;
			
			when PURPOSE =>
				if 	placeholder.position /= sch_placeholder_purpose.position or
					placeholder.rotation /= sch_placeholder_purpose.rotation or
					placeholder.alignment /= sch_placeholder_purpose.alignment then
					result := true;
				end if;
		end case;

		return result;
	end moved_by_operator;
	
	procedure draw_symbol (
		symbol		: in et_symbols.type_symbol;
		position	: in et_coordinates.geometry.type_point) -- x/y on the schematic sheet
	is

	-- CS Could be useful to use the primitive draw operations of et_canvas_draw:
	-- In order to draw objects of a symbol we instantiate this package:
	-- 	package pac_draw_symbol is new et_canvas_draw.pac_draw (
	-- 		pac_canvas	=> pac_canvas,
	-- 		pac_shapes	=> et_symbols.pac_shapes);

		use et_symbols;
		use type_lines;
		use type_arcs;
		use type_circles;
		use type_ports;
		use type_texts;

		-- First we take a copy the boundaries of the symbol.
		boundaries : geometry.type_boundaries := symbol.boundaries;

		-- In the next steps the boundaries are to be extended.
		-- Reason: The operator may have changed positions of
		-- placeholders (for name, value and purpose) from their initial
		-- position as specified in the symbol model. So the boundaries
		-- may have become wider.
		-- Other things like lines, arcs, ports and texts can't be moved separately in the 
		-- schematic editor. They already have been included in the bounding box.
		-- See procedure et_symbols.compute_boundaries for details.

		-- This is the bounding box required for drawing the symbol. The bounding
		-- box exists in the model only.
		bounding_box : type_rectangle;
		
		procedure make_bounding_box is begin
			-- In case the symbol belongs to a real device, probe placeholders and
			-- update boundaries. If a placeholder is inside the boundaries,
			-- nothing happens -> The boundaries are NOT changed.
			if symbol.appearance = PCB then
				-- CS: Currently the area occupied by the text content is ignored.
				pac_shapes.union (boundaries, symbol.name.position);
				pac_shapes.union (boundaries, symbol.value.position);
				pac_shapes.union (boundaries, symbol.purpose.position);
			end if;

			bounding_box := (
				-- The bounding box origin is the upper left corner.
				-- The box position in x is shifted by the smallest_x to the left.
				-- The box position in y is shifted by the greatest_y (upwards).
				-- The box position in y is additonally converted to y axis going downwards.
				x		=> position.x 
							- abs (boundaries.smallest_x),
				
				y		=> convert_and_shift_y (self,
							  position.y
							+ abs (boundaries.greatest_y)), -- convert y to "downwards"

				-- The box width is the difference between greatest x and smallest x.
				-- The box height is the difference between greatest y and smallest y.
				width	=> boundaries.greatest_x - boundaries.smallest_x,
				height	=> boundaries.greatest_y - boundaries.smallest_y
				);
			
		end make_bounding_box;

		-- Transposes the x-value from the drawing to the view.
		function transpose_x (x : in type_distance) return type_view_coordinate is begin
			--return convert_x (x - boundaries.smallest_x);
			return convert_x (x);
		end;

		-- Transposes the y-value from the drawing to the view.
		function transpose_y (y : in type_distance) return type_view_coordinate is begin
			--return convert_y (abs (y - boundaries.greatest_y));
			return convert_y (- y);
		end;
		
		procedure draw_line (c : in type_lines.cursor) is 
			type type_line is new pac_shapes.type_line with null record;
			line : type_line := (pac_shapes.type_line (element (c)) with null record);
		begin
			rotate (line, unit_rotation);
			--put_line ("width " & to_string (element (c).width));
			
			-- set line width
			cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));
			
			-- start point
			cairo.move_to (
				context.cr,
				transpose_x (x (line.start_point)),
				transpose_y (y (line.start_point))
				);

			-- end point
			cairo.line_to (
				context.cr,
				transpose_x (x (line.end_point)),
				transpose_y (y (line.end_point))
				);

			cairo.stroke (context.cr);
		end draw_line;

		procedure draw_arc (c : in type_arcs.cursor) is 
			use et_symbols.pac_shapes;
			arc : type_arc_angles := to_arc_angles (element (c));
		begin
			cairo.new_sub_path (context.cr); -- required to suppress an initial line

			-- set line width
			cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));
			
			if arc.direction = CCW then

				cairo.arc (
					context.cr,
					xc		=> transpose_x (arc.center.x),
					yc		=> transpose_y (arc.center.y),
					radius	=> type_view_coordinate (arc.radius),
					angle1	=> type_view_coordinate (to_radians (arc.angle_start)),
					angle2	=> type_view_coordinate (to_radians (arc.angle_end))
					);

			else
				
				cairo.arc_negative (
					context.cr,
					xc		=> transpose_x (arc.center.x),
					yc		=> transpose_y (arc.center.y),
					radius	=> type_view_coordinate (arc.radius),
					angle1	=> type_view_coordinate (to_radians (arc.angle_start)),
					angle2	=> type_view_coordinate (to_radians (arc.angle_end))
					);
			end if;

			cairo.stroke (context.cr);
		end draw_arc;

		procedure draw_circle (c : in type_circles.cursor) is begin
			
			-- set line width
			cairo.set_line_width (context.cr, type_view_coordinate (element (c).width));

			cairo.new_sub_path (context.cr); -- required to suppress an initial line

			cairo.arc (
				cr		=> context.cr,
				xc		=> transpose_x (element (c).center.x),
				yc		=> transpose_y (element (c).center.y),
				radius	=> type_view_coordinate (element (c).radius),

				-- it must be a full circle starting at 0 degree and ending at 360 degree:
				angle1	=> 0.0,
				angle2	=> type_view_coordinate (2 * pi)
				);
			
			cairo.stroke (context.cr);
		end draw_circle;

		procedure draw_port (c : in type_ports.cursor) is
			start_point : type_point := element (c).position;
			end_point : type_point := element (c).position;
		begin
			-- A port is basically a line. Its start point is the port position.
			-- The end point points towards the symbol body. Depending on the port
			-- rotation the end tail points:
			--  to the left if rotation is 0 degree
			--  to the right if rotation is 180 degree
			--  downwards if the rottion is 90 degree
			--  upwards if the rotation is 270 degree

			-- set line width
			cairo.set_line_width (context.cr, type_view_coordinate (et_symbols.port_line_width));

			-- set color
			cairo.set_source_rgb (context.cr, gdouble (1), gdouble (1), gdouble (1)); -- white
			
			-- We start drawing at the port position:
			rotate (start_point, unit_rotation);
			
			cairo.move_to (
				context.cr,
				transpose_x (x (start_point)),
				transpose_y (y (start_point))
				);

			-- set the end point
			if element (c).rotation = 0.0 then -- end point points to the left
				set (axis => X, value => - (element (c).length), point => end_point);
				
			elsif element (c).rotation = 90.0 then -- end point points downwards
				set (axis => Y, value => - (element (c).length), point => end_point);

			elsif element (c).rotation = 180.0 then  -- end point points to the right
				set (axis => X, value => element (c).length, point => end_point);
				
			elsif element (c).rotation = 270.0 then -- end point points upwards
				set (axis => Y, value => element (c).length, point => end_point);
				
			else
				raise constraint_error; -- CS do something helpful
			end if;

			-- draw the end point
			rotate (end_point, unit_rotation);
			
			cairo.line_to (
				context.cr,
				transpose_x (end_point.x),
				transpose_y (end_point.y)
				);

			cairo.stroke (context.cr);


			-- The start point of the port must have a small green circle around it.

			-- set color and line width
			cairo.set_source_rgb (context.cr, gdouble (0), gdouble (1), gdouble (0)); -- green
			cairo.set_line_width (context.cr, type_view_coordinate (port_circle_line_width));

			cairo.new_sub_path (context.cr); -- required to suppress an initial line
			cairo.arc (
				cr		=> context.cr,
				xc		=> transpose_x (x (start_point)),
				yc		=> transpose_y (y (start_point)),
				radius	=> type_view_coordinate (port_circle_radius),

				-- it must be a full circle starting at 0 degree and ending at 360 degree:
				angle1	=> 0.0,
				angle2	=> type_view_coordinate (2 * pi)
				);

			cairo.stroke (context.cr);
			
			-- CS draw terminal and port name, direction, sensitivity, level

		end draw_port;

		-- This procedure draws fixed texts like "MUX" or "CT16" as they 
		-- are frequently placed inside symbols:
		procedure draw_text (c : in type_texts.cursor) is 
			position : type_point := element (c).position;
		begin
			rotate (position, unit_rotation);
			
			pac_draw_misc.draw_text 
				(
				context		=> context,
				text		=> pac_text.type_text (element (c)),
				content		=> element (c).content,
				size		=> element (c).size,
				x			=> transpose_x (x (position)),
				y			=> transpose_y (y (position)),
				rotation	=> element (c).rotation,
				alignment	=> element (c).alignment
				);
		end draw_text;

		-- This procedure draws text placeholders for device name, value and purpose:
		procedure draw_placeholders is 
			use et_text;
			use et_devices;
			position : type_point;
		begin

			-- If operator has moved or rotate the placeholder in the schematic,
			-- then the position and rotation of the placeholder in the schematic
			-- is used. Otherwise the default position and rotation as given in
			-- the symbol is used.
			
			-- DEVICE NAME:
			if moved_by_operator (symbol.name) then

				position := sch_placeholder_name.position;
				rotate (position, unit_rotation);
				
				pac_draw_misc.draw_text (
					context		=> context,
					text		=> pac_text.type_text (symbol.name),
					content		=> to_content (to_full_name (device_name, unit_name, unit_count)), -- IC4.PWR
					size		=> symbol.name.size,
					x			=> transpose_x (x (position)),
					y			=> transpose_y (y (position)),
					rotation	=> sch_placeholder_name.rotation,
					alignment	=> sch_placeholder_name.alignment);

			else -- use defaults from library symbol

				position := symbol.name.position;
				rotate (position, unit_rotation);
				
				pac_draw_misc.draw_text (
					context		=> context,
					text		=> pac_text.type_text (symbol.name),
					content		=> to_content (to_full_name (device_name, unit_name, unit_count)),
					size		=> symbol.name.size,
					x			=> transpose_x (x (position)),
					y			=> transpose_y (y (position)),
					rotation	=> symbol.name.rotation,
					alignment	=> symbol.name.alignment);

			end if;
			
			-- VALUE
			-- The value may be empty. We do not draw it in this case:
			if not is_empty (device_value) then
				if moved_by_operator (symbol.value) then

					position := sch_placeholder_value.position;
					rotate (position, unit_rotation);
					
					pac_draw_misc.draw_text (
						context		=> context,
						text		=> pac_text.type_text (symbol.value),
						content		=> to_content (to_string (device_value)), -- 100R
						size		=> symbol.value.size,
						x			=> transpose_x (x (sch_placeholder_value.position)),
						y			=> transpose_y (y (sch_placeholder_value.position)),
						rotation	=> sch_placeholder_value.rotation,
						alignment	=> sch_placeholder_value.alignment);
					
				else -- use defaults from library symbol

					position := symbol.value.position;
					rotate (position, unit_rotation);
					
					pac_draw_misc.draw_text (
						context		=> context,
						text		=> pac_text.type_text (symbol.value),
						content		=> to_content (to_string (device_value)),
						size		=> symbol.value.size,
						x			=> transpose_x (x (position)),
						y			=> transpose_y (y (position)),
						rotation	=> symbol.value.rotation,
						alignment	=> symbol.value.alignment);

				end if;
			end if;
			
			-- PURPOSE
			-- The purpose may be empty. We do not draw it in this case:
			if not is_empty (device_purpose) then

				if moved_by_operator (symbol.purpose) then

					pac_draw_misc.draw_text (
						context		=> context,
						text		=> pac_text.type_text (symbol.purpose),
						content		=> to_content (to_string (device_purpose)), -- "brightness control"
						size		=> symbol.purpose.size,
						x			=> transpose_x (x (sch_placeholder_purpose.position)),
						y			=> transpose_y (y (sch_placeholder_purpose.position)),
						rotation	=> sch_placeholder_purpose.rotation,
						alignment	=> sch_placeholder_purpose.alignment);

				else -- use defaults from library symbol
					pac_draw_misc.draw_text (
						context		=> context,
						text		=> pac_text.type_text (symbol.purpose),
						content		=> to_content (to_string (device_purpose)),
						size		=> symbol.purpose.size,
						x			=> transpose_x (x (symbol.purpose.position)),
						y			=> transpose_y (y (symbol.purpose.position)),
						rotation	=> symbol.purpose.rotation,
						alignment	=> symbol.purpose.alignment);

				end if;
			end if;
			
		end draw_placeholders;

		procedure draw_origin is begin
			cairo.set_source_rgb (context.cr, gdouble (1), gdouble (1), gdouble (1)); -- white
			cairo.set_line_width (context.cr, type_view_coordinate (origin_line_width));

			-- horizontal line from left to right
			cairo.move_to (
				context.cr,
				transpose_x (- origin_half_size),
				transpose_y (zero)
				);

			cairo.line_to (
				context.cr,
				transpose_x (origin_half_size),
				transpose_y (zero)
				);

			-- vertical line downwards
			cairo.move_to (
				context.cr,
				transpose_x (zero),
				transpose_y (origin_half_size)
				);

			cairo.line_to (
				context.cr,
				transpose_x (zero),
				transpose_y (- origin_half_size)
				);

			cairo.stroke (context.cr);
		end draw_origin;

		-- Build the center point of the symbol:
		center : constant type_point := type_point (set (
			x	=> self.drawing.frame_bounding_box.x + x (position),
			y	=> et_coordinates.type_distance (self.drawing.frame.size.y) - y (position) + self.drawing.frame_bounding_box.y));
		
	begin -- draw_symbol
		-- The unit might have been rotated. So the boundaries must be computed anew:
		if unit_rotation /= zero_rotation then
-- 			put_line (to_string (boundaries));
-- 			put_line ("unit rotated by" & to_string (unit_rotation));
			rotate (boundaries, unit_rotation);
-- 			put_line (to_string (boundaries));
		end if;
		
		make_bounding_box;

-- 		put_line ("bounding box position in model" & to_string (bounding_box.x) & to_string (bounding_box.y));

-- 		put_line ("unit position in drawing " & to_string (position.x) & to_string (position.y));

		-- We draw the symbol if:
		--  - no area given or
		--  - if the bounding box of the symbol intersects the given area		
		if (in_area = no_rectangle
			or else intersects (in_area, bounding_box)) 
		then
			save (context.cr);
			
			-- Prepare the current transformation matrix (CTM) so that
			-- all following drawing is relative to the upper left corner
			-- of the symbol bounding box (in the non-rotated state).
			-- Further-on the drawing must be offset by the position
			-- of the frame_bounding_box:
-- 			translate (
-- 				context.cr,
-- 				convert_x (self.drawing.frame_bounding_box.x 
-- 						+ bounding_box.x 
-- 
-- 						-- compensate the rotatation of the bounding box:
-- 						+ x (boundaries.distance_of_topleft_to_default)),
-- 				
-- 				convert_y (self.drawing.frame_bounding_box.y 
-- 						+ bounding_box.y 
-- 
-- 						 -- compensates the rotatation of the bounding box:   
-- 						+ y (boundaries.distance_of_topleft_to_default))
-- 					  );

-- 			put_line (to_string (center));

			-- Prepare the current transformation matrix (CTM) so that
			-- all following drawing is relative to the center of the symbol:
			translate (
				context.cr,
				convert_x (x (center)),	
				convert_y (y (center)));

			
			-- SYMBOL BODY
			-- set color
			cairo.set_source_rgb (context.cr, gdouble (1), gdouble (1), gdouble (1)); -- white

			iterate (symbol.shapes.lines, draw_line'access);
			iterate (symbol.shapes.arcs, draw_arc'access);
			iterate (symbol.shapes.circles, draw_circle'access);

			
			-- SYMBOL PORTS
			iterate (symbol.ports, draw_port'access); -- has internal color settings


			-- SYMBOL TEXTS
			cairo.set_source_rgb (context.cr, gdouble (1), gdouble (1), gdouble (1)); -- white
			iterate (symbol.texts, draw_text'access);
			
			-- Draw placeholders if this is the symbol of a real device. 
			-- Virtual symbols do not have placeholders.
			if symbol.appearance = PCB then
				draw_placeholders;
			end if;

			-- draw origin
			draw_origin;
			

			restore (context.cr);
			
		end if;

	end draw_symbol;

	procedure query_devices (device_cursor : in et_schematic.type_devices.cursor) is
		
		-- get the model of the current device
		device_model : et_devices.type_device_model_file.bounded_string :=
			element (device_cursor).model;	-- ../libraries/devices/transistor/pnp.dev

		unit_position : et_coordinates.geometry.type_point; -- only x and y relevant
		
		procedure locate_symbol (unit_cursor : in et_devices.type_unit_cursors) is
			use et_devices;
			use pac_units_external;
			use pac_units_internal;

			use et_symbols;
			symbol_model : type_symbol_model_file.bounded_string; -- like libraries/symbols/NAND.sym
			symbol_cursor : et_symbols.type_symbols.cursor;
		begin
			case unit_cursor.ext_int is
				when EXT =>
					--put_line ("external unit");
					-- If the unit is external, we must fetch the symbol 
					-- via its model file:
					symbol_model := element (unit_cursor.external).file;
					symbol_cursor := locate (symbol_model);
					draw_symbol (
						symbol		=> type_symbols.element (symbol_cursor),
						position	=> unit_position);
					
				when INT =>
					--put_line ("internal unit");						
					-- If the unit is internal, we can fetch it the symbol 
					-- directly from the unit:
					draw_symbol (
						symbol		=> element (unit_cursor.internal).symbol,
						position	=> unit_position);
			end case;
		end locate_symbol;
		
		procedure query_units (unit_cursor : in et_schematic.type_units.cursor) is
			use et_devices;
			use et_symbols;
			device_cursor_lib : type_devices.cursor;
		begin
			-- we want to draw only those units which are on the active sheet:
			if element (unit_cursor).position.sheet = self.drawing.sheet then
				unit_name := key (unit_cursor);
				unit_position := type_point (element (unit_cursor).position);
				unit_rotation := rot (element (unit_cursor).position);
				--put_line (to_string (unit_name));

				-- Get a copy of the placeholders of the unit:
				if element (unit_cursor).appearance = PCB then
					sch_placeholder_name := element (unit_cursor).name;
					sch_placeholder_value := element (unit_cursor).value;
					sch_placeholder_purpose := element (unit_cursor).purpose;
				end if;
				
				device_cursor_lib := locate_device (device_model);
				locate_symbol (locate_unit (device_cursor_lib, unit_name));
			end if;
		end query_units;

		use et_symbols;
		
	begin -- query_devices

		-- Get device name, value, purpose and number of units of the current device.
		-- Procedure draw_symbol needs them later:
		if element (device_cursor).appearance = PCB then
			device_name := key (device_cursor); -- like R1, IC100
			device_value := element (device_cursor).value; -- like 100R or TL084
			device_purpose := element (device_cursor).purpose; -- like "brightness control"
		end if;
		
		unit_count := et_devices.type_unit_count (length (element (device_cursor).units));

		-- Iterate the units of the current device:
		iterate (element (device_cursor).units, query_units'access);
	end query_devices;
		
begin
	-- 	put_line ("draw units ...");
	
	iterate (element (self.drawing.module).devices, query_devices'access);
	
end draw_units;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
