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
with cairo;						use cairo;
with pango.layout;				use pango.layout;

with et_general;				use et_general;
with et_project;				use et_project;
with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

with et_schematic;				--use et_schematic;

use et_project.type_modules;
use et_schematic.type_devices;
use et_schematic.type_units;

with et_devices;
with et_symbols;

separate (et_canvas.schematic)

procedure draw_units (
	model	: not null access type_model;
	in_area	: in type_model_rectangle := no_rectangle;
	context : in type_draw_context) is

	procedure draw_symbol (
		symbol		: in et_symbols.type_symbol;
		position	: in et_coordinates.geometry.type_point) -- x/y on the schematic sheet
	is
		use et_symbols;
		use type_lines;

		smallest_x, smallest_y : type_distance := type_distance'last; 
		greatest_x, greatest_y : type_distance := type_distance'first;

		procedure update_boundaries (p : et_coordinates.geometry.type_point) is begin
			if p.x < smallest_x then smallest_x := p.x; end if;
			if p.x > greatest_x then greatest_x := p.x; end if;
			if p.y < smallest_y then smallest_y := p.y; end if;
			if p.y > greatest_y then greatest_y := p.y; end if;
		end;
		
		bounding_box : type_model_rectangle;
		
		-- The bounding box that surrounds the symbol must be calculated.
		function make_bounding_box return type_model_rectangle is 
			
			procedure query_line (c : in type_lines.cursor) is begin
				update_boundaries (element (c).start_point);
				update_boundaries (element (c).end_point);
			end query_line;
				
		begin -- make_bounding_box
			iterate (symbol.shapes.lines, query_line'access);
			-- CS arcs, circles, text, placeholders, ports

			put_line ("smallest_x " & to_string (smallest_x));
			put_line ("greatest_x " & to_string (greatest_x));
			put_line ("smallest_y " & to_string (smallest_y));
			put_line ("greatest_y " & to_string (greatest_y));
			
			return (
				-- The bounding box origin is the upper left corner.
				-- The box position in x is shifted by the smallest_x to the left.
				-- The box position in y is shifted by the greatest_y upwards.
				-- The box position in y is additonally converted to y axis going downwards.
				x		=> type_model_coordinate (position.x - abs (smallest_x)),
				y		=> convert_y (position.y + abs (greatest_y)), -- convert y to "downwards"
				width	=> type_model_coordinate (greatest_x - smallest_x),
				height	=> type_model_coordinate (greatest_y - smallest_y)
				);
			
		end make_bounding_box;

		procedure draw_line (c : in type_lines.cursor) is begin
			-- start point
			cairo.move_to (
				context.cr,
				convert_x (element (c).start_point.x - smallest_x),
				type_view_coordinate (abs (element (c).start_point.y - greatest_y))
				);

			-- end point
			cairo.line_to (
				context.cr,
				convert_x (element (c).end_point.x - smallest_x),
				type_view_coordinate (abs (element (c).end_point.y - greatest_y))
				);

		end draw_line;

			
	begin -- draw_symbol
		bounding_box := make_bounding_box;

		put_line ("bounding box position in model" & 
				  to_string (bounding_box.x) & to_string (bounding_box.y));

		put_line ("unit position in drawing " & 
				  to_string (position.x) & to_string (position.y));

		
		if (in_area = no_rectangle
			or else intersects (in_area, bounding_box)) 
		then
			save (context.cr);

			-- Prepare the current transformation matrix (CTM) so that
			-- all following drawing is relative to the upper left corner
			-- of the symbol bounding box:
			translate (
				context.cr,
				type_view_coordinate (model.frame_bounding_box.x + bounding_box.x),
				type_view_coordinate (model.frame_bounding_box.y + bounding_box.y));

			cairo.set_line_width (context.cr, 1.0);

			cairo.set_source_rgb (context.cr, gdouble (1), gdouble (1), gdouble (1)); -- white

			-- draw lines
			iterate (symbol.shapes.lines, draw_line'access);
			-- CS arcs, circles, text, placeholders, ports
			

			cairo.stroke (context.cr);
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
			unit_name : type_unit_name.bounded_string; -- like "I/O Bank 3" or "PWR" or "A" or "B" ...
			device_cursor_lib : type_devices.cursor;
		begin
			-- we want to draw only those units which are on the active sheet:
			if element (unit_cursor).position.sheet = model.sheet then
				unit_name := key (unit_cursor);
				unit_position := type_point (element (unit_cursor).position);
				--put_line (to_string (unit_name));
				
				device_cursor_lib := locate_device (device_model);
				locate_symbol (locate_unit (device_cursor_lib, unit_name));
			end if;
		end query_units;

	begin
		-- put_line (et_devices.to_string (key (device_cursor)));			
		iterate (element (device_cursor).units, query_units'access);
	end query_devices;
		
begin
-- 	put_line ("draw units ...");

	iterate (element (model.module).devices, query_devices'access);
	
end draw_units;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
