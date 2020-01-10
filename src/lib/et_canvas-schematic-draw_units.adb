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

		procedure draw_symbol (symbol : in et_symbols.type_symbol) is
		begin
			null;
		end draw_symbol;
	
		procedure query_devices (device_cursor : in et_schematic.type_devices.cursor) is

			-- get the model of the current device
			device_model : et_devices.type_device_model_file.bounded_string :=
				element (device_cursor).model;	-- ../libraries/devices/transistor/pnp.dev

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
						draw_symbol (type_symbols.element (symbol_cursor));
						
					when INT =>
						--put_line ("internal unit");						
						-- If the unit is internal, we can fetch it the symbol 
						-- directly from the unit:
						draw_symbol (element (unit_cursor.internal).symbol);
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
